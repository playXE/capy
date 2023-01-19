#![allow(non_snake_case, dead_code)]
//! Port implementation based on Ypsilon Scheme.
//!
//! # TODO
//!
//! Make code more Rusty.

use std::{
    borrow::Cow,
    ffi::{CStr, CString},
    mem::MaybeUninit,
    ptr::{copy_nonoverlapping, null_mut, write_bytes},
};

use rsgc::{
    prelude::{Allocation, Handle, Object},
    sync::mutex::Mutex,
    system::string::Str,
};

use crate::prelude::{ByteVector, Context, Procedure, Value, Vector};

pub const PORT_LOOKAHEAD_SIZE: usize = 8;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum PortOperation {
    Open,
    Read,
    Write,
    Close,
    Seek,
    Stat,
    Select,
    Encode,
    Decode,
}
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum SocketOperation {
    Open,
    Write,
    Read,
    Accept,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum Operation {
    Port(PortOperation),
    Socket(SocketOperation),
}

pub struct Port {
    lock: Mutex<()>,
    handlers: Value,
    bytes: Value,
    lookahead: [u8; PORT_LOOKAHEAD_SIZE],
    buf: *mut u8,
    buf_head: *mut u8,
    buf_tail: *mut u8,
    name: Value,
    transcoder: Value,
    mark: i64,
    fd: i32,
    lookahead_size: usize,
    buf_size: usize,
    buf_state: i32,
    line: i32,
    column: i32,
    codec: u8,
    eol_style: u8,
    error_handling_mode: u8,
    file_options: u8,
    buffer_mode: u8,
    typ: u8,
    subtype: u8,
    direction: u8,
    force_sync: bool,
    bom_le: bool,
    bom_be: bool,
    track_line_column: bool,
    opened: bool,
}

impl Object for Port {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.handlers.trace(visitor);
        self.bytes.trace(visitor);
        self.name.trace(visitor);
        self.transcoder.trace(visitor);
    }
}

impl Allocation for Port {}

impl Port {
    pub const TYPE_NAMED_FILE: u8 = 1;
    pub const TYPE_BYTEVECTOR: u8 = 2;
    pub const TYPE_CUSTOM: u8 = 3;
    pub const TYPE_SOCKET: u8 = 4;

    pub const SUBTYPE_NONE: u8 = 0;
    pub const SUBTYPE_CHAR_SPECIAL: u8 = 1;
    pub const SUBTYPE_FIFO: u8 = 2;

    pub const DIRECTION_IN: u8 = 0x01;
    pub const DIRECTION_OUT: u8 = 0x02;
    pub const DIRECTION_IO: u8 = 0x03;

    pub const BUFFER_MODE_NONE: u8 = 1;
    pub const BUFFER_MODE_LINE: u8 = 2;
    pub const BUFFER_MODE_BLOCK: u8 = 3;

    pub const FILE_OPTION_NONE: u8 = 0;
    pub const FILE_OPTION_NO_CREATE: u8 = 0x01;
    pub const FILE_OPTION_NO_FAIL: u8 = 0x02;
    pub const FILE_OPTION_NO_TRUNCATE: u8 = 0x04;

    pub const BUFFER_CODEC_LATIN1: u8 = 1;
    pub const BUFFER_CODEC_UTF8: u8 = 2;
    pub const BUFFER_CODEC_UTF16: u8 = 3;

    pub const EOL_STYLE_NONE: u8 = 1;
    pub const EOL_STYLE_LF: u8 = 2;
    pub const EOL_STYLE_CR: u8 = 3;
    pub const EOL_STYLE_CRLF: u8 = 4;
    pub const EOL_STYLE_NEL: u8 = 5;
    pub const EOL_STYLE_CRNEL: u8 = 6;
    pub const EOL_STYLE_LS: u8 = 7;

    pub const ERROR_HANDLING_MODE_IGNORE: u8 = 1;
    pub const ERROR_HANDLING_MODE_RAISE: u8 = 2;
    pub const ERROR_HANDLING_MODE_REPLACE: u8 = 3;

    pub const BLOCK_BUFFER_SIZE: usize = 4096;
    pub const LINE_BUFFER_SIZE: usize = 4096;
    pub const CUSTOM_BUFFER_SIZE: usize = Self::BLOCK_BUFFER_SIZE;

    pub const BUF_STATE_UNSPECIFIED: i32 = 0;
    pub const BUF_STATE_READ: i32 = 1;
    pub const BUF_STATE_WRITE: i32 = 2;
    pub const BUF_STATE_ACCUMULATE: i32 = 3;

    pub const UCS4_BOM: u32 = 0x0feff;
    pub const UCS4_REPLACEMENT_CHAR: u32 = 0x0fffd;
    pub const UCS4_LF: u32 = 0x0000a;
    pub const UCS4_CR: u32 = 0x0000d;
    pub const UCS4_NEL: u32 = 0x00085;
    pub const UCS4_LS: u32 = 0x02028;

    pub const BYRE_REPLACEMENT_CHAR: u8 = b'?';
    pub const BYTEVECTOR_OUTPUT_CHUNK: usize = 128;

    pub const CODEC_NATIVE: u8 = Self::BUFFER_CODEC_UTF8;
    pub const EOL_STYLE_NATIVE: u8 = Self::EOL_STYLE_LF;
}

pub const PORT_HANDLER_TEXTUAL: i32 = 0;
pub const PORT_HANDLER_READ: i32 = 1;
pub const PORT_HANDLER_WRITE: i32 = 2;
pub const PORT_HANDLER_GET_POS: i32 = 3;
pub const PORT_HANDLER_SET_POS: i32 = 4;
pub const PORT_HANDLER_CLOSE: i32 = 5;

pub fn cstr<'a>(s: &'a str) -> Cow<'a, CStr> {
    match CStr::from_bytes_until_nul(s.as_bytes()) {
        Ok(cstr) => Cow::Borrowed(cstr),
        Err(_) => Cow::Owned(CString::new(s).unwrap()),
    }
}

fn io_open(path: &str, oflag: i32, pmode: i32) -> i32 {
    unsafe { libc::open(cstr(path).as_ptr(), oflag, pmode) }
}
fn io_close(fd: i32) -> i32 {
    unsafe { libc::close(fd) }
}

fn io_read(fd: i32, buf: &mut [u8]) -> isize {
    unsafe { libc::read(fd, buf.as_mut_ptr().cast(), buf.len()) }
}

fn io_write(fd: i32, buf: &[u8]) -> isize {
    unsafe { libc::write(fd, buf.as_ptr().cast(), buf.len()) }
}

fn io_pread(fd: i32, buf: &mut [u8], offset: i64) -> isize {
    unsafe { libc::pread(fd, buf.as_mut_ptr().cast(), buf.len(), offset) }
}

fn io_pwrite(fd: i32, buf: &[u8], offset: i64) -> isize {
    unsafe { libc::pwrite(fd, buf.as_ptr().cast(), buf.len(), offset) }
}

fn io_lseek64(fd: i32, offset: i64, whence: i32) -> i64 {
    unsafe { libc::lseek(fd, offset, whence) }
}

fn io_mkstemp(template: &mut [u8]) -> i32 {
    unsafe { libc::mkstemp(template.as_mut_ptr().cast()) }
}
use libc::*;

use super::encoding::{cnvt_ucs4_to_utf8, cnvt_utf8_to_ucs4, utf8_byte_count};
#[inline]
pub fn S_ISBLK(m: mode_t) -> bool {
    ((m) & S_IFMT) == S_IFBLK /* block special */
}

#[inline]
pub fn S_ISCHR(m: mode_t) -> bool {
    ((m) & S_IFMT) == S_IFCHR /* char special */
}

#[inline]
pub fn S_ISDIR(m: mode_t) -> bool {
    ((m) & S_IFMT) == S_IFDIR /* directory */
}

#[inline]
pub fn S_ISFIFO(m: mode_t) -> bool {
    ((m) & S_IFMT) == S_IFIFO /* fifo or socket */
}

#[inline]
pub fn S_ISREG(m: mode_t) -> bool {
    ((m) & S_IFMT) == S_IFREG /* regular file */
}

#[inline]
pub fn S_ISLNK(m: mode_t) -> bool {
    ((m) & S_IFMT) == S_IFLNK /* symbolic link */
}

#[inline]
pub fn S_ISSOCK(m: mode_t) -> bool {
    ((m) & S_IFMT) == S_IFSOCK /* socket */
}

fn io_stat_mode(path: &str) -> u8 {
    unsafe {
        let mut st: libc::stat = MaybeUninit::<libc::stat>::zeroed().assume_init();
        if libc::stat(cstr(path).as_ptr(), &mut st) == 0 {
            if S_ISCHR(st.st_mode) {
                return Port::SUBTYPE_CHAR_SPECIAL;
            }

            if S_ISFIFO(st.st_mode) {
                return Port::SUBTYPE_FIFO;
            }
        }
        return Port::SUBTYPE_NONE;
    }
}

fn io_fstat_mode(fd: i32) -> u8 {
    unsafe {
        let mut st: libc::stat = MaybeUninit::<libc::stat>::zeroed().assume_init();
        if libc::fstat(fd, &mut st) == 0 {
            if S_ISCHR(st.st_mode) {
                return Port::SUBTYPE_CHAR_SPECIAL;
            }

            if S_ISFIFO(st.st_mode) {
                return Port::SUBTYPE_FIFO;
            }
        }
        return Port::SUBTYPE_NONE;
    }
}

fn io_fstat_size(fd: i32, size: &mut i64) -> i32 {
    unsafe {
        let mut st: libc::stat = MaybeUninit::<libc::stat>::zeroed().assume_init();
        if fstat(fd, &mut st) == 0 {
            *size = st.st_size;
            return 0;
        }
        -1
    }
}

fn no_input_buffered(port: Handle<Port>) -> bool {
    (port.lookahead_size == 0)
        && (port.buf.is_null()
            || port.buf_head == port.buf_tail
            || port.buf_state != Port::BUF_STATE_READ)
}

fn no_output_buffered(port: Handle<Port>) -> bool {
    port.buf_state != Port::BUF_STATE_WRITE || port.buf.is_null() || port.buf_head == port.buf_tail
}

#[derive(Debug)]
pub enum IoError {
    Code(Operation, i32),
    Message(Operation, String),
    Value(Value),
}

fn device_read(
    ctx: &mut Context,
    port: Handle<Port>,
    p: &mut [u8],
    mark: i64,
) -> Result<isize, IoError> {
    if port.typ == Port::TYPE_NAMED_FILE {
        let mut n;

        loop {
            match port.subtype {
                Port::SUBTYPE_NONE => {
                    n = io_pread(port.fd, p, mark);
                }
                Port::SUBTYPE_FIFO | Port::SUBTYPE_CHAR_SPECIAL => {
                    n = io_read(port.fd, p);
                }
                _ => unreachable!(),
            }

            if n < 0 {
                if errno::errno().0 == libc::EINTR {
                    continue;
                }

                return Err(IoError::Code(
                    Operation::Port(PortOperation::Read),
                    errno::errno().0,
                ));
            }

            break;
        }

        return Ok(n);
    }

    if port.typ == Port::TYPE_SOCKET {
        todo!()
    }

    assert_eq!(port.typ, Port::TYPE_CUSTOM);
    assert!(p.len() <= Port::CUSTOM_BUFFER_SIZE);

    let vect = port.handlers.get_handle_of::<Vector>();
    let bv = port.bytes.get_handle_of::<ByteVector>();



    let result = ctx
        .apply(vect.elements[PORT_HANDLER_READ as usize], &[Value::new(bv), Value::new(0), Value::new(p.len() as i32)])
        .map_err(IoError::Value)?;
    unsafe {
        copy_nonoverlapping(bv.as_ptr(), p.as_mut_ptr(), p.len());
    }

    if result.is_int32() {
        return Ok(result.get_int32() as isize);
    }

    return Err(IoError::Message(
        Operation::Port(PortOperation::Read),
        "custom port read! procedure return invalid value".to_owned(),
    ));
}

#[allow(unused_variables)]
fn device_write(
    ctx: &mut Context,
    port: Handle<Port>,
    mut p: &[u8],
    mark: i64,
) -> Result<(), IoError> {
    if port.typ == Port::TYPE_NAMED_FILE {
        let mut rest = p.len();
        let mut offset = mark;
        let mut written;
        while rest > 0 {
            match port.subtype {
                Port::SUBTYPE_NONE => {
                    written = io_pwrite(port.fd, &p[p.len() - rest..], offset);
                }

                _ => {
                    written = io_write(port.fd, &p[p.len() - rest..]);
                }
            }

            if written < 0 {
                if errno::errno().0 == libc::EINTR {
                    continue;
                }

                return Err(IoError::Code(
                    Operation::Port(PortOperation::Write),
                    errno::errno().0,
                ));
            }

            p = &p[written as usize..];
            rest -= written as usize;
            offset += written as i64;
        }

        return Ok(());
    }

    if port.typ == Port::TYPE_SOCKET {
        todo!()
    }

    assert_eq!(port.typ, Port::TYPE_CUSTOM);
    assert!(p.len() <= Port::CUSTOM_BUFFER_SIZE);

    let vect = port.handlers.get_handle_of::<Vector>();
    let mut bv = port.bytes.get_handle_of::<ByteVector>();

    let mut rest = p.len();
    let mut offset = mark;

    while rest > 0 {
        let written;

        unsafe {
            copy_nonoverlapping(p.as_ptr(), bv.as_mut_ptr(), p.len());
        }

        written = ctx
            .apply(vect.elements[PORT_HANDLER_WRITE as usize], &[Value::new(bv), Value::new(0), Value::new(rest as i32)])
            .map_err(IoError::Value)?
            .get_int32();
        p = &p[written as usize..];
        rest -= written as usize;
        offset += written as i64;
    }

    Ok(())
}

fn device_set_mark(ctx: &mut Context, mut port: Handle<Port>, offset: i64) -> Result<(), IoError> {
    if port.typ == Port::TYPE_NAMED_FILE {
        match port.subtype {
            Port::SUBTYPE_NONE => {
                if io_lseek64(port.fd, offset, libc::SEEK_SET) < 0 {
                    return Err(IoError::Code(
                        Operation::Port(PortOperation::Seek),
                        errno::errno().0,
                    ));
                }
            }

            _ => {
                assert!(false);
            }
        }
    } else if port.typ == Port::TYPE_CUSTOM {
        let vect = port.handlers.get_handle_of::<Vector>();
        let off = Value::new(offset as i32);

        ctx.apply(vect.elements[PORT_HANDLER_SET_POS], &[off])
            .map_err(IoError::Value)?;
    } else {
        unreachable!()
    }

    port.mark = offset as _;
    port.buf_head = port.buf;
    port.buf_tail = port.buf;
    port.buf_state = Port::BUF_STATE_UNSPECIFIED;
    Ok(())
}

fn device_close(ctx: &mut Context, mut port: Handle<Port>) -> Result<(), IoError> {
    if port.typ == Port::TYPE_NAMED_FILE {
        if port.fd != -1 {
            loop {
                if io_close(port.fd) < 0 {
                    if errno::errno().0 == libc::EINTR {
                        continue;
                    }
                    return Err(IoError::Code(
                        Operation::Port(PortOperation::Close),
                        errno::errno().0,
                    ));
                }

                break;
            }
        }

        port.fd = -1;
        return Ok(());
    }

    if port.typ == Port::TYPE_CUSTOM {
        let vect = port.handlers.get_handle_of::<Vector>();

        if vect.elements[PORT_HANDLER_CLOSE].is_handle_of::<Procedure>() {
            ctx.apply(vect.elements[PORT_HANDLER_CLOSE], &[])
                .map_err(IoError::Value)?;
        }
    }
    Ok(())
}

fn init_port_buffer(mut port: Handle<Port>) {
    match port.buffer_mode {
        Port::BUFFER_MODE_LINE => unsafe {
            port.buf = libc::malloc(Port::LINE_BUFFER_SIZE) as *mut u8;
            port.buf_tail = port.buf;
            port.buf_head = port.buf;
            port.buf_size = Port::LINE_BUFFER_SIZE;
        },

        Port::BUFFER_MODE_BLOCK => unsafe {
            port.buf = libc::malloc(Port::BLOCK_BUFFER_SIZE) as *mut u8;
            port.buf_tail = port.buf;
            port.buf_head = port.buf;
            port.buf_size = Port::BLOCK_BUFFER_SIZE;
        },
        _ => {
            port.buf_state = Port::BUF_STATE_UNSPECIFIED;
        }
    }
}

fn init_port_transcode(mut port: Handle<Port>) {
    let transcoder = port.transcoder;

    if transcoder.is_bytes() {
        let bvector = transcoder.get_bytes();

        port.codec = bvector[0];
        port.eol_style = bvector[1];
        port.error_handling_mode = bvector[2];

        if port.codec == Port::CODEC_NATIVE
            && port.eol_style == Port::EOL_STYLE_NATIVE
            && port.error_handling_mode == Port::ERROR_HANDLING_MODE_REPLACE
        {
            port.transcoder = Value::new(true);
        }
    } else {
        port.codec = Port::CODEC_NATIVE;
        port.eol_style = Port::EOL_STYLE_NATIVE;
        port.error_handling_mode = Port::ERROR_HANDLING_MODE_REPLACE;
    }
}

fn init_port_tracking(mut port: Handle<Port>) {
    port.mark = 0;
    port.line = 1;
    port.column = 1;
    port.track_line_column = !port.transcoder.is_false();
    port.lookahead_size = 0;
    port.bom_le = false;
    port.bom_be = false;
}

pub fn port_open_std(
    mut port: Handle<Port>,
    fd: i32,
    name: Value,
    direction: u8,
    file_options: u8,
    buffer_mode: u8,
    transcoder: Value,
) {
    assert!(name.is_string());

    port.fd = fd;
    port.opened = true;
    port.typ = Port::TYPE_NAMED_FILE;
    port.subtype = io_fstat_mode(fd);
    port.handlers = Value::new(false);
    port.bytes = Value::new(false);
    port.name = name;
    port.direction = direction;
    port.transcoder = transcoder;
    port.buffer_mode = buffer_mode;
    port.file_options = file_options;
    port.force_sync = false;

    init_port_tracking(port);
    init_port_transcode(port);
    init_port_buffer(port);
}

pub fn std_port_position(fd: i32) -> Result<i64, IoError> {
    if io_fstat_mode(fd) == Port::SUBTYPE_NONE {
        let pos = io_lseek64(fd, 0, libc::SEEK_CUR);
        if pos < 0 {
            return Err(IoError::Code(
                Operation::Port(PortOperation::Seek),
                errno::errno().0,
            ));
        }
        Ok(pos)
    } else {
        Ok(0)
    }
}

pub fn port_sync_port_position(port: Handle<Port>) -> Result<(), IoError> {
    if port_has_port_position_pred(port) {
        if port.typ == Port::TYPE_NAMED_FILE && port.subtype == Port::SUBTYPE_NONE {
            if io_lseek64(port.fd, 0, libc::SEEK_CUR) >= 0 {
                return Ok(());
            }

            return Err(IoError::Code(
                Operation::Port(PortOperation::Seek),
                errno::errno().0,
            ));
        }
    }
    Ok(())
}

pub fn port_has_port_position_pred(port: Handle<Port>) -> bool {
    match port.typ {
        Port::TYPE_BYTEVECTOR => true,
        Port::TYPE_SOCKET => false,

        Port::TYPE_CUSTOM => {
            let vect = port.handlers.get_vector();
            vect.elements[PORT_HANDLER_GET_POS].is_handle_of::<Procedure>()
        }

        Port::TYPE_NAMED_FILE => {
            if port.subtype == Port::SUBTYPE_NONE {
                true
            } else {
                false
            }
        }

        _ => unreachable!(),
    }
}

pub fn port_has_set_port_position_pred(port: Handle<Port>) -> bool {
    match port.typ {
        Port::TYPE_BYTEVECTOR => true,
        Port::TYPE_SOCKET => false,

        Port::TYPE_CUSTOM => {
            let vect = port.handlers.get_vector();
            vect.elements[PORT_HANDLER_SET_POS].is_handle_of::<Procedure>()
        }

        Port::TYPE_NAMED_FILE => {
            if port.subtype == Port::SUBTYPE_NONE {
                true
            } else {
                false
            }
        }

        _ => unreachable!(),
    }
}

pub fn port_open_file(
    mut port: Handle<Port>,
    name: Value,
    direction: u8,
    file_options: u8,
    buffer_mode: u8,
    transcoder: Value,
) -> Result<(), IoError> {
    let path = name.get_string();
    port.fd = -1;
    port.opened = false;
    port.typ = Port::TYPE_NAMED_FILE;
    port.subtype = io_stat_mode(&path);
    port.handlers = Value::new(false);
    port.bytes = Value::new(false);
    port.direction = direction;
    port.transcoder = transcoder;
    port.buffer_mode = buffer_mode;
    port.file_options = file_options;
    port.force_sync = false;

    let mut options;

    match port.subtype {
        Port::SUBTYPE_NONE => {
            match port.direction {
                Port::DIRECTION_IN => {
                    options = O_RDONLY;
                }
                Port::DIRECTION_OUT => {
                    options = O_WRONLY | O_CREAT | O_TRUNC | O_EXCL;
                }

                Port::DIRECTION_IO => {
                    options = O_RDWR | O_CREAT | O_TRUNC | O_EXCL;
                }

                _ => unreachable!(),
            }

            if (file_options & Port::FILE_OPTION_NO_CREATE) != 0 {
                options &= !(O_CREAT);
            }

            if (file_options & Port::FILE_OPTION_NO_FAIL) != 0 {
                options &= !(O_EXCL);
            }

            if (file_options & Port::FILE_OPTION_NO_TRUNCATE) != 0 {
                options &= !(O_TRUNC);
            }
        }

        _ => match port.direction {
            Port::DIRECTION_IN => {
                options = O_RDONLY;
            }
            Port::DIRECTION_OUT => {
                options = O_WRONLY;
            }

            Port::DIRECTION_IO => {
                options = O_RDWR;
            }

            _ => unreachable!(),
        },
    }

    port.fd = io_open(&path, options, (S_IRUSR | S_IWUSR) as i32);
    if port.fd == -1 {
        return Err(IoError::Code(
            Operation::Port(PortOperation::Open),
            errno::errno().0,
        ));
    }

    init_port_tracking(port);
    init_port_transcode(port);
    init_port_tracking(port);
    port.opened = true;
    Ok(())
}

pub fn port_open_temp_file(
    mut port: Handle<Port>,
    name: Value,
    buffer_mode: u8,
    transcoder: Value,
) -> Result<(), IoError> {
    port.fd = -1;
    port.opened = false;
    port.typ = Port::TYPE_NAMED_FILE;
    port.subtype = Port::SUBTYPE_NONE;
    port.handlers = Value::new(false);
    port.bytes = Value::new(false);
    port.name = name;
    port.direction = Port::DIRECTION_IN;
    port.transcoder = transcoder;
    port.buffer_mode = buffer_mode;
    port.file_options = Port::FILE_OPTION_NONE;
    port.force_sync = false;

    let mut tmpl: [u8; 256] = [0; 256];
    unsafe {
        core::ptr::copy_nonoverlapping(
            "/tmp/scm_temp_XXXXXX".as_ptr(),
            tmpl.as_mut_ptr(),
            "/tmp/scm_temp_XXXXXX".len(),
        );
    }

    port.fd = io_mkstemp(&mut tmpl);

    if port.fd == -1 {
        return Err(IoError::Code(
            Operation::Port(PortOperation::Open),
            errno::errno().0,
        ));
    }

    init_port_tracking(port);
    init_port_transcode(port);
    init_port_buffer(port);
    port.opened = true;

    Ok(())
}

pub fn port_open_bytevector(
    mut port: Handle<Port>,
    name: Value,
    direction: u8,
    bytes: Value,
    transcoder: Value,
) -> Result<(), IoError> {
    port.fd = -1;
    port.opened = false;
    port.typ = Port::TYPE_BYTEVECTOR;
    port.subtype = Port::SUBTYPE_NONE;
    port.handlers = Value::new(false);

    if (port.direction & Port::DIRECTION_OUT) != 0 {
        port.bytes = Value::new(false);
    } else {
        port.bytes = bytes;
    }

    port.name = name;
    port.direction = direction;
    port.transcoder = transcoder;
    port.buffer_mode = Port::BUFFER_MODE_NONE;
    port.file_options = Port::FILE_OPTION_NONE;
    port.force_sync = false;
    init_port_tracking(port);
    init_port_transcode(port);

    port.buf = null_mut();
    port.buf_tail = null_mut();
    port.buf_tail = null_mut();
    port.buf_size = 0;
    port.buf_state = if (port.direction & Port::DIRECTION_OUT) != 0 {
        Port::BUF_STATE_ACCUMULATE
    } else {
        Port::BUF_STATE_UNSPECIFIED
    };
    port.opened = true;

    Ok(())
}

pub fn port_open_custom_port(
    ctx: &mut Context,
    mut port: Handle<Port>,
    name: Value,
    direction: u8,
    handlers: Value,
    transcoder: Value,
) -> Result<(), IoError> {
    port.fd = -1;
    port.opened = false;
    port.typ = Port::TYPE_CUSTOM;
    port.subtype = Port::SUBTYPE_NONE;
    port.handlers = handlers;
    port.bytes = Value::new(ByteVector::new(ctx, Port::CUSTOM_BUFFER_SIZE));
    port.name = name;
    port.direction = direction;
    port.transcoder = transcoder;
    port.buffer_mode = Port::BUFFER_MODE_BLOCK;
    port.file_options = Port::FILE_OPTION_NONE;
    port.force_sync = false;
    init_port_tracking(port);
    init_port_transcode(port);
    init_port_buffer(port);
    port.opened = true;

    Ok(())
}

pub fn port_open_transcoder_port(
    ctx: &mut Context,
    mut binary: Handle<Port>,
    mut textual: Handle<Port>,
    name: Value,
    transcoder: Value,
) -> Result<(), IoError> {
    ctx.mutator().write_barrier(textual);
    textual.bytes = binary.bytes;
    textual.lookahead.copy_from_slice(&binary.lookahead);
    textual.lookahead_size = binary.lookahead_size;
    textual.buf = binary.buf;
    textual.buf_tail = binary.buf_tail;
    textual.buf_size = binary.buf_size;
    textual.buf_state = binary.buf_state;
    textual.mark = binary.mark;
    textual.line = binary.line;
    textual.column = binary.column;
    textual.name = name;
    textual.fd = binary.fd;
    textual.transcoder = transcoder;
    textual.codec = binary.codec;
    textual.eol_style = binary.eol_style;
    textual.error_handling_mode = binary.error_handling_mode;
    textual.file_options = binary.file_options;
    textual.buffer_mode = binary.buffer_mode;
    textual.typ = binary.typ;
    textual.subtype = binary.subtype;
    textual.handlers = binary.handlers;
    textual.direction = binary.direction;
    textual.track_line_column = binary.track_line_column;
    textual.opened = binary.opened;
    textual.force_sync = binary.force_sync;

    binary.fd = -1;
    binary.buf = null_mut();
    binary.buf_head = null_mut();
    binary.buf_tail = null_mut();
    binary.buf_size = 0;
    binary.buf_state = Port::BUF_STATE_UNSPECIFIED;
    binary.opened = false;
    Ok(())
}

pub fn port_flush_output(ctx: &mut Context, mut port: Handle<Port>) -> Result<(), IoError> {
    if port.opened {
        if no_output_buffered(port) {
            return Ok(());
        }
        unsafe {
            let n = port.buf_tail as usize - port.buf_head as usize;
            let buf = std::slice::from_raw_parts_mut(port.buf_head, n);
            device_write(ctx, port, buf, port.mark - n as i64)?;
            port.buf_head = port.buf;
            port.buf_tail = port.buf;
            port.buf_state = Port::BUF_STATE_UNSPECIFIED;
        }
    }

    Ok(())
}

pub fn port_discard_buffer(_: &mut Context, mut port: Handle<Port>) {
    if !port.buf.is_null() {
        unsafe {
            free(port.buf.cast());
        }
        port.buf = null_mut();
        port.buf_head = null_mut();
        port.buf_tail = null_mut();
        port.buf_size = 0;
        port.buf_state = Port::BUF_STATE_UNSPECIFIED;
    }
}

pub fn port_close(ctx: &mut Context, mut port: Handle<Port>) -> Result<(), IoError> {
    if port.opened {
        port_flush_output(ctx, port)?;
        port_discard_buffer(ctx, port);
        device_close(ctx, port)?;
        port.opened = false;
    }

    Ok(())
}

pub fn port_nonblock_byte_ready(_: &mut Context, port: Handle<Port>) -> Result<bool, IoError> {
    if port.opened {
        match port.typ {
            Port::TYPE_NAMED_FILE => match port.subtype {
                Port::SUBTYPE_FIFO | Port::SUBTYPE_CHAR_SPECIAL => {
                    if no_input_buffered(port) {
                        unsafe {
                            let mut tm = timeval {
                                tv_sec: 0,
                                tv_usec: 0,
                            };
                            let mut fds = MaybeUninit::<fd_set>::zeroed().assume_init();

                            FD_ZERO(&mut fds);
                            FD_SET(port.fd, &mut fds);
                            let state =
                                select(port.fd + 1, &mut fds, null_mut(), null_mut(), &mut tm);
                            if state < 0 {
                                if errno::errno().0 == EINTR {
                                    return Ok(false);
                                }

                                return Err(IoError::Code(
                                    Operation::Port(PortOperation::Select),
                                    errno::errno().0,
                                ));
                            }
                            return Ok(state != 0);
                        }
                    }

                    Ok(true)
                }
                _ => Ok(true),
            },

            _ => Ok(true),
        }
    } else {
        Ok(true)
    }
}

pub fn port_buffered_bytecount(port: Handle<Port>) -> isize {
    if port.opened {
        match port.typ {
            Port::TYPE_NAMED_FILE => {
                if port.buf_state == Port::BUF_STATE_READ {
                    return port.buf_tail as isize - port.buf_head as isize;
                }
                return port.lookahead_size as _;
            }

            Port::TYPE_BYTEVECTOR => {
                let bvector = port.bytes.get_bytes();
                return bvector.len() as isize - port.mark as isize;
            }
            _ => (),
        }
    }

    0
}

pub fn port_eof(port: Handle<Port>) -> Result<bool, IoError> {
    if port.opened {
        match port.typ {
            Port::TYPE_NAMED_FILE => match port.subtype {
                Port::SUBTYPE_NONE => {
                    if no_input_buffered(port) {
                        let mut size = 0;
                        if io_fstat_size(port.fd, &mut size) == 0 {
                            return Ok(size <= port.mark);
                        }

                        return Err(IoError::Code(
                            Operation::Port(PortOperation::Stat),
                            errno::errno().0,
                        ));
                    }
                }
                _ => (),
            },

            Port::TYPE_BYTEVECTOR => {
                let bvec = port.bytes.get_bytes();
                return Ok((bvec.len() as i64) <= port.mark);
            }

            _ => (),
        }
    }

    Ok(true)
}

pub fn port_lookahead_byte(ctx: &mut Context, mut port: Handle<Port>) -> Result<i32, IoError> {
    if port.opened {
        match port.typ {
            Port::TYPE_BYTEVECTOR => {
                let bvec = port.bytes.get_bytes();
                if (bvec.len() as i64) <= port.mark {
                    return Ok(EOF);
                }
                return Ok(bvec[port.mark as usize] as i32);
            }
            _ => {
                if !port.buf.is_null() {
                    if port.buf_state == Port::BUF_STATE_READ && port.buf_head != port.buf_tail {
                        unsafe { return Ok(port.buf_head.read() as i32) }
                    }

                    port_flush_output(ctx, port)?;
                    port.buf_state = Port::BUF_STATE_READ;
                    port.buf_head = port.buf;
                    port.buf_tail = port.buf;
                    let buf =
                        unsafe { std::slice::from_raw_parts_mut(port.buf, port.buf_size as usize) };
                    let n = device_read(ctx, port, buf, port.mark)?;
                    if n == 0 {
                        return Ok(EOF);
                    }

                    port.buf_tail = unsafe { port.buf.offset(n) };
                    unsafe {
                        return Ok(port.buf_head.read() as i32);
                    }
                } else {
                    if port.lookahead_size != 0 {
                        return Ok(port.lookahead[0] as i32);
                    }

                    let mut b = [0];

                    let n = device_read(ctx, port, &mut b, port.mark)?;
                    if n == 0 {
                        return Ok(EOF);
                    }

                    port.lookahead[0] = b[0];
                    port.lookahead_size = 1;
                    return Ok(b[0] as i32);
                }
            }
        }
    }

    Ok(EOF)
}

fn port_update_line_column(mut port: Handle<Port>, c: i32) {
    if c == b'\n' as i32 {
        port.line += 1;
        port.column = 1;
        return;
    }

    port.column += 1;
}

pub fn port_get_byte(ctx: &mut Context, mut port: Handle<Port>) -> Result<i32, IoError> {
    if port.opened {
        match port.typ {
            Port::TYPE_BYTEVECTOR => {
                let bvec = port.bytes.get_bytes();
                if (bvec.len() as i64) <= port.mark {
                    return Ok(EOF);
                }
                let b = bvec[port.mark as usize];
                if port.track_line_column {
                    port_update_line_column(port, b as _);
                }
                port.mark += 1;
                return Ok(b as _);
            }

            _ => {
                if !port.buf.is_null() {
                    if port.buf_state != Port::BUF_STATE_READ || port.buf_head == port.buf_tail {
                        port_flush_output(ctx, port)?;
                        port.buf_state = Port::BUF_STATE_READ;
                        port.buf_head = port.buf;
                        port.buf_tail = port.buf;

                        let buf = unsafe {
                            std::slice::from_raw_parts_mut(port.buf, port.buf_size as usize)
                        };
                        let n = device_read(ctx, port, buf, port.mark)?;
                        if n == 0 {
                            return Ok(EOF);
                        }
                        port.buf_tail = unsafe { port.buf.offset(n) };
                    }

                    let b = unsafe { port.buf_head.read() };
                    unsafe {
                        port.buf_head = port.buf_head.add(1);
                    }

                    if port.track_line_column {
                        port_update_line_column(port, b as _);
                    }

                    port.mark += 1;
                    return Ok(b as _);
                } else {
                    let b = if port.lookahead_size != 0 {
                        let b = port.lookahead[0];
                        port.lookahead[0] = port.lookahead[1];
                        port.lookahead[1] = port.lookahead[2];
                        port.lookahead[2] = port.lookahead[3];
                        port.lookahead_size -= 1;
                        b
                    } else {
                        let mut b = [0];
                        let n = device_read(ctx, port, &mut b, port.mark)?;
                        if n == 0 {
                            return Ok(EOF);
                        }

                        b[0]
                    };

                    if port.track_line_column {
                        port_update_line_column(port, b as _);
                    }

                    port.mark += 1;
                    return Ok(b as _);
                }
            }
        }
    }

    Ok(EOF)
}

pub fn port_get_bytes(
    ctx: &mut Context,
    mut port: Handle<Port>,
    buf: &mut [u8],
) -> Result<usize, IoError> {
    if buf.len() == 0 {
        return Ok(0);
    }

    if port.opened {
        match port.typ {
            Port::TYPE_BYTEVECTOR => {
                let bvec = port.bytes.get_bytes();

                if bvec.len() <= port.mark as usize {
                    return Ok(0);
                }

                let available = bvec.len() - port.mark as usize;
                let bsize = buf.len().min(available);

                unsafe {
                    core::ptr::copy_nonoverlapping(
                        bvec.as_ptr().add(port.mark as _),
                        buf.as_mut_ptr(),
                        bsize,
                    );
                }

                if port.track_line_column {
                    for i in 0..bsize {
                        port_update_line_column(port, buf[i] as _);
                    }
                }

                port.mark = port.mark + bsize as i64;
                return Ok(bsize);
            }

            _ => {
                if !port.buf.is_null() {
                    if port.buf_state != Port::BUF_STATE_READ || port.buf_head == port.buf_tail {
                        port_flush_output(ctx, port)?;
                        port.buf_state = Port::BUF_STATE_READ;
                        port.buf_head = port.buf;
                        port.buf_tail = port.buf;

                        let n = device_read(
                            ctx,
                            port,
                            unsafe { std::slice::from_raw_parts_mut(port.buf, port.buf_size) },
                            port.mark,
                        )?;

                        if n == 0 {
                            return Ok(0);
                        }

                        port.buf_tail = unsafe { port.buf.offset(n) };
                    }

                    let available = port.buf_tail as isize - port.buf_head as isize;
                    let n = buf.len().min(available as usize);

                    unsafe {
                        core::ptr::copy_nonoverlapping(port.buf_head, buf.as_mut_ptr(), n);
                    }

                    if port.track_line_column {
                        for i in 0..n {
                            port_update_line_column(port, buf[i] as _);
                        }
                    }

                    port.buf_head = unsafe { port.buf_head.add(n) };
                    port.mark += n as i64;

                    if n == buf.len() {
                        return Ok(n);
                    }

                    return Ok(n + port_get_bytes(ctx, port, &mut buf[n..])?);
                } else {
                    if port.lookahead_size != 0 {
                        let b = port.lookahead[0];
                        port.lookahead[0] = port.lookahead[1];
                        port.lookahead[1] = port.lookahead[2];
                        port.lookahead[2] = port.lookahead[3];
                        port.lookahead_size -= 1;
                        buf[0] = b;
                        if port.track_line_column {
                            port_update_line_column(port, b as _);
                        }

                        port.mark += 1;

                        if buf.len() == 1 {
                            return Ok(1);
                        }

                        return Ok(1 + port_get_bytes(ctx, port, &mut buf[1..])?);
                    } else {
                        let n = device_read(ctx, port, buf, port.mark)?;
                        if n == 0 {
                            return Ok(0);
                        }

                        if port.track_line_column {
                            for i in 0..n {
                                port_update_line_column(port, buf[i as usize] as _);
                            }
                        }

                        port.mark += n as i64;
                        return Ok(n as _);
                    }
                }
            }
        }
    }

    Ok(0)
}

pub fn port_lookahead_utf8(ctx: &mut Context, mut port: Handle<Port>) -> Result<Value, IoError> {
    let hit_eof = |port: Handle<Port>| match port.error_handling_mode {
        Port::ERROR_HANDLING_MODE_IGNORE => return Ok(Value::eof()),
        Port::ERROR_HANDLING_MODE_REPLACE => {
            todo!()
        }

        _ => unreachable!(),
    };
    if port.opened {
        loop {
            let mut utf8: [u8; 6] = [0; 6];

            let b = port_lookahead_byte(ctx, port)?;
            if b == EOF {
                return Ok(Value::eof());
            }

            if b > 127 {
                let code_length = utf8_byte_count(b as _);

                utf8[0] = b as u8;
                if code_length > 1 {
                    match port.typ {
                        Port::TYPE_BYTEVECTOR => {
                            let bvec = port.bytes.get_bytes();
                            if bvec.len() as i64 - port.mark < code_length as i64 {
                                return Ok(Value::eof());
                            }
                            for i in 1..code_length {
                                utf8[i] = bvec[(port.mark + i as i64) as usize]
                            }
                        }

                        _ => {
                            if !port.buf.is_null() {
                                let mut n1 = port.buf_tail as usize - port.buf_head as usize;
                                while n1 < code_length {
                                    if port.buf != port.buf_head {
                                        unsafe {
                                            libc::memmove(
                                                port.buf.cast(),
                                                port.buf_head.cast(),
                                                n1 as _,
                                            );
                                            port.buf_head = port.buf;
                                        }
                                    }

                                    let buf = unsafe {
                                        std::slice::from_raw_parts_mut(
                                            port.buf.add(n1),
                                            port.buf_size - n1,
                                        )
                                    };

                                    let n2 = device_read(ctx, port, buf, port.mark + n1 as i64)?;
                                    n1 = (n1 as isize + n2) as _;
                                    port.buf_tail = unsafe { port.buf_head.add(n1) };

                                    if n2 == 0 {
                                        return hit_eof(port);
                                    }
                                }

                                unsafe {
                                    core::ptr::copy_nonoverlapping(
                                        port.buf_head,
                                        utf8.as_mut_ptr(),
                                        code_length,
                                    );
                                }
                            } else {
                                while port.lookahead_size < code_length {
                                    let buf = unsafe {
                                        std::slice::from_raw_parts_mut(
                                            port.lookahead.as_mut_ptr().add(port.lookahead_size),
                                            code_length - port.lookahead_size,
                                        )
                                    };
                                    let n = device_read(
                                        ctx,
                                        port,
                                        buf,
                                        port.mark + port.lookahead_size as i64,
                                    )?;
                                    port.lookahead_size += n as usize;
                                    if n == 0 {
                                        return hit_eof(port);
                                    }
                                }

                                unsafe {
                                    core::ptr::copy_nonoverlapping(
                                        port.lookahead.as_ptr(),
                                        utf8.as_mut_ptr(),
                                        code_length,
                                    );
                                }
                            }
                        }
                    }
                }
            }

            let mut ucs4 = 0;
            if cnvt_utf8_to_ucs4(&utf8, &mut ucs4) < 1 {
                match port.error_handling_mode {
                    Port::ERROR_HANDLING_MODE_IGNORE => {
                        continue;
                    }
                    Port::ERROR_HANDLING_MODE_REPLACE => {
                        ucs4 = Port::UCS4_REPLACEMENT_CHAR;
                    }

                    Port::ERROR_HANDLING_MODE_RAISE => {
                        return Err(IoError::Message(
                            Operation::Port(PortOperation::Decode),
                            "encountered invalid UTF-8 sequence".to_string(),
                        ))
                    }
                    _ => unreachable!(),
                }
            }

            if port.mark == 0 && ucs4 == Port::UCS4_BOM {
                port_get_byte(ctx, port)?;
                port_get_byte(ctx, port)?;
                port_get_byte(ctx, port)?;
                continue;
            }

            return Ok(Value::encode_char(ucs4));
        }
    }

    return Ok(Value::eof());
}

pub fn port_get_utf8(ctx: &mut Context, port: Handle<Port>) -> Result<Value, IoError> {
    if port.opened {
        loop {
            let mut utf8 = [0u8; 6];

            let b = port_get_byte(ctx, port)?;
            if b == EOF {
                return Ok(Value::eof());
            }

            if b > 127 {
                let code_length = utf8_byte_count(b as _);
                utf8[0] = b as u8;

                for i in 1..code_length {
                    let b = port_get_byte(ctx, port)?;
                    if b == EOF {
                        match port.error_handling_mode {
                            Port::ERROR_HANDLING_MODE_IGNORE => {
                                return Ok(Value::eof());
                            }

                            Port::ERROR_HANDLING_MODE_REPLACE => {
                                return Ok(Value::encode_char(Port::UCS4_REPLACEMENT_CHAR));
                            }
                            _ => {
                                return Err(IoError::Message(
                                    Operation::Port(PortOperation::Decode),
                                    "encountered invalid UTF-8 sequence".to_string(),
                                ))
                            }
                        }
                    }

                    utf8[i] = b as u8;
                }

                let mut ucs4 = 0;

                if cnvt_utf8_to_ucs4(&utf8, &mut ucs4) < 1 {
                    match port.error_handling_mode {
                        Port::ERROR_HANDLING_MODE_IGNORE => {
                            continue;
                        }
                        Port::ERROR_HANDLING_MODE_REPLACE => {
                            ucs4 = Port::UCS4_REPLACEMENT_CHAR;
                        }

                        Port::ERROR_HANDLING_MODE_RAISE => {
                            return Err(IoError::Message(
                                Operation::Port(PortOperation::Decode),
                                "encountered invalid UTF-8 sequence".to_string(),
                            ))
                        }
                        _ => unreachable!(),
                    }
                }

                if port.mark == 3 && ucs4 == Port::UCS4_BOM {
                    continue;
                }

                return Ok(Value::encode_char(ucs4));
            }

            return Ok(Value::encode_char(b as _));
        }
    }

    Ok(Value::eof())
}

pub fn port_set_mark(
    ctx: &mut Context,
    mut port: Handle<Port>,
    offset: i64,
) -> Result<(), IoError> {
    if port.opened {
        port_flush_output(ctx, port)?;
        port.lookahead_size = 0;

        match port.typ {
            Port::TYPE_BYTEVECTOR => {
                port.mark = offset;
            }
            _ => {
                device_set_mark(ctx, port, offset)?;
            }
        }

        port.track_line_column = false;
        port.column = 0;
        port.line = 0;
    }

    Ok(())
}

const fn hundred_twenty_five_percent_of(n: usize) -> usize {
    n + (n >> 2)
}

pub fn port_put_byte(ctx: &mut Context, mut port: Handle<Port>, byte: i32) -> Result<(), IoError> {
    if port.opened {
        port.lookahead_size = 0;

        if port.track_line_column {
            port_update_line_column(port, byte);
        }

        match port.typ {
            Port::TYPE_BYTEVECTOR => {
                if port.mark >= port.buf_size as i64 {
                    let mut newsize = port.mark as usize + Port::BYTEVECTOR_OUTPUT_CHUNK;
                    let cursize = port.buf_tail as usize - port.buf_head as usize;

                    if newsize < hundred_twenty_five_percent_of(cursize) {
                        newsize = hundred_twenty_five_percent_of(cursize);
                    }

                    if newsize < (port.mark + Port::BYTEVECTOR_OUTPUT_CHUNK as i64) as usize {
                        return Err(IoError::Message(
                            Operation::Port(PortOperation::Write),
                            "bytevector output buffer overflow".to_string(),
                        ));
                    }

                    let prev = port.buf;
                    port.buf = unsafe { libc::realloc(port.buf.cast(), newsize).cast() };
                    if port.buf.is_null() {
                        port.buf = prev;
                        return Err(IoError::Message(
                            Operation::Port(PortOperation::Write),
                            "bytevector output buffer overflow".to_string(),
                        ));
                    }

                    unsafe {
                        write_bytes(port.buf.add(newsize), 0, newsize - cursize);
                    }

                    port.buf_head = port.buf;
                    port.buf_tail = unsafe { port.buf.add(cursize) };
                    port.buf_size = newsize;
                }
                unsafe {
                    let mut p = port.buf_head.offset(port.mark as _);
                    p.write(byte as _);
                    port.mark += 1;

                    p = p.add(1);
                    if p > port.buf_tail {
                        port.buf_tail = p;
                    }
                }
            }

            _ => unsafe {
                if !port.buf.is_null() {
                    if port.buf_state != Port::BUF_STATE_WRITE {
                        port.buf_head = port.buf;
                        port.buf_tail = port.buf;
                        port.buf_state = Port::BUF_STATE_WRITE;
                    } else {
                        if port.buf_tail == port.buf.add(port.buf_size) {
                            port_flush_output(ctx, port)?;
                            port.buf_state = Port::BUF_STATE_WRITE;
                        }
                    }

                    port.buf_tail.write(byte as _);
                    port.buf_tail = port.buf_tail.add(1);
                    port.mark += 1;

                    if port.buffer_mode == Port::BUFFER_MODE_LINE {
                        if byte == b'\n' as i32 {
                            port_flush_output(ctx, port)?;
                        }
                    }
                } else {
                    let b = byte as u8;
                    device_write(ctx, port, &[b], port.mark)?;
                    port.mark += 1;
                }
            },
        }
    }

    Ok(())
}

pub fn port_put_bytes(
    ctx: &mut Context,
    mut port: Handle<Port>,
    bytes: &[u8],
) -> Result<(), IoError> {
    if port.opened {
        match port.typ {
            Port::TYPE_BYTEVECTOR => unsafe {
                if port.mark >= port.buf_size as i64 {
                    let mut newsize = port.mark as usize + Port::BYTEVECTOR_OUTPUT_CHUNK;
                    let cursize = port.buf_tail as usize - port.buf_head as usize;

                    if newsize < hundred_twenty_five_percent_of(cursize) {
                        newsize = hundred_twenty_five_percent_of(cursize);
                    }

                    if newsize < (port.mark + Port::BYTEVECTOR_OUTPUT_CHUNK as i64) as usize {
                        return Err(IoError::Message(
                            Operation::Port(PortOperation::Write),
                            "bytevector output buffer overflow".to_string(),
                        ));
                    }

                    let prev = port.buf;
                    port.buf = libc::realloc(port.buf.cast(), newsize).cast();
                    if port.buf.is_null() {
                        port.buf = prev;
                        return Err(IoError::Message(
                            Operation::Port(PortOperation::Write),
                            "bytevector output buffer overflow".to_string(),
                        ));
                    }

                    write_bytes(port.buf.add(newsize), 0, newsize - cursize);

                    port.buf_head = port.buf;
                    port.buf_tail = port.buf.add(cursize);
                    port.buf_size = newsize;
                }

                let space = port.buf_size as i64 - port.mark;
                let n = if space < bytes.len() as i64 {
                    space as usize
                } else {
                    bytes.len()
                };

                copy_nonoverlapping(bytes.as_ptr(), port.buf_head.offset(port.mark as _), n);

                port.mark += n as i64;

                if port.buf_head.offset(port.mark as _) > port.buf_tail {
                    port.buf_tail = port.buf_head.offset(port.mark as _);
                }

                if port.track_line_column {
                    for i in 0..n {
                        port_update_line_column(port, bytes[i] as _);
                    }
                }

                if n != bytes.len() {
                    port_put_bytes(ctx, port, &bytes[n..])?;
                }
            },

            _ => unsafe {
                if !port.buf.is_null() {
                    if port.buf_state != Port::BUF_STATE_WRITE {
                        port.buf_head = port.buf;
                        port.buf_tail = port.buf;
                        port.buf_state = Port::BUF_STATE_WRITE;
                    } else {
                        if port.buf_tail == port.buf.add(port.buf_size) {
                            port_flush_output(ctx, port)?;
                            port.buf_state = Port::BUF_STATE_WRITE;
                        }
                    }

                    let space = port.buf_size - (port.buf_tail as usize - port.buf as usize);
                    let n = if space < bytes.len() {
                        space
                    } else {
                        bytes.len()
                    };

                    copy_nonoverlapping(bytes.as_ptr(), port.buf_tail, n);
                    port.buf_tail = port.buf_tail.add(n);
                    port.mark += n as i64;

                    if port.track_line_column {
                        for i in 0..n {
                            port_update_line_column(port, bytes[i] as _);
                        }
                    }

                    if n == bytes.len() && port.buffer_mode == Port::BUFFER_MODE_LINE {
                        let mut flush = false;
                        for i in 0..n {
                            if bytes[i] == b'\n' as u8 {
                                flush = true;
                                break;
                            }
                        }

                        if flush {
                            port_flush_output(ctx, port)?;
                        }
                    }
                } else {
                    device_write(ctx, port, bytes, port.mark)?;
                    port.mark += bytes.len() as i64;

                    if port.track_line_column {
                        for i in 0..bytes.len() {
                            port_update_line_column(port, bytes[i] as _);
                        }
                    }
                }
            },
        }
    }

    Ok(())
}

pub fn port_put_utf8(ctx: &mut Context, port: Handle<Port>, ucs4: i32) -> Result<(), IoError> {
    let mut utf8: [u8; 4] = [0; 4];
    let n = cnvt_ucs4_to_utf8(ucs4 as _, &mut utf8);
    for i in 0..n {
        port_put_byte(ctx, port, utf8[i] as _)?;
    }
    Ok(())
}

pub fn port_input_pred(port: Handle<Port>) -> bool {
    (port.direction & Port::DIRECTION_IN) != 0
}

pub fn port_output_pred(port: Handle<Port>) -> bool {
    (port.direction & Port::DIRECTION_OUT) != 0
}

pub fn port_textual_pred(port: Handle<Port>) -> bool {
    !port.transcoder.is_false()
}

pub fn port_binary_pred(port: Handle<Port>) -> bool {
    port.transcoder.is_false()
}

pub fn port_open_pred(port: Handle<Port>) -> bool {
    port.opened
}

pub fn port_bytevector_pred(port: Handle<Port>) -> bool {
    port.typ == Port::TYPE_BYTEVECTOR
}

pub fn port_regular_file_pred(port: Handle<Port>) -> bool {
    port.typ == Port::TYPE_NAMED_FILE && port.subtype == Port::SUBTYPE_NONE
}

pub fn port_get_bytevector(ctx: &mut Context, port: Handle<Port>) -> Handle<ByteVector> {
    let n = port.buf_tail as usize - port.buf_head as usize;
    let bvector = if port.buf.is_null() {
        ByteVector::new(ctx, 0)
    } else {
        let mut bvector = ByteVector::new(ctx, n);
        unsafe {
            copy_nonoverlapping(port.buf_head, &mut bvector[0], n);
        }

        bvector
    };

    bvector
}

pub fn port_extract_bytevector(ctx: &mut Context, mut port: Handle<Port>) -> Handle<ByteVector> {
    let bvec = port_get_bytevector(ctx, port);

    unsafe {
        libc::free(port.buf.cast());
    }

    port.buf = null_mut();
    port.buf_head = null_mut();
    port.buf_tail = null_mut();
    port.buf_size = 0;
    port.mark = 0;
    bvec
}

pub fn port_get_string_ref<'a>(port: &'a Handle<Port>) -> &'a str {
    if port.buf.is_null() {
        ""
    } else {
        unsafe {
            std::str::from_utf8_unchecked(std::slice::from_raw_parts(
                port.buf_head,
                port.buf_tail as usize - port.buf_head as usize,
            ))
        }
    }
}
pub fn port_get_string(ctx: &mut Context, port: Handle<Port>) -> Handle<Str> {
    Str::new(ctx.mutator(), port_get_string_ref(&port))
}

pub fn port_extract_string(ctx: &mut Context, mut port: Handle<Port>) -> Handle<Str> {
    let s = port_get_string(ctx, port);

    unsafe {
        libc::free(port.buf.cast());
    }

    port.buf = null_mut();
    port.buf_head = null_mut();
    port.buf_tail = null_mut();
    port.buf_size = 0;
    port.mark = 0;
    s
}

pub fn port_lookahead_ch(ctx: &mut Context, port: Handle<Port>) -> Result<Value, IoError> {
    match port.codec {
        Port::BUFFER_CODEC_UTF8 => port_lookahead_utf8(ctx, port),
        _ => {
            let b = port_lookahead_byte(ctx, port)?;
            if b == EOF {
                return Ok(Value::eof());
            }

            Ok(Value::encode_char(b as _))
        }
    }
}

pub fn port_lookahead_char(ctx: &mut Context, port: Handle<Port>) -> Result<Value, IoError> {
    let ch = port_lookahead_ch(ctx, port)?;

    if ch.is_eof() {
        return Ok(ch);
    }

    if port.transcoder.is_false() {
        return Ok(ch);
    }

    if port.eol_style == Port::EOL_STYLE_NONE {
        return Ok(ch);
    }

    match ch.get_native_u32() {
        Port::UCS4_CR | Port::UCS4_NEL | Port::UCS4_LF => Ok(Value::encode_char(Port::UCS4_LF)),
        _ => Ok(ch),
    }
}

pub fn port_get_ch(ctx: &mut Context, port: Handle<Port>) -> Result<Value, IoError> {
    match port.codec {
        Port::BUFFER_CODEC_UTF8 => port_get_utf8(ctx, port),
        _ => {
            let b = port_get_byte(ctx, port)?;
            if b == EOF {
                return Ok(Value::eof());
            }

            Ok(Value::encode_char(b as _))
        }
    }
}

pub fn port_get_char(ctx: &mut Context, port: Handle<Port>) -> Result<Value, IoError> {
    let ch = port_get_ch(ctx, port)?;

    if ch.is_eof() {
        return Ok(ch);
    }

    if port.transcoder.is_false() {
        return Ok(ch);
    }

    if port.eol_style == Port::EOL_STYLE_NONE {
        return Ok(ch);
    }

    match ch.get_native_u32() {
        Port::UCS4_CR => {
            let post = port_lookahead_ch(ctx, port)?;
            match post.get_native_u32() {
                Port::UCS4_LF | Port::UCS4_NEL => {
                    port_get_ch(ctx, port)?;
                    return Ok(Value::encode_char(Port::UCS4_LF));
                }
                _ => Ok(Value::encode_char(Port::UCS4_LF)),
            }
        }
        Port::UCS4_NEL | Port::UCS4_LS => Ok(Value::encode_char(Port::UCS4_LF)),
        _ => Ok(ch),
    }
}

pub fn port_lookahead_u8(ctx: &mut Context, port: Handle<Port>) -> Result<Value, IoError> {
    let b = port_lookahead_byte(ctx, port)?;
    if b == EOF {
        return Ok(Value::eof());
    }

    Ok(Value::new(b))
}

pub fn port_get_u8(ctx: &mut Context, port: Handle<Port>) -> Result<Value, IoError> {
    let b = port_get_byte(ctx, port)?;
    if b == EOF {
        return Ok(Value::eof());
    }

    Ok(Value::new(b))
}

pub fn make_port(ctx: &mut Context) -> Handle<Port> {
    ctx.mutator().allocate(Port {
        lock: Mutex::new(()),
        handlers: Value::nil(),
        bytes: Value::nil(),
        lookahead: [0; PORT_LOOKAHEAD_SIZE],
        buf: null_mut(),
        buf_head: null_mut(),
        buf_tail: null_mut(),
        name: Value::nil(),
        transcoder: Value::nil(),
        mark: 0,
        fd: -1,
        lookahead_size: 0,
        buf_size: 0,
        buf_state: Port::BUF_STATE_UNSPECIFIED,
        line: -1,
        column: -1,
        codec: Port::CODEC_NATIVE,
        eol_style: Port::EOL_STYLE_NATIVE,
        error_handling_mode: Port::ERROR_HANDLING_MODE_IGNORE,
        file_options: 0,
        buffer_mode: Port::BUFFER_MODE_NONE,
        typ: Port::TYPE_NAMED_FILE,
        subtype: Port::SUBTYPE_NONE,
        direction: Port::DIRECTION_IN,
        force_sync: false,
        bom_be: false,
        bom_le: false,
        track_line_column: false,
        opened: false,
    })
}

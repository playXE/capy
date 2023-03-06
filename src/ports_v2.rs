use std::{ffi::CString, mem::MaybeUninit, ptr::null_mut};

use rsgc::{
    prelude::{Allocation, Handle, Object},
    sync::mutex::RawMutex,
    thread::Thread,
};

use crate::{error::make_srcloc, raise_exn, value::*};

#[repr(C)]
pub struct Port {
    pub(crate) hdr: Hdr,
    pub(crate) lock: RawMutex,
    pub(crate) handlers: Value,
    pub(crate) bytes: Value,
    pub(crate) lookahead: [u8; 8],
    pub(crate) buf: *mut u8,
    pub(crate) buf_head: *mut u8,
    pub(crate) buf_tail: *mut u8,
    pub(crate) name: Value,
    pub(crate) transcoder: Value,
    pub(crate) mark: i64,
    pub(crate) fd: i32,
    pub(crate) lookahead_size: usize,
    pub(crate) buf_size: usize,
    pub(crate) buf_state: usize,
    pub(crate) line: i32,
    pub(crate) column: i32,

    pub(crate) codec: u8,
    pub(crate) eol_style: u8,
    pub(crate) error_handling_mode: u8,
    pub(crate) file_options: u8,
    pub(crate) buffer_mode: u8,
    pub(crate) typ: u8,
    pub(crate) subtype: u8,
    pub(crate) direction: u8,

    pub(crate) force_sync: bool,
    pub(crate) bom_le: bool,
    pub(crate) bom_be: bool,
    pub(crate) track_line_column: bool,
    pub(crate) opened: bool,
}

impl Port {
    pub fn new(thread: &mut Thread) -> Handle<Self> {
        thread.allocate(Self {
            hdr: Hdr::new(Type::Port),
            lock: RawMutex::new(),
            handlers: Value::make_null(),
            bytes: Value::make_null(),
            lookahead: [0; 8],
            buf: null_mut(),
            buf_head: null_mut(),
            buf_tail: null_mut(),
            name: Value::make_null(),
            transcoder: Value::make_null(),
            mark: 0,
            fd: -1,
            lookahead_size: 0,
            buf_size: 0,
            buf_state: 0,
            line: 0,
            column: 0,
            codec: 0,
            eol_style: 0,
            error_handling_mode: 0,
            file_options: 0,
            buffer_mode: 0,
            typ: 0,
            subtype: 0,
            direction: 0,
            force_sync: false,
            bom_le: false,
            bom_be: false,
            track_line_column: false,
            opened: false,
        })
    }
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

pub const SCM_PORT_TYPE_NAMED_FILE: u8 = 1;
pub const SCM_PORT_TYPE_BYTEVECTOR: u8 = 2;
pub const SCM_PORT_TYPE_CUSTOM: u8 = 3;
pub const SCM_PORT_TYPE_SOCKET: u8 = 4;

pub const SCM_PORT_SUBTYPE_NONE: u8 = 0;
pub const SCM_PORT_SUBTYPE_CHAR_SPECIAL: u8 = 1; // non-positionable named file
pub const SCM_PORT_SUBTYPE_FIFO: u8 = 2;

pub const SCM_PORT_DIRECTION_IN: u8 = 0x01;
pub const SCM_PORT_DIRECTION_OUT: u8 = 0x02;
pub const SCM_PORT_DIRECTION_BOTH: u8 = 0x03;

pub const SCM_PORT_BUFFER_MODE_NONE: u8 = 1;
pub const SCM_PORT_BUFFER_MODE_LINE: u8 = 2;
pub const SCM_PORT_BUFFER_MODE_BLOCK: u8 = 3;

pub const SCM_PORT_FILE_OPTION_NONE: u8 = 0;
pub const SCM_PORT_FILE_OPTION_NO_CREATE: u8 = 0x01;
pub const SCM_PORT_FILE_OPTION_NO_FAIL: u8 = 0x02;
pub const SCM_PORT_FILE_OPTION_NO_TRUNCATE: u8 = 0x04;

pub const SCM_PORT_CODEC_LATIN1: u8 = 1;
pub const SCM_PORT_CODEC_UTF8: u8 = 2;
pub const SCM_PORT_CODEC_UTF16: u8 = 3;

pub const SCM_PORT_EOL_STYLE_NONE: u8 = 1;
pub const SCM_PORT_EOL_STYLE_LF: u8 = 2;
pub const SCM_PORT_EOL_STYLE_CR: u8 = 3;
pub const SCM_PORT_EOL_STYLE_CRLF: u8 = 4;
pub const SCM_PORT_EOL_STYLE_NEL: u8 = 5;
pub const SCM_PORT_EOL_STYLE_CRNEL: u8 = 6;
pub const SCM_PORT_EOL_STYLE_LS: u8 = 7;

pub const SCM_PORT_ERROR_HANDLING_MODE_IGNORE: u8 = 1;
pub const SCM_PORT_ERROR_HANDLING_MODE_RAISE: u8 = 2;
pub const SCM_PORT_ERROR_HANDLING_MODE_REPLACE: u8 = 3;

pub const SCM_PORT_BLOCK_BUFFER_SIZE: usize = 4096;
pub const SCM_PORT_LINE_BUFFER_SIZE: usize = 256;
pub const SCM_PORT_CUSTOM_BUFFER_SIZE: usize = SCM_PORT_BLOCK_BUFFER_SIZE;

pub const SCM_PORT_BUF_STATE_UNSPECIFIED: usize = 0;
pub const SCM_PORT_BUF_STATE_READ: usize = 1;
pub const SCM_PORT_BUF_STATE_WRITE: usize = 2;
pub const SCM_PORT_BUF_STATE_ACCUMULATE: usize = 3;

pub const SCM_PORT_UCS4_BOM: u32 = 0x0feff;
pub const SCM_PORT_UCS4_REPLACEMENT_CHAR: u32 = 0x0fffd;
pub const SCM_PORT_UCS4_LF: u32 = 0x0000a;
pub const SCM_PORT_UCS4_CR: u32 = 0x0000d;
pub const SCM_PORT_UCS4_NEL: u32 = 0x00085;
pub const SCM_PORT_UCS4_LS: u32 = 0x02028;

pub const SCM_PORT_BYTE_REPLACEMENT_CHAR: u8 = b'?';

pub const SCM_PORT_BYTEVECTOR_OUTPUT_CHUNK: usize = 128;

// do not change
pub const SCM_PORT_CODEC_NATIVE: u8 = SCM_PORT_CODEC_UTF8;
pub const SCM_PORT_EOL_STYLE_NATIVE: u8 = SCM_PORT_EOL_STYLE_LF;

pub const SCM_PORT_HANDLER_TEXTUAL: usize = 0;
pub const SCM_PORT_HANDLER_READ: usize = 1;
pub const SCM_PORT_HANDLER_WRITE: usize = 2;
pub const SCM_PORT_HANDLER_GET_POS: usize = 3;
pub const SCM_PORT_HANDLER_SET_POS: usize = 4;
pub const SCM_PORT_HANDLER_CLOSE: usize = 5;

fn io_open(path: &str, oflag: i32, pmode: i32) -> i32 {
    let cstr = CString::new(path).unwrap();

    unsafe { libc::open(cstr.as_ptr(), oflag, pmode) }
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

fn s_istype(mode: u32, stype: u32) -> bool {
    (mode as u32 & libc::S_IFMT) == stype as u32
}

fn s_ischr(mode: u32) -> bool {
    s_istype(mode, libc::S_IFCHR)
}

fn s_isfifo(mode: u32) -> bool {
    s_istype(mode, libc::S_IFIFO)
}

fn io_stat_mode(path: &str) -> u8 {
    let mut statbuf = std::mem::MaybeUninit::<libc::stat>::uninit();

    let cstr = CString::new(path).unwrap();

    unsafe {
        if libc::stat(cstr.as_ptr(), statbuf.as_mut_ptr()) == 0 {
            let mode = statbuf.assume_init().st_mode;

            if s_ischr(mode) {
                SCM_PORT_SUBTYPE_CHAR_SPECIAL
            } else if s_isfifo(mode) {
                SCM_PORT_SUBTYPE_FIFO
            } else {
                SCM_PORT_SUBTYPE_NONE
            }
        } else {
            SCM_PORT_SUBTYPE_NONE
        }
    }
}

fn io_fstat_mode(fd: i32) -> u8 {
    let mut statbuf = std::mem::MaybeUninit::<libc::stat>::uninit();

    unsafe {
        if libc::fstat(fd, statbuf.as_mut_ptr()) == 0 {
            let mode = statbuf.assume_init().st_mode;

            if s_ischr(mode) {
                SCM_PORT_SUBTYPE_CHAR_SPECIAL
            } else if s_isfifo(mode) {
                SCM_PORT_SUBTYPE_FIFO
            } else {
                SCM_PORT_SUBTYPE_NONE
            }
        } else {
            SCM_PORT_SUBTYPE_NONE
        }
    }
}

fn io_fstat_size(fd: i32) -> Option<i64> {
    let mut statbuf = std::mem::MaybeUninit::<libc::stat>::uninit();

    unsafe {
        if libc::fstat(fd, statbuf.as_mut_ptr()) == 0 {
            Some(statbuf.assume_init().st_size as _)
        } else {
            None
        }
    }
}

fn no_input_buffered(p: Handle<Port>) -> bool {
    p.lookahead_size == 0
        && (p.buf.is_null() || p.buf_head == p.buf_tail || p.buf_state != SCM_PORT_BUF_STATE_READ)
}

fn no_output_buffered(p: Handle<Port>) -> bool {
    p.buf.is_null() || p.buf_head == p.buf_tail || p.buf_state != SCM_PORT_BUF_STATE_WRITE
}

fn device_read(port: Handle<Port>, buf: &mut [u8], mark: i64) -> Result<isize, Value> {
    if port.typ == SCM_PORT_TYPE_NAMED_FILE {
        let mut n;

        loop {
            match port.subtype {
                SCM_PORT_SUBTYPE_NONE => n = io_pread(port.fd, buf, mark),
                _ => n = io_read(port.fd, buf),
            }

            if n < 0 {
                if errno::errno().0 == libc::EINTR {
                    continue;
                } else {
                    return raise_exn!(
                        FailFilesystemErrno,
                        &[Value::make_int(errno::errno().0)],
                        "port read failed"
                    );
                }
            }

            break;
        }

        return Ok(n);
    }

    let vm = crate::vm::vm();

    let vect = port.handlers;
    let bv = port.bytes;

    let result = vm.apply(
        vect.vector_ref(SCM_PORT_HANDLER_READ),
        &[bv, Value::make_int(0), Value::make_int(buf.len() as i32)],
    )?;

    if result.intp() {
        Ok(result.int() as _)
    } else {
        raise_exn!(
            FailFilesystem,
            &[],
            "custom port! read procedure returned invalid value: {}",
            result
        )
    }
}

#[allow(unused_variables)]
fn device_write(port: Handle<Port>, mut buf: &[u8], mark: i64) -> Result<(), Value> {
    if port.typ == SCM_PORT_TYPE_NAMED_FILE {
        let mut rest = buf.len();
        let mut offset = mark;

        while rest > 0 {
            let written;

            match port.subtype {
                SCM_PORT_SUBTYPE_NONE => written = io_pwrite(port.fd, buf, offset),
                _ => written = io_write(port.fd, buf),
            }

            if written < 0 {
                if errno::errno().0 == libc::EINTR {
                    continue;
                } else {
                    return raise_exn!(
                        FailFilesystemErrno,
                        &[Value::make_int(errno::errno().0)],
                        "port write failed"
                    );
                }
            }

            buf = &buf[written as usize..];
            rest -= written as usize;
            offset += written as i64;
        }

        return Ok(());
    }

    let vm = crate::vm::vm();

    let vect = port.handlers;
    let bv = port.bytes;

    let mut rest = buf.len();
    let mut offset = mark;

    while rest > 0 {
        unsafe {
            libc::memcpy(
                bv.byte_vector_as_slice_mut().as_mut_ptr().cast(),
                buf.as_ptr().cast(),
                rest,
            );

            let result = vm.apply(
                vect.vector_ref(SCM_PORT_HANDLER_WRITE),
                &[bv, Value::make_int(0), Value::make_int(rest as i32)],
            )?;
            if result.intp() {
                let written = result.int() as usize;

                if written > rest {
                    return raise_exn!(
                        FailFilesystem,
                        &[],
                        "custom port! write procedure returned invalid value: {}",
                        result
                    );
                }

                buf = &buf[written..];
                rest -= written;
                offset += written as i64;
            } else {
                return raise_exn!(
                    FailFilesystem,
                    &[],
                    "custom port! write procedure returned invalid value: {}",
                    result
                );
            }
        }
    }

    Ok(())
}

fn device_set_mark(mut port: Handle<Port>, mark: i64) -> Result<(), Value> {
    if port.typ == SCM_PORT_TYPE_NAMED_FILE {
        let offset = mark;

        if port.subtype == SCM_PORT_SUBTYPE_NONE {
            loop {
                let result = unsafe { libc::lseek(port.fd, offset, libc::SEEK_SET) };

                if result < 0 {
                    if errno::errno().0 == libc::EINTR {
                        continue;
                    } else {
                        return raise_exn!(
                            FailFilesystemErrno,
                            &[Value::make_int(errno::errno().0)],
                            "port seek failed"
                        );
                    }
                }

                break;
            }
        }
    } else if port.typ == SCM_PORT_TYPE_CUSTOM {
        let vm = crate::vm::vm();

        let vect = port.handlers;

        vm.apply(
            vect.vector_ref(SCM_PORT_HANDLER_SET_POS),
            &[Value::make_int(mark as i32)],
        )?;
    }

    port.mark = mark;
    port.buf_tail = port.buf;
    port.buf_head = port.buf_tail;
    port.buf_state = SCM_PORT_BUF_STATE_UNSPECIFIED;
    Ok(())
}

fn device_close(mut port: Handle<Port>) -> Result<(), Value> {
    if port.typ == SCM_PORT_TYPE_NAMED_FILE {
        if port.subtype == SCM_PORT_SUBTYPE_NONE {
            let result = unsafe { libc::close(port.fd) };

            if result < 0 {
                return raise_exn!(
                    FailFilesystemErrno,
                    &[Value::make_int(errno::errno().0)],
                    "port close failed"
                );
            }
        }

        port.fd = -1;
    } else {
        let vm = crate::vm::vm();

        let vect = port.handlers;
        let clos = vect.vector_ref(SCM_PORT_HANDLER_CLOSE);
        if clos.procedurep() {
            vm.apply(vect.vector_ref(SCM_PORT_HANDLER_CLOSE), &[])?;
        }
    }

    Ok(())
}

fn init_port_buffer(mut port: Handle<Port>) {
    match port.buffer_mode {
        SCM_PORT_BUFFER_MODE_LINE => {
            port.buf = unsafe { libc::malloc(SCM_PORT_LINE_BUFFER_SIZE).cast() };
            port.buf_size = SCM_PORT_LINE_BUFFER_SIZE;
            port.buf_head = port.buf;
            port.buf_tail = port.buf;
        }

        SCM_PORT_BUFFER_MODE_BLOCK => {
            port.buf = unsafe { libc::malloc(SCM_PORT_BLOCK_BUFFER_SIZE).cast() };
            port.buf_size = SCM_PORT_BLOCK_BUFFER_SIZE;
            port.buf_head = port.buf;
            port.buf_tail = port.buf;
        }

        _ => {
            port.buf = std::ptr::null_mut();
            port.buf_size = 0;
            port.buf_head = port.buf;
            port.buf_tail = port.buf;
            port.buf_state = SCM_PORT_BUF_STATE_UNSPECIFIED;
        }
    }
}

fn init_port_transcoder(mut port: Handle<Port>) {
    let transcoder = port.transcoder;

    if transcoder.byte_vectorp() {
        port.codec = transcoder.byte_vector_ref(0);
        port.eol_style = transcoder.byte_vector_ref(1);
        port.error_handling_mode = transcoder.byte_vector_ref(2);

        if port.codec == SCM_PORT_CODEC_NATIVE
            && port.eol_style == SCM_PORT_EOL_STYLE_NATIVE
            && port.error_handling_mode == SCM_PORT_ERROR_HANDLING_MODE_REPLACE
        {
            port.transcoder = Value::make_true();
        }
    } else {
        port.codec = SCM_PORT_CODEC_NATIVE;
        port.eol_style = SCM_PORT_EOL_STYLE_NATIVE;
        port.error_handling_mode = SCM_PORT_ERROR_HANDLING_MODE_REPLACE;
    }
}

fn init_port_tracking(mut port: Handle<Port>) {
    port.mark = 0;
    port.line = 1;
    port.column = 1;

    port.track_line_column = port.transcoder.is_true();
    port.lookahead_size = 0;
    port.bom_be = false;
    port.bom_le = false;
}

pub fn port_open_std(
    mut port: Handle<Port>,
    fd: i32,
    name: Value,
    direction: u8,
    file_options: u8,
    buffer_mode: u8,
    transocder: Value,
) {
    port.fd = fd;
    port.name = name;
    port.direction = direction;
    port.file_options = file_options;
    port.typ = SCM_PORT_TYPE_NAMED_FILE;
    port.subtype = io_fstat_mode(fd);
    port.handlers = Value::make_false();
    port.bytes = Value::make_false();
    port.buffer_mode = buffer_mode;
    port.transcoder = transocder;

    init_port_tracking(port);
    init_port_transcoder(port);
    init_port_buffer(port);
}

pub fn std_port_position(fd: i32) -> Result<i64, Value> {
    if io_fstat_mode(fd) == SCM_PORT_SUBTYPE_NONE {
        let mark = unsafe { libc::lseek(fd, 0, libc::SEEK_CUR) };

        if mark < 0 {
            return raise_exn!(
                FailFilesystemErrno,
                &[Value::make_int(errno::errno().0)],
                "port seek failed"
            );
        }

        Ok(mark)
    } else {
        Ok(0)
    }
}

pub fn port_sync_port_position(port: Handle<Port>) -> Result<(), Value> {
    if port_has_port_position_pred(port) {
        if port.typ == SCM_PORT_TYPE_NAMED_FILE && port.subtype == SCM_PORT_SUBTYPE_NONE {
            if unsafe { libc::lseek(port.fd, port.mark, libc::SEEK_CUR) } >= 0 {
                return Ok(());
            }

            return raise_exn!(
                FailFilesystemErrno,
                &[Value::make_int(errno::errno().0)],
                "port seek failed"
            );
        }
    }

    Ok(())
}

pub fn port_open_file(
    mut port: Handle<Port>,
    name: Value,
    direction: u8,
    file_options: u8,
    buffer_mode: u8,
    transcoder: Value,
) -> Result<(), Value> {
    port.fd = -1;
    let path = name.strsym();
    port.opened = false;
    port.typ = SCM_PORT_TYPE_NAMED_FILE;
    port.subtype = io_stat_mode(path);
    port.handlers = Value::make_false();
    port.bytes = Value::make_false();
    port.name = name;
    port.direction = direction;
    port.file_options = file_options;
    port.buffer_mode = buffer_mode;
    port.force_sync = false;
    port.transcoder = transcoder;
    let mut options = 0;

    match port.subtype {
        SCM_PORT_SUBTYPE_NONE => {
            match direction {
                SCM_PORT_DIRECTION_IN => {
                    options |= libc::O_RDONLY;
                }
                SCM_PORT_DIRECTION_OUT => {
                    options |= libc::O_WRONLY | libc::O_CREAT | libc::O_TRUNC | libc::O_EXCL;
                }

                SCM_PORT_DIRECTION_BOTH => {
                    options |= libc::O_RDWR | libc::O_CREAT | libc::O_TRUNC | libc::O_EXCL;
                }
                _ => unreachable!(),
            }
            if (file_options & SCM_PORT_FILE_OPTION_NO_CREATE) != 0 {
                options &= !libc::O_CREAT;
            }

            if (file_options & SCM_PORT_FILE_OPTION_NO_TRUNCATE) != 0 {
                options &= !libc::O_TRUNC;
            }

            if (file_options & SCM_PORT_FILE_OPTION_NO_FAIL) != 0 {
                options &= !libc::O_EXCL;
            }
        }

        _ => match direction {
            SCM_PORT_DIRECTION_IN => {
                options |= libc::O_RDONLY;
            }
            SCM_PORT_DIRECTION_OUT => {
                options |= libc::O_WRONLY;
            }

            SCM_PORT_DIRECTION_BOTH => {
                options |= libc::O_RDWR;
            }
            _ => unreachable!(),
        },
    }

    port.fd = io_open(path, options, libc::S_IRUSR as i32 | libc::S_IWUSR as i32);

    if port.fd == -1 {
        return raise_exn!(
            FailFilesystemErrno,
            &[Value::make_int(errno::errno().0)],
            "port open failed"
        );
    }

    init_port_tracking(port);
    init_port_transcoder(port);
    init_port_buffer(port);
    port.opened = true;
    Ok(())
}

pub fn port_open_temp_file(
    mut port: Handle<Port>,
    name: Value,
    buffer_mode: u8,
    transcoder: Value,
) -> Result<(), Value> {
    port.fd = -1;

    port.opened = false;
    port.typ = SCM_PORT_TYPE_NAMED_FILE;
    port.subtype = SCM_PORT_SUBTYPE_NONE;
    port.handlers = Value::make_false();
    port.bytes = Value::make_false();
    port.name = name;
    port.direction = SCM_PORT_DIRECTION_BOTH;
    port.transcoder = transcoder;
    port.buffer_mode = buffer_mode;
    port.file_options = SCM_PORT_FILE_OPTION_NONE;
    port.force_sync = false;

    let mut tmpl = [0u8; 256];
    let tmp = "/tmp/scm_temp_XXXXXX";
    let len = tmp.len();
    tmpl[..len].copy_from_slice(tmp.as_bytes());

    port.fd = unsafe { libc::mkstemp(tmpl.as_mut_ptr() as *mut i8) };

    if port.fd == -1 {
        return raise_exn!(
            FailFilesystemErrno,
            &[Value::make_int(errno::errno().0)],
            "port open failed"
        );
    }

    init_port_tracking(port);
    init_port_transcoder(port);
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
) {
    port.fd = -1;
    port.opened = false;
    port.typ = SCM_PORT_TYPE_BYTEVECTOR;
    port.subtype = SCM_PORT_SUBTYPE_NONE;
    port.handlers = Value::make_false();

    if (port.direction & SCM_PORT_DIRECTION_OUT) != 0 {
        port.bytes = Value::make_false();
    } else {
        port.bytes = bytes;
    }

    port.name = name;
    port.direction = direction;
    port.transcoder = transcoder;
    port.buffer_mode = SCM_PORT_BUFFER_MODE_NONE;
    port.file_options = SCM_PORT_FILE_OPTION_NONE;
    port.force_sync = false;
    init_port_tracking(port);
    init_port_transcoder(port);

    port.buf = null_mut();
    port.buf_size = 0;
    port.buf_tail = port.buf;
    port.buf_head = port.buf;
    port.buf_state = if (port.direction & SCM_PORT_DIRECTION_OUT) != 0 {
        SCM_PORT_BUF_STATE_ACCUMULATE
    } else {
        SCM_PORT_BUF_STATE_UNSPECIFIED
    };

    port.opened = true;
}

pub fn port_make_custom_port(
    mut port: Handle<Port>,
    name: Value,
    direction: u8,
    handlers: Value,
    transcoder: Value,
) {
    port.fd = -1;
    port.opened = false;
    port.typ = SCM_PORT_TYPE_CUSTOM;
    port.subtype = SCM_PORT_SUBTYPE_NONE;
    port.handlers = handlers;
    port.bytes = Value::make_byte_vector(Thread::current(), SCM_PORT_CUSTOM_BUFFER_SIZE as _, 0);
    port.name = name;
    port.direction = direction;
    port.transcoder = transcoder;
    port.buffer_mode = SCM_PORT_BUFFER_MODE_NONE;
    port.file_options = SCM_PORT_FILE_OPTION_NONE;
    port.force_sync = false;

    init_port_tracking(port);
    init_port_transcoder(port);
    init_port_buffer(port);
    port.opened = true;
}

pub fn port_make_transcoder_port(
    mut textual: Handle<Port>,
    name: Value,
    mut binary: Handle<Port>,
    transcoder: Value,
) {
    Thread::current().write_barrier(textual);
    textual.bytes = textual.bytes;
    textual.lookahead.copy_from_slice(&binary.lookahead);
    textual.lookahead_size = binary.lookahead_size;
    textual.buf = binary.buf;
    textual.buf_size = binary.buf_size;
    textual.buf_tail = binary.buf_tail;
    textual.buf_head = binary.buf_head;
    textual.buf_state = binary.buf_state;

    textual.mark = binary.mark;
    textual.line = binary.line;
    textual.column = binary.column;
    textual.fd = binary.fd;
    textual.name = name;
    textual.transcoder = transcoder;
    textual.codec = transcoder.byte_vector_ref(0);
    textual.eol_style = transcoder.byte_vector_ref(1);
    textual.error_handling_mode = transcoder.byte_vector_ref(2);
    textual.file_options = binary.file_options;
    textual.buffer_mode = binary.buffer_mode;
    textual.typ = binary.typ;
    textual.subtype = binary.subtype;
    textual.direction = binary.direction;
    textual.track_line_column = binary.track_line_column;
    textual.opened = binary.opened;
    textual.force_sync = binary.force_sync;

    binary.fd = -1;
    binary.opened = false;
    binary.buf = null_mut();
    binary.buf_size = 0;
    binary.buf_tail = binary.buf;
    binary.buf_head = binary.buf;
    binary.buf_state = SCM_PORT_BUF_STATE_UNSPECIFIED;
}

pub fn port_has_port_position_pred(port: Handle<Port>) -> bool {
    match port.typ {
        SCM_PORT_TYPE_NAMED_FILE => {
            return port.subtype == SCM_PORT_SUBTYPE_NONE;
        }

        SCM_PORT_TYPE_CUSTOM => {
            let vect = port.handlers;

            vect.vector_ref(SCM_PORT_HANDLER_GET_POS).procedurep()
        }

        _ => false,
    }
}

pub fn port_flush_output(mut port: Handle<Port>) -> Result<(), Value> {
    if port.opened {
        if no_output_buffered(port) {
            return Ok(());
        }

        let n = port.buf_tail as usize - port.buf_head as usize;
        unsafe {
            device_write(
                port,
                std::slice::from_raw_parts(port.buf_head, n),
                port.mark - n as i64,
            )?;
        }

        port.buf_tail = port.buf;
        port.buf_head = port.buf_tail;
        port.buf_state = SCM_PORT_BUF_STATE_UNSPECIFIED;
    }

    Ok(())
}

pub fn port_discard_buffer(mut port: Handle<Port>) {
    if !port.buf.is_null() {
        unsafe {
            libc::free(port.buf as *mut libc::c_void);
        }

        port.buf = null_mut();
        port.buf_size = 0;
        port.buf_tail = port.buf;
        port.buf_head = port.buf;
        port.buf_state = SCM_PORT_BUF_STATE_UNSPECIFIED;
    }
}

pub fn port_close(mut port: Handle<Port>) -> Result<(), Value> {
    if port.opened {
        port_flush_output(port)?;
        port_discard_buffer(port);
        device_close(port)?;
        port.opened = false;
    }
    Ok(())
}

pub fn port_nonblock_byte_ready(port: Handle<Port>) -> Result<bool, Value> {
    if port.opened {
        match port.typ {
            SCM_PORT_TYPE_NAMED_FILE => match port.subtype {
                SCM_PORT_SUBTYPE_FIFO | SCM_PORT_SUBTYPE_CHAR_SPECIAL => {
                    if no_input_buffered(port) {
                        let mut tm = libc::timeval {
                            tv_sec: 0,
                            tv_usec: 0,
                        };

                        let mut fds: libc::fd_set = unsafe { MaybeUninit::zeroed().assume_init() };

                        unsafe {
                            libc::FD_ZERO(&mut fds);
                            libc::FD_SET(port.fd, &mut fds);

                            let state = libc::select(
                                port.fd + 1,
                                &mut fds,
                                null_mut(),
                                null_mut(),
                                &mut tm,
                            );

                            if state < 0 {
                                if errno::errno().0 == libc::EINTR {
                                    return Ok(false);
                                } else {
                                    return raise_exn!(
                                        FailFilesystemErrno,
                                        &[Value::make_int(errno::errno().0)],
                                        "port select failed"
                                    );
                                }
                            }

                            return Ok(state > 0);
                        }
                    }
                }
                _ => (),
            },

            _ => (),
        }
    }

    Ok(true)
}

pub fn port_buffered_byte_count(port: Handle<Port>) -> i32 {
    if port.opened {
        match port.typ {
            SCM_PORT_TYPE_NAMED_FILE => {
                if port.buf_state == SCM_PORT_BUF_STATE_READ {
                    return port.buf_tail as i32 - port.buf_head as i32;
                }
                return port.lookahead_size as _;
            }

            SCM_PORT_TYPE_BYTEVECTOR => {
                return port.bytes.byte_vector_len() as i32 - port.mark as i32;
            }

            _ => unreachable!("invalid port type"),
        }
    }

    0
}

pub fn port_eof(port: Handle<Port>) -> Result<bool, Value> {
    if port.opened {
        match port.typ {
            SCM_PORT_TYPE_NAMED_FILE => match port.subtype {
                SCM_PORT_SUBTYPE_NONE => {
                    if no_input_buffered(port) {
                        if let Some(size) = io_fstat_size(port.fd) {
                            return Ok(size <= port.mark);
                        }

                        return raise_exn!(FailFilesystemErrno, &[], "fstat failed");
                    }
                }
                _ => (),
            },
            SCM_PORT_TYPE_BYTEVECTOR => {
                return Ok((port.bytes.byte_vector_len() as i64) <= port.mark);
            }

            _ => (),
        }
        return Ok(false);
    }

    Ok(true)
}

pub fn port_lookahead_byte(mut port: Handle<Port>) -> Result<i32, Value> {
    if port.opened {
        match port.typ {
            SCM_PORT_TYPE_BYTEVECTOR => {
                if port.bytes.byte_vector_len() <= port.mark as usize {
                    return Ok(libc::EOF);
                }

                return Ok(port.bytes.byte_vector_ref(port.mark as usize) as i32);
            }

            _ => {
                if !port.buf.is_null() {
                    if port.buf_state == SCM_PORT_BUF_STATE_READ && port.buf_head != port.buf_tail {
                        return unsafe { Ok(*port.buf_head as i32) };
                    }

                    port_flush_output(port)?;
                    port.buf_state = SCM_PORT_BUF_STATE_READ;
                    port.buf_head = port.buf;
                    port.buf_tail = port.buf;

                    let n = device_read(
                        port,
                        unsafe { std::slice::from_raw_parts_mut(port.buf, port.buf_size) },
                        port.mark,
                    )?;

                    if n == 0 {
                        return Ok(libc::EOF);
                    }

                    port.buf_tail = unsafe { port.buf.add(n as _) };
                    return unsafe { Ok(*port.buf_head as i32) };
                }
            }
        }
    }

    Ok(libc::EOF)
}

fn port_update_line_column(mut port: Handle<Port>, c: i32) {
    if c == b'\n' as i32 {
        port.line += 1;
        port.column = 1;
    } else {
        port.column += 1;
    }
}

pub fn port_get_byte(mut port: Handle<Port>) -> Result<i32, Value> {
    if port.opened {
        match port.typ {
            SCM_PORT_TYPE_BYTEVECTOR => {
                if port.bytes.byte_vector_len() <= port.mark as usize {
                    return Ok(libc::EOF);
                }

                let c = port.bytes.byte_vector_ref(port.mark as usize) as i32;
                if port.track_line_column {
                    port_update_line_column(port, c);
                }
                port.mark += 1;

                return Ok(c);
            }

            _ => {
                if !port.buf.is_null() {
                    if port.buf_state != SCM_PORT_BUF_STATE_READ || port.buf_head == port.buf_tail {
                        port_flush_output(port)?;
                        port.buf_state = SCM_PORT_BUF_STATE_READ;
                        port.buf_head = port.buf;
                        port.buf_tail = port.buf;

                        let n = device_read(
                            port,
                            unsafe { std::slice::from_raw_parts_mut(port.buf, port.buf_size) },
                            port.mark,
                        )?;

                        if n == 0 {
                            return Ok(libc::EOF);
                        }

                        port.buf_tail = unsafe { port.buf.add(n as _) };
                    }

                    let c = unsafe { *port.buf_head };
                    if port.track_line_column {
                        port_update_line_column(port, c as i32);
                    }
                    port.buf_head = unsafe { port.buf_head.add(1) };
                    port.mark += 1;
                    if port.track_line_column {
                        port_update_line_column(port, c as i32);
                    }

                    return Ok(c as i32);
                } else {
                    let mut b = 0;

                    if port.lookahead_size != 0 {
                        b = port.lookahead[0];
                        port.lookahead[0] = port.lookahead[1];
                        port.lookahead[1] = port.lookahead[2];
                        port.lookahead[2] = port.lookahead[3];
                        port.lookahead_size -= 1;
                    } else {
                        let n = device_read(
                            port,
                            unsafe { std::slice::from_raw_parts_mut(&mut b, 1) },
                            port.mark,
                        )?;

                        if n == 0 {
                            return Ok(libc::EOF);
                        }
                    }

                    if port.track_line_column {
                        port_update_line_column(port, b as i32);
                    }

                    port.mark += 1;
                    return Ok(b as i32);
                }
            }
        }
    }

    Ok(libc::EOF)
}

pub fn port_get_bytes(mut port: Handle<Port>, p: &mut [u8]) -> Result<i32, Value> {
    if p.len() == 0 {
        return Ok(0);
    }

    if port.opened {
        match port.typ {
            SCM_PORT_TYPE_BYTEVECTOR => {
                if port.bytes.byte_vector_len() <= port.mark as usize {
                    return Ok(0);
                }

                let available = port.bytes.byte_vector_len() - port.mark as usize;
                let len = p.len().min(available);

                for i in 0..len {
                    p[i] = port.bytes.byte_vector_ref(port.mark as usize + i) as u8;
                }

                if port.track_line_column {
                    for i in 0..len {
                        port_update_line_column(port, p[i] as i32);
                    }
                }

                port.mark += len as i64;

                return Ok(len as i32);
            }

            _ => {
                if !port.buf.is_null() {
                    if port.buf_state != SCM_PORT_BUF_STATE_READ || port.buf_head == port.buf_tail {
                        port_flush_output(port)?;
                        port.buf_state = SCM_PORT_BUF_STATE_READ;
                        port.buf_head = port.buf;
                        port.buf_tail = port.buf;

                        let n = device_read(
                            port,
                            unsafe { std::slice::from_raw_parts_mut(port.buf, port.buf_size) },
                            port.mark,
                        )?;

                        if n == 0 {
                            return Ok(0);
                        }

                        port.buf_tail = unsafe { port.buf.add(n as _) };
                    }

                    let available = (port.buf_tail as usize - port.buf_head as usize) as usize;
                    let len = p.len().min(available);

                    for i in 0..len {
                        p[i] = unsafe { *port.buf_head.add(i) };
                    }

                    if port.track_line_column {
                        for i in 0..len {
                            port_update_line_column(port, p[i] as i32);
                        }
                    }

                    port.buf_head = unsafe { port.buf_head.add(len as _) };
                    port.mark += len as i64;
                    if len == p.len() {
                        return Ok(p.len() as _);
                    }
                    return Ok(len as i32 + port_get_bytes(port, &mut p[len..])?);
                } else {
                    if port.lookahead_size != 0 {
                        let b = port.lookahead[0];
                        port.lookahead[0] = port.lookahead[1];
                        port.lookahead[1] = port.lookahead[2];
                        port.lookahead[2] = port.lookahead[3];
                        port.lookahead_size -= 1;

                        p[0] = b;

                        if port.track_line_column {
                            port_update_line_column(port, b as i32);
                        }
                        port.mark += 1;

                        if p.len() == 1 {
                            return Ok(1);
                        }

                        return Ok(1 + port_get_bytes(port, &mut p[1..])?);
                    } else {
                        let n = device_read(
                            port,
                            unsafe { std::slice::from_raw_parts_mut(p.as_mut_ptr(), p.len()) },
                            port.mark,
                        )?;

                        if n == 0 {
                            return Ok(0);
                        }

                        if port.track_line_column {
                            for i in 0..n {
                                port_update_line_column(port, p[i as usize] as i32);
                            }
                        }

                        port.mark += n as i64;
                        return Ok(n as i32);
                    }
                }
            }
        }
    }

    Ok(0)
}

pub fn port_lookahead_utf8(mut port: Handle<Port>) -> Result<Value, Value> {
    let hit_eof = |mut port: Handle<Port>| -> Result<Value, Value> {
        match port.error_handling_mode {
            SCM_PORT_ERROR_HANDLING_MODE_IGNORE => return Ok(Value::make_eof()),

            SCM_PORT_ERROR_HANDLING_MODE_REPLACE => {
                if port.lookahead_size != 0 {
                    port.lookahead_size =
                        cnvt_ucs4_to_utf8(SCM_PORT_UCS4_REPLACEMENT_CHAR, &mut port.lookahead);
                }
                return Ok(Value::make_char_cached(char::from_u32(0xFFFD).unwrap()));
            }

            _ => {
                let srcloc = make_srcloc(port.name, port.line, port.column, port.mark as _);

                raise_exn!(
                    FailReadNonChar,
                    &[srcloc],
                    "encountered invalid utf-8 sequence"
                )
            }
        }
    };

    if port.opened {
        'top: loop {
            let mut utf8 = [0u8; 6];

            let b = port_lookahead_byte(port)?;
            if b == libc::EOF {
                return Ok(Value::make_eof());
            }

            if b > 127 {
                let code_length = utf8_byte_count(b as _);
                utf8[0] = b as _;

                if code_length > 1 {
                    match port.typ {
                        SCM_PORT_TYPE_BYTEVECTOR => {
                            if port.bytes.byte_vector_len() as i64 - port.mark < code_length as i64
                            {
                                return hit_eof(port);
                            }

                            unsafe {
                                libc::memcpy(
                                    utf8.as_mut_ptr().cast(),
                                    port.bytes
                                        .byte_vector_as_slice()
                                        .as_ptr()
                                        .add(port.mark as _)
                                        .cast(),
                                    code_length,
                                );
                            }
                        }

                        SCM_PORT_TYPE_NAMED_FILE => {
                            if !port.buf.is_null() {
                                let mut n1 = port.buf_tail as usize - port.buf_head as usize;

                                while n1 < code_length {
                                    unsafe {
                                        if port.buf != port.buf_head {
                                            libc::memmove(
                                                port.buf.cast(),
                                                port.buf_head.cast(),
                                                n1,
                                            );
                                            port.buf_head = port.buf;
                                        }

                                        let n2 = device_read(
                                            port,
                                            std::slice::from_raw_parts_mut(
                                                port.buf.add(n1),
                                                port.buf_size - n1,
                                            ),
                                            port.mark + n1 as i64,
                                        )?;

                                        n1 = n1 + n2 as usize;

                                        port.buf_tail = port.buf_head.add(n1);

                                        if n2 == 0 {
                                            return hit_eof(port);
                                        }
                                    }
                                }

                                unsafe {
                                    libc::memcpy(
                                        utf8.as_mut_ptr().cast(),
                                        port.buf_head.cast(),
                                        code_length,
                                    );
                                }
                            } else {
                                while port.lookahead_size < code_length {
                                    let p = port;
                                    let n = device_read(
                                        p,
                                        &mut port.lookahead[p.lookahead_size..],
                                        p.mark + p.lookahead_size as i64,
                                    )?;

                                    port.lookahead_size += n as usize;

                                    if n == 0 {
                                        return hit_eof(port);
                                    }
                                }

                                unsafe {
                                    libc::memcpy(
                                        utf8.as_mut_ptr().cast(),
                                        port.lookahead.as_mut_ptr().cast(),
                                        code_length,
                                    );
                                }
                            }
                        }

                        _ => unreachable!(),
                    }
                }

                let mut ucs4 = 0;

                if cnvt_utf8_to_ucs4(&utf8, &mut ucs4) < 1 {
                    match port.error_handling_mode {
                        SCM_PORT_ERROR_HANDLING_MODE_IGNORE => {
                            for _ in 0..code_length {
                                port_get_byte(port)?;
                            }

                            continue 'top;
                        }

                        SCM_PORT_ERROR_HANDLING_MODE_RAISE => {
                            let srcloc =
                                make_srcloc(port.name, port.line, port.column, port.mark as _);
                            return raise_exn!(
                                FailReadNonChar,
                                &[srcloc],
                                "encountered invalid utf-8 sequence"
                            );
                        }

                        SCM_PORT_ERROR_HANDLING_MODE_REPLACE => {
                            if port.lookahead_size != 0 {
                                port.lookahead_size = cnvt_ucs4_to_utf8(
                                    SCM_PORT_UCS4_REPLACEMENT_CHAR,
                                    &mut port.lookahead,
                                );
                            }

                            ucs4 = SCM_PORT_UCS4_REPLACEMENT_CHAR;
                        }
                        _ => unreachable!(),
                    }
                }

                if port.mark == 0 && ucs4 == SCM_PORT_UCS4_BOM {
                    port_get_byte(port)?;
                    port_get_byte(port)?;
                    port_get_byte(port)?;
                    continue 'top;
                }

                return Ok(Value::make_char_cached(char::from_u32(ucs4).unwrap()));
            }

            return Ok(Value::make_char_cached(b as u8 as char));
        }
    }

    Ok(Value::make_eof())
}

pub fn port_get_utf8(port: Handle<Port>) -> Result<Value, Value> {
    if port.opened {
        'top: loop {
            let mut utf8 = [0u8; 6];

            let b = port_get_byte(port)?;

            if b == libc::EOF {
                return Ok(Value::make_eof());
            }

            if b > 127 {
                let code_length = utf8_byte_count(b as _);

                utf8[0] = b as _;

                for i in 1..code_length {
                    let b = port_get_byte(port)?;

                    if b == libc::EOF {
                        match port.error_handling_mode {
                            SCM_PORT_ERROR_HANDLING_MODE_IGNORE => return Ok(Value::make_eof()),
                            SCM_PORT_ERROR_HANDLING_MODE_REPLACE => {
                                return Ok(Value::make_char_cached(
                                    char::from_u32(0xFFFD).unwrap(),
                                ));
                            }
                            _ => {
                                let srcloc =
                                    make_srcloc(port.name, port.line, port.column, port.mark as _);

                                return raise_exn!(
                                    FailReadNonChar,
                                    &[srcloc],
                                    "encountered invalid utf-8 sequence"
                                );
                            }
                        }
                    }

                    utf8[i] = b as _;
                }

                let mut ucs4 = 0;

                if cnvt_utf8_to_ucs4(&utf8, &mut ucs4) < 1 {
                    match port.error_handling_mode {
                        SCM_PORT_ERROR_HANDLING_MODE_IGNORE => {
                            continue 'top;
                        }

                        SCM_PORT_ERROR_HANDLING_MODE_RAISE => {
                            let srcloc =
                                make_srcloc(port.name, port.line, port.column, port.mark as _);
                            return raise_exn!(
                                FailReadNonChar,
                                &[srcloc],
                                "encountered invalid utf-8 sequence"
                            );
                        }

                        SCM_PORT_ERROR_HANDLING_MODE_REPLACE => {
                            ucs4 = SCM_PORT_UCS4_REPLACEMENT_CHAR;
                        }
                        _ => unreachable!(),
                    }
                }

                if port.mark == 3 && ucs4 == SCM_PORT_UCS4_BOM {
                    continue 'top;
                }

                return Ok(Value::make_char_cached(char::from_u32(ucs4).unwrap()));
            }

            return Ok(Value::make_char_cached(b as u8 as char));
        }
    }

    Ok(Value::make_eof())
}

pub fn port_set_mark(mut port: Handle<Port>, offset: i64) -> Result<(), Value> {
    if port.opened {
        port_flush_output(port)?;
        port.lookahead_size = 0;

        match port.typ {
            SCM_PORT_TYPE_BYTEVECTOR => {
                port.mark = offset;
            }
            _ => {
                device_set_mark(port, offset)?;
            }
        }

        port.track_line_column = false;
        port.line = 0;
        port.column = 0;
    }
    Ok(())
}

macro_rules! hundred_twenty_five_percent_of {
    ($n: expr) => {
        $n + ($n >> 2)
    };
}

pub fn port_put_byte(mut port: Handle<Port>, byte: u8) -> Result<(), Value> {
    if port.opened {
        port.lookahead_size = 0;

        if port.track_line_column {
            port_update_line_column(port, byte as _);
        }

        match port.typ {
            SCM_PORT_TYPE_BYTEVECTOR => {
                if port.mark >= port.buf_size as i64 {
                    let mut newsize = port.mark as usize + SCM_PORT_BYTEVECTOR_OUTPUT_CHUNK;
                    let cursize = port.buf_tail as usize - port.buf_head as usize;

                    if newsize < hundred_twenty_five_percent_of!(cursize) {
                        newsize = hundred_twenty_five_percent_of!(cursize);
                    }

                    if newsize < port.mark as usize + SCM_PORT_BYTEVECTOR_OUTPUT_CHUNK {
                        return raise_exn!(
                            FailOutOfMemory,
                            &[],
                            "port bytevector too large, memory allocation failed"
                        );
                    }

                    let prev = port.buf;
                    port.buf = unsafe { libc::realloc(port.buf.cast(), newsize) as *mut u8 };

                    if port.buf.is_null() {
                        port.buf = prev;
                        return raise_exn!(
                            FailOutOfMemory,
                            &[],
                            "port bytevector too large, memory allocation failed"
                        );
                    }

                    unsafe {
                        libc::memset(port.buf.add(cursize).cast(), 0, newsize - cursize);
                        port.buf_head = port.buf;
                        port.buf_tail = port.buf.add(cursize);
                        port.buf_size = newsize;
                    }
                }

                unsafe {
                    let mut p = port.buf.add(port.mark as usize);
                    *p = byte;
                    port.mark += 1;
                    p = p.add(1);
                    if p > port.buf_tail {
                        port.buf_tail = p;
                    }
                }
            }

            _ => unsafe {
                if !port.buf.is_null() {
                    if port.buf_state != SCM_PORT_BUF_STATE_WRITE {
                        port.buf_state = SCM_PORT_BUF_STATE_WRITE;
                        port.buf_head = port.buf;
                        port.buf_tail = port.buf;
                    } else {
                        if port.buf_tail == port.buf.add(port.buf_size) {
                            port_flush_output(port)?;
                            port.buf_state = SCM_PORT_BUF_STATE_WRITE;
                        }
                    }

                    *port.buf_tail = byte;
                    port.buf_tail = port.buf_tail.add(1);
                    port.mark += 1;

                    if port.buffer_mode == SCM_PORT_BUFFER_MODE_LINE {
                        if byte == b'\n' {
                            port_flush_output(port)?;
                        }
                    }
                } else {
                    device_write(port, &[byte], port.mark)?;
                    port.mark += 1;
                }
            },
        }
    }
    Ok(())
}

pub fn port_put_bytes(mut port: Handle<Port>, p: &[u8]) -> Result<(), Value> {
    if port.opened {
        port.lookahead_size = 0;

        match port.typ {
            SCM_PORT_TYPE_BYTEVECTOR => {
                if port.mark >= port.buf_size as i64 {
                    let mut newsize = port.mark as usize + SCM_PORT_BYTEVECTOR_OUTPUT_CHUNK;
                    let cursize = port.buf_tail as usize - port.buf_head as usize;

                    if newsize < hundred_twenty_five_percent_of!(cursize) {
                        newsize = hundred_twenty_five_percent_of!(cursize);
                    }

                    if newsize < port.mark as usize + SCM_PORT_BYTEVECTOR_OUTPUT_CHUNK {
                        return raise_exn!(
                            FailOutOfMemory,
                            &[],
                            "port bytevector too large, memory allocation failed"
                        );
                    }

                    let prev = port.buf;
                    port.buf = unsafe { libc::realloc(port.buf.cast(), newsize) as *mut u8 };

                    if port.buf.is_null() {
                        port.buf = prev;
                        return raise_exn!(
                            FailOutOfMemory,
                            &[],
                            "port bytevector too large, memory allocation failed"
                        );
                    }

                    unsafe {
                        libc::memset(port.buf.add(cursize).cast(), 0, newsize - cursize);
                        port.buf_head = port.buf;
                        port.buf_tail = port.buf.add(cursize);
                        port.buf_size = newsize;
                    }
                }

                let space = port.buf_size as i64 - port.mark;
                let n = if space > p.len() as i64 {
                    p.len()
                } else {
                    space as usize
                };

                unsafe {
                    libc::memcpy(
                        port.buf.add(port.mark as usize).cast(),
                        p.as_ptr().cast(),
                        n,
                    );
                    port.mark += n as i64;

                    if port.buf_head.add(port.mark as _) > port.buf_tail {
                        port.buf_tail = port.buf_head.add(port.mark as _);
                    }

                    if port.track_line_column {
                        for i in 0..n {
                            port_update_line_column(port, p[i] as _);
                        }
                    }

                    if n != p.len() {
                        return port_put_bytes(port, &p[n..]);
                    }
                }
            }

            _ => unsafe {
                if !port.buf.is_null() {
                    if port.buf_state != SCM_PORT_BUF_STATE_WRITE {
                        port.buf_state = SCM_PORT_BUF_STATE_WRITE;
                        port.buf_head = port.buf;
                        port.buf_tail = port.buf;
                    } else {
                        if port.buf_tail == port.buf.add(port.buf_size) {
                            port_flush_output(port)?;
                            port.buf_state = SCM_PORT_BUF_STATE_WRITE;
                        }
                    }

                    let space = port.buf_size - (port.buf_tail as usize - port.buf as usize);
                    let n = space.min(p.len());

                    libc::memcpy(port.buf_tail.cast(), p.as_ptr().cast(), n);
                    port.buf_tail = port.buf_tail.add(n);
                    port.mark += n as i64;

                    if port.track_line_column {
                        for i in 0..n {
                            port_update_line_column(port, p[i] as _);
                        }
                    }

                    if n == p.len() && port.buffer_mode == SCM_PORT_BUFFER_MODE_LINE {
                        if p.contains(&b'\n') {
                            port_flush_output(port)?;
                        }
                    }

                    if n != p.len() {
                        return port_put_bytes(port, &p[n..]);
                    }
                } else {
                    device_write(port, p, port.mark)?;
                    port.mark += p.len() as i64;

                    if port.track_line_column {
                        for i in 0..p.len() {
                            port_update_line_column(port, p[i] as _);
                        }
                    }
                }
            },
        }
    }

    Ok(())
}

pub fn port_put_utf8(port: Handle<Port>, ucs4: u32) -> Result<(), Value> {
    let mut utf8 = [0u8; 4];
    let n = cnvt_ucs4_to_utf8(ucs4, &mut utf8);

    port_put_bytes(port, &utf8[..n])
}

pub fn port_input_pred(port: Handle<Port>) -> bool {
    (port.direction & SCM_PORT_DIRECTION_IN) != 0
}

pub fn port_output_pred(port: Handle<Port>) -> bool {
    (port.direction & SCM_PORT_DIRECTION_OUT) != 0
}

pub fn port_textual_pred(port: Handle<Port>) -> bool {
    port.transcoder.is_true()
}

pub fn port_binary_pred(port: Handle<Port>) -> bool {
    port.transcoder.falsep()
}

pub fn port_open_pred(port: Handle<Port>) -> bool {
    port.opened
}

pub fn port_bytevector_pred(port: Handle<Port>) -> bool {
    port.typ == SCM_PORT_TYPE_BYTEVECTOR
}

pub fn port_regular_file_pred(port: Handle<Port>) -> bool {
    port.typ == SCM_PORT_TYPE_NAMED_FILE && port.subtype == SCM_PORT_SUBTYPE_NONE
}

pub fn port_get_bytevector(port: Handle<Port>) -> Value {
    let n = port.buf_tail as usize - port.buf_head as usize;

    if port.buf.is_null() {
        Value::make_byte_vector(Thread::current(), 0, 0)
    } else {
        let bv = Value::make_byte_vector(Thread::current(), n as _, 0);

        unsafe {
            libc::memcpy(
                bv.byte_vector_as_slice_mut().as_mut_ptr().cast(),
                port.buf_head.cast(),
                n,
            );
        }

        bv
    }
}

pub fn port_extract_bytevector(mut port: Handle<Port>) -> Value {
    let n = port.buf_tail as usize - port.buf_head as usize;

    if port.buf.is_null() {
        Value::make_byte_vector(Thread::current(), 0, 0)
    } else {
        let bv = Value::make_byte_vector(Thread::current(), n as _, 0);

        unsafe {
            libc::memcpy(
                bv.byte_vector_as_slice_mut().as_mut_ptr().cast(),
                port.buf_head.cast(),
                n,
            );
            libc::free(port.buf.cast());
        }

        port.buf = std::ptr::null_mut();
        port.buf_head = std::ptr::null_mut();
        port.buf_tail = std::ptr::null_mut();
        port.buf_size = 0;
        port.mark = 0;

        bv
    }
}

pub fn port_get_string(port: Handle<Port>) -> Result<Value, Value> {
    if port.buf.is_null() {
        Ok(Value::make_string(Thread::current(), ""))
    } else {
        let s = match std::str::from_utf8(unsafe {
            std::slice::from_raw_parts(
                port.buf_head,
                port.buf_tail as usize - port.buf_head as usize,
            )
        }) {
            Ok(s) => s,
            Err(e) => {
                return raise_exn!(Fail, &[], "port-get-string: invalid UTF-8 sequence: {}", e);
            }
        };
        let s = Value::make_string(Thread::current(), s);

        Ok(s)
    }
}

pub fn port_extract_string(mut port: Handle<Port>) -> Result<Value, Value> {
    let s = port_get_string(port)?;

    unsafe {
        libc::free(port.buf.cast());
    }

    port.buf = std::ptr::null_mut();
    port.buf_head = std::ptr::null_mut();
    port.buf_tail = std::ptr::null_mut();
    port.buf_size = 0;
    port.mark = 0;

    Ok(s)
}

fn port_lookahead_ch(port: Handle<Port>) -> Result<Value, Value> {
    match port.codec {
        SCM_PORT_CODEC_UTF8 => port_lookahead_utf8(port),
        SCM_PORT_CODEC_LATIN1 => port_lookahead_byte(port).map(|x| {
            if x == -1 {
                Value::make_eof()
            } else {
                Value::make_char_cached(x as u8 as _)
            }
        }),
        _ => todo!("UTF-16 codec"),
    }
}

pub fn port_lookahead_char(port: Handle<Port>) -> Result<Value, Value> {
    let ch = port_lookahead_ch(port)?;

    if ch.eofp() {
        return Ok(ch);
    }

    if port.transcoder.falsep() {
        return Ok(ch);
    }

    if port.eol_style == SCM_PORT_EOL_STYLE_NONE {
        return Ok(ch);
    }

    match *ch.char_val() as u32 {
        SCM_PORT_UCS4_CR | SCM_PORT_UCS4_LS | SCM_PORT_UCS4_NEL => {
            Ok(Value::make_char_cached('\n'))
        }
        _ => Ok(ch),
    }
}

fn port_get_ch(port: Handle<Port>) -> Result<Value, Value> {
    match port.codec {
        SCM_PORT_CODEC_UTF8 => port_get_utf8(port),
        SCM_PORT_CODEC_LATIN1 => port_get_byte(port).map(|x| {
            if x == -1 {
                Value::make_eof()
            } else {
                Value::make_char_cached(x as u8 as _)
            }
        }),
        _ => todo!("UTF-16 codec"),
    }
}

pub fn port_get_char(port: Handle<Port>) -> Result<Value, Value> {
    let ch = port_get_ch(port)?;

    if ch.eofp() {
        return Ok(ch);
    }

    if port.transcoder.falsep() {
        return Ok(ch);
    }

    if port.eol_style == SCM_PORT_EOL_STYLE_NONE {
        return Ok(ch);
    }

    match *ch.char_val() as u32 {
        SCM_PORT_UCS4_CR | SCM_PORT_UCS4_LS | SCM_PORT_UCS4_NEL => {
            Ok(Value::make_char_cached('\n'))
        }
        _ => Ok(ch),
    }
}

pub fn port_has_set_port_position_pred(port: Handle<Port>) -> bool {
    match port.typ {
        SCM_PORT_TYPE_NAMED_FILE => {
            return port.subtype == SCM_PORT_SUBTYPE_NONE;
        }

        SCM_PORT_TYPE_CUSTOM => {
            let vect = port.handlers;

            vect.vector_ref(SCM_PORT_HANDLER_SET_POS).procedurep()
        }

        _ => false,
    }
}

pub fn port_lookahead_u8(port: Handle<Port>) -> Result<Value, Value> {
    let ch = port_lookahead_byte(port)?;

    if ch == -1 {
        Ok(Value::make_eof())
    } else {
        Ok(Value::make_int(ch as _))
    }
}

pub fn port_get_u8(port: Handle<Port>) -> Result<Value, Value> {
    let ch = port_get_byte(port)?;

    if ch == -1 {
        Ok(Value::make_eof())
    } else {
        Ok(Value::make_int(ch as _))
    }
}

pub fn port_position(port: Handle<Port>) -> Result<i64, Value> {
    if port.typ == SCM_PORT_TYPE_CUSTOM {
        let vect = port.handlers;

        if vect.vector_ref(SCM_PORT_HANDLER_TEXTUAL).truep() {
            let vm = crate::vm::vm();
            let token = Value::make_int(port.mark as _);

            let _ = vm.apply(vect.vector_ref(SCM_PORT_HANDLER_GET_POS), &[token])?;
        }
    }

    Ok(port.mark)
}

pub fn port_set_port_position(port: Handle<Port>, pos: i64) -> Result<(), Value> {
    port_set_mark(port, pos)?;
    Ok(())
}

pub fn port_output_buffer_mode(port: Handle<Port>) -> u8 {
    port.buffer_mode
}

fn port_putc(port: Handle<Port>, ucs4: u32) -> Result<(), Value> {
    match port.codec {
        SCM_PORT_CODEC_UTF8 => {
            if ucs4 == SCM_PORT_UCS4_LF {
                if !port.transcoder.falsep() {
                    match port.eol_style {
                        SCM_PORT_EOL_STYLE_CR => {
                            port_putc(port, SCM_PORT_UCS4_CR)?;
                            return Ok(());
                        }

                        SCM_PORT_EOL_STYLE_CRLF => {
                            port_putc(port, SCM_PORT_UCS4_CR)?;
                            port_putc(port, SCM_PORT_UCS4_LF)?;
                            return Ok(());
                        }
                        SCM_PORT_EOL_STYLE_NEL => {
                            port_putc(port, SCM_PORT_UCS4_NEL)?;
                            return Ok(());
                        }

                        SCM_PORT_EOL_STYLE_CRNEL => {
                            port_putc(port, SCM_PORT_UCS4_CR)?;
                            port_putc(port, SCM_PORT_UCS4_NEL)?;
                            return Ok(());
                        }

                        SCM_PORT_EOL_STYLE_LS => {
                            port_putc(port, SCM_PORT_UCS4_LS)?;
                            return Ok(());
                        }
                        _ => unreachable!(),
                    }
                }
            }

            port_put_utf8(port, ucs4)?;
            return Ok(());
        }

        SCM_PORT_CODEC_LATIN1 => {
            if ucs4 <= 0xff {
                if ucs4 == SCM_PORT_UCS4_LF {
                    if !port.transcoder.falsep() {
                        match port.eol_style {
                            SCM_PORT_EOL_STYLE_CR => {
                                port_putc(port, SCM_PORT_UCS4_CR)?;
                                return Ok(());
                            }

                            SCM_PORT_EOL_STYLE_CRLF => {
                                port_putc(port, SCM_PORT_UCS4_CR)?;
                                port_putc(port, SCM_PORT_UCS4_LF)?;
                                return Ok(());
                            }

                            _ => {}
                        }
                    }
                }

                port_put_byte(port, ucs4 as u8)?;
                return Ok(());
            }

            match port.error_handling_mode {
                SCM_PORT_ERROR_HANDLING_MODE_IGNORE => return Ok(()),

                SCM_PORT_ERROR_HANDLING_MODE_REPLACE => {
                    port_put_byte(port, SCM_PORT_BYTE_REPLACEMENT_CHAR)?;
                    return Ok(());
                }

                SCM_PORT_ERROR_HANDLING_MODE_RAISE => {
                    return raise_exn!(
                        Fail,
                        &[],
                        "port encountered a character it cannot encode: {:x}",
                        ucs4
                    )
                }

                _ => todo!(),
            }
        }

        _ => todo!(),
    }
}

pub fn port_put_char(port: Handle<Port>, ch: char) -> Result<(), Value> {
    port_putc(port, ch as u32)?;

    if port.typ == SCM_PORT_TYPE_CUSTOM {
        if port.buf_tail > unsafe { port.buf_head.add(port.buf_size - 4) } {
            port_flush_output(port)?;
        }
    }
    Ok(())
}

pub fn port_puts(port: Handle<Port>, s: &str) -> Result<(), Value> {
    port_put_bytes(port, s.as_bytes())?;

    if port.typ == SCM_PORT_TYPE_CUSTOM {
        if port.handlers.vector_ref(SCM_PORT_HANDLER_TEXTUAL).truep() {
            port_flush_output(port)?;
        }
    }
    Ok(())
}

pub fn port_put_string(port: Handle<Port>, s: Handle<Str>) -> Result<(), Value> {
    if port.transcoder.truep() || port.transcoder.falsep() {
        return port_puts(port, s.str());
    } else {
        let mut buf = s.str().as_bytes();
        let mut bytes;
        loop {
            let mut ucs4 = 0;

            if buf[0] == 0 {
                if port.typ == SCM_PORT_TYPE_CUSTOM {
                    if port.handlers.vector_ref(SCM_PORT_HANDLER_TEXTUAL).truep() {
                        port_flush_output(port)?;
                    }
                }
                return Ok(());
            }

            bytes = cnvt_utf8_to_ucs4(&buf, &mut ucs4);

            if bytes > 0 {
                port_putc(port, ucs4)?;
                buf = &buf[bytes as usize..];
                if buf.is_empty() {
                    if port.typ == SCM_PORT_TYPE_CUSTOM {
                        if port.handlers.vector_ref(SCM_PORT_HANDLER_TEXTUAL).truep() {
                            port_flush_output(port)?;
                        }
                    }
                    return Ok(());
                }
            } else {
                raise_exn!(
                    Fail,
                    &[],
                    "port encountered a character it cannot decode: {:x}",
                    buf[0]
                )?;
            }
        }
    }
}

pub fn cnvt_ucs4_to_utf8(ucs4: u32, utf8: &mut [u8]) -> usize {
    if ucs4 < 0x80 {
        utf8[0] = ucs4 as u8;
        return 1;
    }

    if ucs4 < 0x800 {
        utf8[0] = 0xC0 | ((ucs4 >> 6) as u8);
        utf8[1] = 0x80 | ((ucs4 & 0x3F) as u8);
        return 2;
    }

    if ucs4 < 0x10000 {
        utf8[0] = 0xE0 | ((ucs4 >> 12) as u8);
        utf8[1] = 0x80 | ((ucs4 >> 6 & 0x3F) as u8);
        utf8[2] = 0x80 | ((ucs4 & 0x3F) as u8);
        return 3;
    }

    if ucs4 < 0x200000 {
        utf8[0] = (((ucs4 as u32 >> 18) & 0x07) | 0xf0) as u8;
        utf8[1] = (((ucs4 as u32 >> 12) & 0x3f) | 0x80) as u8;
        utf8[2] = (((ucs4 as u32 >> 6) & 0x3f) | 0x80) as u8;
        utf8[3] = (((ucs4 as u32) & 0x3f) | 0x80) as u8;
        return 4;
    }

    return 0;
}

pub fn utf8_byte_count(datum: u8) -> usize {
    if datum < 0x80 {
        return 1;
    };
    if datum < 0xc2 {
        return 1;
    }; // cnvt_utf8_to_ucs4() detect this
    if datum < 0xe0 {
        return 2;
    };
    if datum < 0xf0 {
        return 3;
    };
    if datum < 0xf8 {
        return 4;
    };
    if datum < 0xfc {
        return 5;
    };
    return 6;
}

pub fn utf8_sizeof_ucs4(ucs4: u32) -> usize {
    if ucs4 < 0x80 {
        return 1;
    }

    if ucs4 < 0x800 {
        return 2;
    }

    if ucs4 < 0x10000 {
        return 3;
    }

    if ucs4 < 0x200000 {
        return 4;
    }

    return 0;
}

pub fn cnvt_utf8_to_ucs4(utf8: &[u8], ucs4: &mut u32) -> i32 {
    let sv;

    if utf8[0] < 0x80 {
        sv = utf8[0] as u32;
        *ucs4 = sv;
        return 1;
    } else if utf8[0] < 0xc2 {
        return -1;
    } else if utf8[0] < 0xe0 {
        if (utf8[1] < 0x80) | (utf8[1] > 0xbf) {
            return -1;
        };
        sv = ((utf8[0] as u32 & 0x1f) << 6) + (utf8[1] as u32 & 0x3f);
        if (sv < 0x80) | (sv > 0x7FF) {
            return -1;
        }; // invalid sequence
        *ucs4 = sv;
        return 2;
    } else if utf8[0] < 0xf0 {
        if (utf8[1] < 0x80) | (utf8[1] > 0xbf) {
            return -1;
        };
        if (utf8[2] < 0x80) | (utf8[2] > 0xbf) {
            return -1;
        };
        sv = ((utf8[0] as u32 & 0x0f) << 12)
            + ((utf8[1] as u32 & 0x3f) << 6)
            + (utf8[2] as u32 & 0x3f);
        if (sv < 0x800) | (sv > 0xFFFF) {
            return -1;
        }; // invalid sequence
        if (sv >= 0xD800) & (sv <= 0xDFFF) {
            return -1;
        }; // SURROGATE AREA
           // if (sv >= 0xFFFE) return -1;                     // NONCHARACTERS
        *ucs4 = sv;
        return 3;
    } else if utf8[0] < 0xf8 {
        if (utf8[1] < 0x80) | (utf8[1] > 0xbf) {
            return -1;
        };
        if (utf8[2] < 0x80) | (utf8[2] > 0xbf) {
            return -1;
        };
        if (utf8[3] < 0x80) | (utf8[3] > 0xbf) {
            return -1;
        };
        sv = ((utf8[0] as u32 & 0x07) << 18)
            + ((utf8[1] as u32 & 0x3f) << 12)
            + ((utf8[2] as u32 & 0x3f) << 6)
            + (utf8[3] as u32 & 0x3f);
        if (sv < 0x10000) | (sv > 0x10FFFF) {
            return -1;
        }; // non-assignment
        *ucs4 = sv;
        return 4;
    }
    return -1;
}

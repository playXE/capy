use super::port::{cstr, IoError, Operation, SocketOperation};
use rsgc::prelude::{Allocation, Handle, Object};
use std::{
    ffi::CStr,
    mem::{size_of, MaybeUninit},
    ptr::{copy_nonoverlapping, null_mut},
};

pub struct Socket {
    addr: libc::sockaddr_storage,
    mode: SocketMode,
    fd: i32,
    family: i32,
    socktype: i32,
    protocol: i32,
    addrlen: u32,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub enum SocketMode {
    None,
    Client,
    Server,
}

impl Object for Socket {}
impl Allocation for Socket {}

impl Socket {
    pub fn open(
        mut self: Handle<Socket>,
        node: &str,
        service: &str,
        family: i32,
        typ: i32,
        flags: i32,
        protocol: i32,
    ) -> Result<(), IoError> {
        unsafe {
            let mut hints: libc::addrinfo = MaybeUninit::zeroed().assume_init();
            let mut list: *mut libc::addrinfo = null_mut();

            hints.ai_family = family;
            hints.ai_socktype = typ;
            hints.ai_flags = flags;
            hints.ai_protocol = protocol;
            hints.ai_canonname = null_mut();
            hints.ai_addr = null_mut();
            hints.ai_next = null_mut();

            let mut retval;
            loop {
                retval = libc::getaddrinfo(
                    cstr(node).as_ptr(),
                    cstr(service).as_ptr(),
                    &hints,
                    &mut list,
                );
                if retval != libc::EAI_AGAIN {
                    break;
                }
            }

            if retval != 0 {
                libc::freeaddrinfo(list);
                if retval == libc::EAI_SYSTEM {
                    return Err(IoError::Code(
                        Operation::Socket(SocketOperation::Open),
                        errno::errno().0,
                    ));
                } else {
                    let str = libc::gai_strerror(retval);
                    let cstr = CStr::from_ptr(str).to_str().unwrap();
                    return Err(IoError::Message(
                        Operation::Socket(SocketOperation::Open),
                        cstr.to_string(),
                    ));
                }
            }

            let mut first_error = 0;

            if flags & libc::AI_PASSIVE != 0 {
                let mut p = list;
                while !p.is_null() {
                    let fd = libc::socket((*p).ai_family, (*p).ai_socktype, (*p).ai_protocol);

                    if fd == -1 {
                        if first_error == 0 {
                            first_error = errno::errno().0;
                        }
                        p = (*p).ai_next;
                        continue;
                    }

                    self.mode = SocketMode::Server;
                    self.fd = fd;
                    self.family = (*p).ai_family;
                    self.socktype = (*p).ai_socktype;
                    self.protocol = (*p).ai_protocol;
                    self.addrlen = (*p).ai_addrlen;
                    copy_nonoverlapping(
                        (*p).ai_addr.cast::<u8>(),
                        &mut self.addr as *mut _ as *mut u8,
                        (*p).ai_addrlen as _,
                    );

                    let one = 1;

                    if libc::setsockopt(
                        fd,
                        libc::SOL_SOCKET,
                        libc::SO_REUSEADDR,
                        &one as *const i32 as _,
                        0,
                    ) == 0
                    {
                        if libc::bind(
                            fd,
                            &self.addr as *const _ as *const libc::sockaddr,
                            self.addrlen as _,
                        ) == 0
                        {
                            if self.socktype == libc::SOCK_STREAM {
                                if libc::listen(fd, 5) == 0 {
                                    libc::freeaddrinfo(list);
                                    return Ok(());
                                }
                            } else {
                                libc::freeaddrinfo(list);
                                return Ok(());
                            }
                        }
                    }

                    if first_error == 0 {
                        first_error = errno::errno().0;
                    }

                    libc::close(fd);
                    p = (*p).ai_next;
                }
            } else {
                let mut p = list;
                while !p.is_null() {
                    let fd = libc::socket((*p).ai_family, (*p).ai_socktype, (*p).ai_protocol);

                    if fd == -1 {
                        if first_error == 0 {
                            first_error = errno::errno().0;
                        }
                        p = (*p).ai_next;
                        continue;
                    }

                    self.mode = SocketMode::Client;
                    self.family = (*p).ai_family;
                    self.socktype = (*p).ai_socktype;
                    self.protocol = (*p).ai_protocol;
                    self.addrlen = (*p).ai_addrlen;
                    copy_nonoverlapping(
                        (*p).ai_addr.cast::<u8>(),
                        &mut self.addr as *mut _ as *mut u8,
                        (*p).ai_addrlen as _,
                    );

                    if libc::connect(fd, &self.addr as *const _ as *const _, self.addrlen as _) == 0
                    {
                        libc::freeaddrinfo(list);
                        return Ok(());
                    }

                    if first_error == 0 {
                        first_error = errno::errno().0;
                    }

                    libc::close(fd);
                    p = (*p).ai_next;
                }
            }

            libc::freeaddrinfo(list);

            Err(IoError::Code(
                Operation::Socket(SocketOperation::Open),
                first_error,
            ))
        }
    }

    pub fn shutdown(self: Handle<Self>, how: i32) {
        unsafe {
            if self.fd == -1 {
                return;
            }

            libc::shutdown(self.fd, how);
        }
    }

    pub fn close(mut self: Handle<Self>) {
        unsafe {
            if self.fd == -1 {
                return;
            }

            libc::close(self.fd);
            self.fd = -1;
        }
    }

    pub fn name(self: Handle<Self>) -> Option<String> {
        unsafe {
            let mut hbuf = [0; libc::NI_MAXHOST as usize];
            let mut sbuf = [0; libc::NI_MAXSERV as usize];

            if libc::getnameinfo(
                &self.addr as *const _ as *const _,
                self.addrlen,
                hbuf.as_mut_ptr(),
                hbuf.len() as _,
                sbuf.as_mut_ptr(),
                sbuf.len() as _,
                libc::NI_NUMERICHOST | libc::NI_NUMERICSERV,
            ) != 0
            {
                return None;
            }

            let mut nbuf = [0; libc::NI_MAXHOST as usize + libc::NI_MAXSERV as usize];
            todo!()
        }
    }
}

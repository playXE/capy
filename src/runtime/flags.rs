#![allow(dead_code)]
use parking_lot::Mutex;
use std::{cmp::Ordering, ptr::null_mut};
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum FlagType {
    Boolean,
    Isize,
    Usize,
    F64,
    MemorySize,
    String,
    FlagHandler,
    OptionHandler,
    NumFlagTypes,
}

pub type FlagHandler = fn(bool);
pub type OptionHandler = fn(&str);

struct Flag {
    comment: &'static str,
    name: &'static str,
    short: Option<&'static str>,

    string_value: Option<String>,
    u: FlagValue,
    typ: FlagType,
    changed: bool,
}

#[repr(C)]
union FlagValue {
    addr: *mut u8,
    bool_ptr: *mut bool,
    int_ptr: *mut isize,
    u64_ptr: *mut usize,
    f64_ptr: *mut f64,
    msize_ptr: *mut MemorySize,
    char_ptr: *mut u8,
    flag_handler: FlagHandler,
    option_handler: OptionHandler,
}

impl Flag {
    fn new_type(
        name: &'static str,
        comment: &'static str,
        addr: *mut u8,
        typ: FlagType,
        short: Option<&'static str>,
    ) -> Self {
        Self {
            comment,
            name,
            string_value: None,
            u: FlagValue { addr },
            typ,
            changed: false,
            short,
        }
    }

    fn new_handler(
        name: &'static str,
        comment: &'static str,
        handler: FlagHandler,
        short: Option<&'static str>,
    ) -> Self {
        Self {
            comment,
            name,
            string_value: None,
            u: FlagValue {
                flag_handler: handler,
            },
            typ: FlagType::FlagHandler,
            changed: false,
            short,
        }
    }

    fn new_option(
        name: &'static str,
        comment: &'static str,
        handler: OptionHandler,
        short: Option<&'static str>,
    ) -> Self {
        Self {
            comment,
            name,
            string_value: None,
            u: FlagValue {
                option_handler: handler,
            },
            typ: FlagType::OptionHandler,
            changed: false,
            short,
        }
    }

    fn is_unrecognized(&self) -> bool {
        self.typ == FlagType::Boolean && unsafe { self.u.bool_ptr.is_null() }
    }
}

struct Flags {
    flags: *mut *mut Flag,
    capacity: usize,
    len: usize,
    initialized: bool,
}

unsafe impl Send for Flags {}

static FLAGS: Mutex<Flags> = Mutex::new(Flags {
    flags: null_mut(),
    capacity: 0,
    len: 0,
    initialized: false,
});

impl Flags {
    fn lookup(name: &str) -> Option<&'static mut Flag> {
        let flags = FLAGS.lock();

        for i in 0..flags.len {
            let flag = unsafe { &mut **flags.flags.add(i) };
            if flag.name == name {
                return Some(flag);
            }
        }

        None
    }

    fn lookup_short(short: &str) -> Option<&'static mut Flag> {
        let flags = FLAGS.lock();

        for i in 0..flags.len {
            let flag = unsafe { &mut **flags.flags.add(i) };
            if flag.short == Some(short) {
                return Some(flag);
            }
        }

        None
    }

    fn is_set(name: &str) -> bool {
        let flag = Self::lookup(name);

        flag.map_or(false, |flag| unsafe {
            flag.typ == FlagType::Boolean && !flag.u.bool_ptr.is_null() && *flag.u.bool_ptr
        })
    }

    unsafe fn add_flag(flag: *mut Flag) {
        let mut flags = FLAGS.lock();

        if flags.len == flags.capacity {
            if flags.flags.is_null() {
                flags.capacity = 256;
                flags.flags = libc::calloc(flags.capacity, std::mem::size_of::<*mut Flag>())
                    as *mut *mut Flag;
            } else {
                let new_capacity = flags.capacity * 2;
                let new_flags = libc::realloc(
                    flags.flags as *mut libc::c_void,
                    new_capacity * std::mem::size_of::<*mut Flag>(),
                ) as *mut *mut Flag;

                flags.capacity = new_capacity;

                for i in 0..flags.len {
                    new_flags.add(i).write(flags.flags.add(i).read());
                }
                flags.flags = new_flags;
            }
        }

        flags.flags.add(flags.len).write(flag);
        flags.len += 1;
    }

    fn set_flag_from_string(flag: &mut Flag, argument: &str) -> bool {
        assert!(!flag.is_unrecognized());

        match flag.typ {
            FlagType::Boolean => {
                if argument == "true" {
                    unsafe {
                        *flag.u.bool_ptr = true;
                    }
                } else if argument == "false" {
                    unsafe {
                        *flag.u.bool_ptr = false;
                    }
                } else {
                    return false;
                }
            }

            FlagType::String => {
                flag.string_value = Some(argument.to_owned());
            }

            FlagType::Isize => {
                let len = argument.len();

                let mut base = 10;

                if len > 2 && &argument[0..2] == "0x" {
                    base = 16;
                } else if len > 1 && &argument[0..1] == "0" {
                    base = 8;
                }

                let value = isize::from_str_radix(argument, base);

                match value {
                    Ok(value) => unsafe {
                        *flag.u.int_ptr = value;
                    },
                    Err(_) => return false,
                }
            }

            FlagType::Usize => {
                let len = argument.len();

                let mut base = 10;

                if len > 2 && &argument[0..2] == "0x" {
                    base = 16;
                } else if len > 1 && &argument[0..1] == "0" {
                    base = 8;
                }

                let value = usize::from_str_radix(argument, base);

                match value {
                    Ok(value) => unsafe {
                        *flag.u.u64_ptr = value;
                    },
                    Err(_) => return false,
                }
            }

            FlagType::FlagHandler => {
                if argument == "true" {
                    unsafe {
                        (flag.u.flag_handler)(true);
                    }
                } else if argument == "false" {
                    unsafe {
                        (flag.u.flag_handler)(false);
                    }
                } else {
                    return false;
                }
            }

            FlagType::OptionHandler => {
                flag.string_value = Some(argument.to_owned());
                unsafe {
                    (flag.u.option_handler)(argument);
                }
            }

            FlagType::MemorySize => {
                let val = parse_float_and_factor_from_str(argument);
                if let Some((float, factor)) = val {
                    unsafe {
                        *flag.u.msize_ptr = MemorySize((float * factor as f64) as usize);
                    }
                } else {
                    return false;
                }
            }

            FlagType::F64 => {
                let val = argument.parse::<f64>();

                match val {
                    Ok(val) => unsafe {
                        *flag.u.f64_ptr = val;
                    },
                    Err(_) => return false,
                }
            }

            _ => unreachable!(),
        }

        flag.changed = true;
        true
    }

    fn parse<const SHORT: bool>(option: &str) {
        let equals_pos = option.find('=');

        let argument;

        if let Some(equals_pos) = equals_pos {
            argument = &option[equals_pos + 1..];
        } else {
            const NO_1_PREFIX: &'static str = "no_";
            const NO_2_PREFIX: &'static str = "no-";

            if option.len() > NO_1_PREFIX.len() && &option[0..NO_1_PREFIX.len()] == NO_1_PREFIX {
                argument = "false";
            } else if option.len() > NO_2_PREFIX.len()
                && &option[0..NO_2_PREFIX.len()] == NO_2_PREFIX
            {
                argument = "false";
            } else {
                argument = "true";
            }
        }

        let name_len = if let Some(equals_pos) = equals_pos {
            equals_pos
        } else {
            option.len()
        };
        let name = option[0..name_len].replace('-', "_");

        let Some(flag) = (if !SHORT {
            Flags::lookup(&name)
        } else {
            Flags::lookup_short(&name)
        }) else {
            todo!()
        };

        if !flag.is_unrecognized() {
            if !Self::set_flag_from_string(flag, argument) {
                eprintln!(
                    "Ignoring flag: {} is an invalid value for flag {}",
                    argument, name
                );
            }
        }
    }

    fn process_command_line_flags(flags: impl Iterator<Item = String>) -> Result<(), &'static str> {
        if FLAGS.lock().initialized {
            return Err("flags have already been parsed");
        }

        let mut flags_vec = flags.collect::<Vec<String>>();
        flags_vec.sort_by(|a, b| compare_flag_names(a, b));

        let prefix = "--";

        let mut i = 0;

        while i < flags_vec.len() && is_valid_flag(&flags_vec[i], prefix) {
            let option = &flags_vec[i][prefix.len()..];
            Self::parse::<false>(option);
            i += 1;
        }

        let prefix = "-";
        while i < flags_vec.len() && is_valid_flag(&flags_vec[i], prefix) {
            let option = &flags_vec[i][prefix.len()..];
            Self::parse::<true>(option);
            i += 1;
        }

        FLAGS.lock().initialized = true;
        Ok(())
    }
}

fn is_valid_flag(name: &str, prefix: &str) -> bool {
    name.len() > prefix.len() && &name[0..prefix.len()] == prefix
}

fn compare_flag_names(left: &str, right: &str) -> Ordering {
    left.cmp(right)
}

pub fn parse(args: impl Iterator<Item = String>) -> Result<(), &'static str> {
    Flags::process_command_line_flags(args)
}

/// Registers a bool flag.
///
/// # Safety
///
/// `addr` must be valid for program lifetime.
#[doc(hidden)]
pub unsafe fn register_bool(
    addr: *mut bool,
    name: &'static str,
    default_value: bool,
    comment: &'static str,
    short: Option<&'static str>,
) -> bool {
    let flag = Flags::lookup(name);

    if flag.is_none() {
        let flag = Flag::new_type(name, comment, addr as *mut u8, FlagType::Boolean, short);
        Flags::add_flag(Box::into_raw(Box::new(flag)));
        default_value
    } else {
        default_value
    }
}

/// Registers a usize flag.
///
/// # Safety
///
/// `addr` must be valid for program lifetime.
#[doc(hidden)]
pub unsafe fn register_usize(
    addr: *mut usize,
    name: &'static str,
    default_value: usize,
    comment: &'static str,
    short: Option<&'static str>,
) -> usize {
    let flag = Flags::lookup(name);

    if flag.is_none() {
        let flag = Flag::new_type(name, comment, addr as *mut u8, FlagType::Usize, short);
        Flags::add_flag(Box::into_raw(Box::new(flag)));
        default_value
    } else {
        default_value
    }
}

/// Registers a isize flag.
///
/// # Safety
///
/// `addr` must be valid for program lifetime.
#[doc(hidden)]
pub unsafe fn register_isize(
    addr: *mut isize,
    name: &'static str,
    default_value: isize,
    comment: &'static str,
    short: Option<&'static str>,
) -> isize {
    let flag = Flags::lookup(name);

    if flag.is_none() {
        let flag = Flag::new_type(name, comment, addr as *mut u8, FlagType::Isize, short);
        Flags::add_flag(Box::into_raw(Box::new(flag)));
        default_value
    } else {
        default_value
    }
}

/// Register a string flag.
///
/// # Safety
///
/// `addr` must be valid for program lifetime.
#[doc(hidden)]
pub unsafe fn register_string(
    addr: *mut String,
    name: &'static str,
    default_value: String,
    comment: &'static str,
    short: Option<&'static str>,
) -> String {
    let flag = Flags::lookup(name);

    if flag.is_none() {
        let flag = Flag::new_type(name, comment, addr as *mut u8, FlagType::String, short);
        Flags::add_flag(Box::into_raw(Box::new(flag)));
        default_value
    } else {
        default_value
    }
}

#[doc(hidden)]
pub unsafe fn register_memorysize(
    addr: *mut MemorySize,
    name: &'static str,
    default_value: MemorySize,
    comment: &'static str,
    short: Option<&'static str>,
) -> MemorySize {
    let flag = Flags::lookup(name);

    if flag.is_none() {
        let flag = Flag::new_type(name, comment, addr as *mut u8, FlagType::MemorySize, short);
        Flags::add_flag(Box::into_raw(Box::new(flag)));
        default_value
    } else {
        default_value
    }
}

/// Registers an option handler.

#[doc(hidden)]
pub fn register_handler(
    handler: OptionHandler,
    name: &'static str,
    comment: &'static str,
    short: Option<&'static str>,
) {
    let flag = Flags::lookup(name);

    if flag.is_none() {
        let flag = Flag::new_option(name, comment, handler, short);
        unsafe {
            Flags::add_flag(Box::into_raw(Box::new(flag)));
        }
    }
}

/// Registers an flag handler.
#[doc(hidden)]
pub fn register_flag_handler(
    handler: FlagHandler,
    name: &'static str,
    comment: &'static str,
    short: Option<&'static str>,
) {
    let flag = Flags::lookup(name);

    if flag.is_none() {
        let flag = Flag::new_handler(name, comment, handler, short);
        unsafe {
            Flags::add_flag(Box::into_raw(Box::new(flag)));
        }
    }
}

#[doc(hidden)]
pub unsafe fn register_f64(
    addr: *mut f64,
    name: &'static str,
    default_value: f64,
    comment: &'static str,
    short: Option<&'static str>,
) -> f64 {
    let flag = Flags::lookup(name);

    if flag.is_none() {
        let flag = Flag::new_type(name, comment, addr as *mut u8, FlagType::F64, short);
        Flags::add_flag(Box::into_raw(Box::new(flag)));
        default_value
    } else {
        default_value
    }
}

#[doc(hidden)]
pub use ctor::ctor;
#[doc(hidden)]
pub use paste;

use crate::runtime::utils::parse_float_and_factor_from_str;

use super::utils::MemorySize;
#[macro_export]
macro_rules! define_flag {
    ($typ: ident, $name: ident, $default_value: expr, $comment: literal) => {
        paste::paste! {

            static mut [<FLAG_ $name:upper>]: std::mem::MaybeUninit<$typ> = std::mem::MaybeUninit::uninit();

            #[doc(hidden)]
            #[ctor::ctor]
            fn [<init_ $name _flag>]() {
                
                unsafe {
                    [<FLAG_ $name:upper>].as_mut_ptr().write($default_value);
                    $crate::runtime::flags::[<register_ $typ:lower>](
                        [<FLAG_ $name:upper>].as_mut_ptr().cast(),
                        stringify!($name),
                        $default_value,
                        $comment,
                        None,
                    );
                }
            }

            pub fn $name() -> &'static $typ {
                unsafe { [<FLAG_ $name:upper>].assume_init_ref() }
            }

            pub fn [<set_ $name>]($name: $typ) {
                unsafe {
                    *[<FLAG_ $name:upper>].as_mut_ptr() = $name;
                }
            }
        }
    };

    ($typ: ident, $name: ident, $short: literal, $default_value: expr, $comment: literal) => {
        paste::paste! {

            static mut [<FLAG_ $name:upper>]: std::mem::MaybeUninit<$typ> = std::mem::MaybeUninit::uninit();

            #[doc(hidden)]
            #[ctor::ctor]
            fn [<init_ $name _flag>]() {
                unsafe {
                    [<FLAG_ $name:upper>].as_mut_ptr().write($default_value);
                    $crate::runtime::flags::[<register_ $typ:lower>](
                        [<FLAG_ $name:upper>].as_mut_ptr().cast(),
                        stringify!($name),
                        $default_value,
                        $comment,
                        Some($short),
                    );
                }
            }

            pub fn $name() -> &'static $typ {
                unsafe { [<FLAG_ $name:upper>].assume_init_ref() }
            }

            pub fn [<set_ $name>]($name: $typ) {
                unsafe {
                    *[<FLAG_ $name:upper>].as_mut_ptr() = $name;
                }
            }
        }
    };
}

#[macro_export]
macro_rules! define_flag_handler {
    ($handler: expr, $name: ident, $comment: literal) => {
        paste::paste! {

            #[doc(hidden)]
            #[ctor::ctor]
            fn [<init_ $name _flag>]() {
                $crate::runtime::flags::register_flag_handler($handler, stringify!($name), $comment, None);
            }
        }
    };
}

#[macro_export]
macro_rules! define_option_handler {
    ($handler: expr, $name: ident, $comment: literal) => {
        paste::paste! {

            #[doc(hidden)]
            #[ctor::ctor]
            fn [<init_ $name _flag>]() {
                $crate::runtime::flags::register_handler($handler, stringify!($name), $comment, None);
            }
        }
    };
}

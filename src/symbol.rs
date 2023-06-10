use std::{mem::MaybeUninit, sync::atomic::AtomicUsize};

use dashmap::DashMap;
use once_cell::sync::Lazy;
use rsgc::{
    heap::{heap::heap, root_processor::SimpleRoot},
    prelude::{Handle, Object, Visitor},
    thread::Thread,
};

use crate::{object::Symbol, value::Value};

static OTABLE: Lazy<DashMap<&'static str, Handle<Symbol>>> = Lazy::new(|| DashMap::new());

fn trace_symbols(visitor: &mut dyn Visitor) {
    {
        for symbol in OTABLE.iter() {
            symbol.value().trace(visitor);
        }
    }
}

pub(crate) fn init_symbols() {
    heap().add_root(SimpleRoot::new("Symbols", "sym", |proc| {
        trace_symbols(proc.visitor());
    }));
}

pub fn make_symbol(name: &str, interned: bool) -> Value {
    if interned {
        // Fast path
        let otable = &*OTABLE;
        if let Some(symbol) = otable.get(name) {
            return Value::encode_object_value(*symbol.value());
        }
    }

    let mut sym = Thread::current().allocate_varsize::<Symbol>(name.len() + 1);

    // # SAFETY:
    // 
    // `allocate_varsize()` allocates enough space to hold the `Symbol` struct plus the length of the name plus 1 for the null terminator.
    // Memory is zeroed and pointer is valid.
    unsafe {
        let ptr = sym.assume_init_mut();
        (*ptr)
            .data
            .as_mut_ptr()
            .copy_from_nonoverlapping(name.as_ptr(), name.len());
        (*ptr).data.as_mut_ptr().add(name.len()).write(0);
        (*ptr).interned = interned;
        (*ptr).generated = false;

        let sym = sym.assume_init();
        
        if interned {
            let otable = &*OTABLE;
            otable.insert(std::mem::transmute(&**sym), sym);
        }
        Value::encode_object_value(sym)
    }
}

pub fn gensym(prefix: &str) -> Value {
    static GENSYM_COUNT: AtomicUsize = AtomicUsize::new(0);

    let sym = make_symbol(
        &format!(
            "{}{}",
            prefix,
            GENSYM_COUNT.fetch_add(1, std::sync::atomic::Ordering::SeqCst)
        ),
        false,
    );

    unsafe {
        let mut symbol = std::mem::transmute::<_, Handle<Symbol>>(sym.get_object());
        symbol.generated = true;
    }

    sym
}

/// If symbol `s` has prefix `p`, return the symbol without the prefix.
/// Otherwise, return `#f`.
pub fn scm_symbol_sans_prefix(s: Handle<Symbol>, p: Handle<Symbol>) -> Value {
    if let Some(n) = s.strip_prefix(&**p) {
        return make_symbol(n, true);
    } else {
        return Value::encode_bool_value(false)
    }
}

pub trait Intern {
    fn intern(&self) -> Handle<Symbol>;
}

impl Intern for str {
    fn intern(&self) -> Handle<Symbol> {
        make_symbol(self, true).symbol()
    }
}

impl Intern for String {
    fn intern(&self) -> Handle<Symbol> {
        make_symbol(self, true).symbol()
    }
}

impl Intern for Handle<Symbol> {
    fn intern(&self) -> Handle<Symbol> {
        *self
    }
}
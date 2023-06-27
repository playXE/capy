use std::{collections::hash_map::Entry, mem::MaybeUninit, sync::atomic::AtomicUsize};

use rsgc::{
    heap::{heap::heap, root_processor::SimpleRoot},
    prelude::{Handle, Object, Visitor},
    sync::mutex::Mutex,
    thread::Thread,
};
use std::collections::HashMap;

use crate::{
    runtime::object::{ObjectHeader, Symbol, Type},
    runtime::value::Value,
};

fn trace_symbols(visitor: &mut dyn Visitor) {
    unsafe {
        let symtab = SYMTAB.assume_init_ref();
        let symtab = symtab.unsafe_get();

        for symbol in symtab.values() {
            symbol.trace(visitor);
        }
        //for symbol in OTABLE.iter() {
        //    symbol.value().trace(visitor);
        //}
    }
}

pub(crate) fn init_symbols() {
    unsafe {
        SYMTAB = MaybeUninit::new(Mutex::new(HashMap::new()));
    }

    heap().add_root(SimpleRoot::new("Symbols", "sym", |proc| {
        trace_symbols(proc.visitor());
    }));
}

static mut SYMTAB: MaybeUninit<Mutex<HashMap<&str, Handle<Symbol>>>> = MaybeUninit::uninit();

pub fn make_symbol(name: &str, interned: bool) -> Value {
    if interned {
        // Fast path
        unsafe {
            let symtab = SYMTAB.assume_init_ref();
            if let Some(sym) = symtab.lock(true).get(name) {
                return Value::encode_object_value(*sym);
            }
        }
    }

    let mut sym = Thread::current().allocate_varsize::<Symbol>(name.len());

    // # SAFETY:
    //
    // `allocate_varsize()` allocates enough space to hold the `Symbol` struct plus the length of the name plus 1 for the null terminator.
    // Memory is zeroed and pointer is valid.
    unsafe {
        let ptr = sym.assume_init_mut();
        (*ptr).object = ObjectHeader::new(Type::Symbol);
        (*ptr)
            .data
            .as_mut_ptr()
            .copy_from_nonoverlapping(name.as_ptr(), name.len());
        (*ptr).interned = interned;
        (*ptr).generated = false;

        let sym = sym.assume_init();

        if interned {
            match SYMTAB
                .assume_init_ref()
                .lock(true)
                .entry(std::mem::transmute({
                    let s: &str = &sym;
                    s
                })) {
                Entry::Occupied(entry) => return Value::encode_object_value(*entry.get()),
                Entry::Vacant(entry) => {
                    entry.insert(sym);
                }
            }
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
        return Value::encode_bool_value(false);
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

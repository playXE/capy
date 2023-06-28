use crate::runtime::{
    list::scm_cons,
    module::{scm_find_binding, SCM_BINDING_STAY_IN_MODULE},
    object::{Identifier, Module, ObjectHeader, ReaderReference, Symbol, Type, GLOC},
    string::make_string,
    value::Value,
    vector::make_vector,
};
use crate::scm_for_each;
use rsgc::{
    prelude::Handle,
    system::collections::hashmap::{Entry, HashMap},
    thread::Thread,
};
use std::collections::hash_map::RandomState;
pub fn scm_outermost_identifier(mut id: Handle<Identifier>) -> Handle<Identifier> {
    while id.name.is_xtype(Type::Identifier) {
        id = id.name.identifier();
    }

    id
}

pub fn scm_unwrap_identifier(id: Handle<Identifier>) -> Handle<Symbol> {
    scm_outermost_identifier(id).name.symbol()
}

pub fn scm_make_identifier(
    name: Value,
    module: Option<Handle<Module>>,
    env: Value,
) -> Handle<Identifier> {
    let id = Thread::current().allocate(Identifier {
        object: ObjectHeader::new(Type::Identifier),
        module: module.unwrap().into(),
        name,
        env: scm_cons(Thread::current(), Value::encode_bool_value(false), env),
    });

    id
}

pub fn scm_wrap_identifier(id: Handle<Identifier>) -> Handle<Identifier> {
    let id = Thread::current().allocate(Identifier {
        object: ObjectHeader::new(Type::Identifier),
        module: id.module,
        name: Value::encode_object_value(id),
        env: id.env,
    });

    id
}

fn get_binding_frame(var: Value, env: Value) -> Value {
    scm_for_each!(frame, env, {
        if !frame.car().is_xtype(Type::Pair) {
            continue;
        }

        scm_for_each!(fp, frame.cdar(), {
            if fp.caar() == var {
                return frame;
            }
        });
    });

    Value::encode_null_value()
}

pub fn scm_identifier_env(id: Handle<Identifier>) -> Value {
    if id.env.car().is_false() {
        let f = get_binding_frame(id.name, id.env.cdr());
        id.env.set_cdr(f);
        id.env.set_car(Value::encode_bool_value(true));
    }

    id.env.cdr()
}

pub fn identifier_to_symbol(id: Handle<Identifier>) -> Handle<Symbol> {
    scm_unwrap_identifier(id)
}

struct UnwrapCtx {
    /// object -> #f | R() | R(v)
    history: Handle<HashMap<Value, Value>>,
    /// location -> read-ref
    refs: Handle<HashMap<usize, Value>>,
    immutable: bool,
}

impl UnwrapCtx {
    fn register_location(&mut self, loc: &mut Value, ref_: Value) {
        if !ref_.is_xtype(Type::ReaderReference) {
            return;
        }

        if ref_.is_reader_reference_realized() {
            *loc = ref_.reader_reference().value;
        } else {
            self.refs
                .put(Thread::current(), loc as *mut Value as usize, ref_);
        }
    }

    fn fill_history(entry: Entry<'_, Value, Value, RandomState>, value: Value) {
        match entry {
            Entry::Vacant(entry) => {
                entry.insert(value);
            }

            Entry::Occupied(entry) => {
                if entry.get().is_reader_reference() {
                    Thread::current().write_barrier(entry.get().reader_reference());
                    entry.get().reader_reference().value = value;
                }
            }
        }
    }

    fn patch_locations(&self) {
        for (&loc, &vref) in self.refs.iter() {
            unsafe {
                let loc = &mut *(loc as *mut Value);
                if vref.is_reader_reference() {
                    *loc = vref.reader_reference().value;
                }
            }
        }
    }

    fn unwrap_rec(&mut self, form: Value) -> Value {
        if !form.is_object() {
            return form;
        }

        let e = self.history.entry(form);
        match e {
            Entry::Occupied(mut entry) => {
                if entry.get().is_false() {
                    let v: Value = Thread::current()
                        .allocate(ReaderReference {
                            header: ObjectHeader::new(Type::ReaderReference),
                            value: Value::encode_undefined_value(),
                        })
                        .into();
                    *entry.get_mut() = v;
                }

                return *entry.get();
            }

            _ => (),
        }

        if form.is_pair() {
            let ca = self.unwrap_rec(form.car());
            let cd = self.unwrap_rec(form.cdr());

            if ca == form.car() && cd == form.cdr() && !self.immutable {
                Self::fill_history(self.history.entry(form), form);

                return form;
            }

            let p = scm_cons(Thread::current(), ca, cd);

            Self::fill_history(self.history.entry(form), p);

            self.register_location(&mut p.pair().car, ca);
            self.register_location(&mut p.pair().cdr, cd);
        } else if form.is_wrapped_identifier() {
            return scm_unwrap_identifier(form.identifier()).into();
        } else if form.is_vector() {
            let len = form.vector_len();

            for i in 0..len {
                let elt = self.unwrap_rec(form.vector_ref(i));

                if elt != form.vector_ref(i) || self.immutable {
                    let mut new_vec = make_vector(Thread::current(), form.vector_len());

                    for j in 0..i {
                        Thread::current().write_barrier(new_vec);
                        new_vec[j] = form.vector_ref(j);
                    }

                    let new_vec: Value = new_vec.into();

                    self.register_location(&mut new_vec.vector()[i], elt);
                    Thread::current().write_barrier(new_vec.vector());
                    new_vec.vector_set(i, elt);

                    for j in i + 1..len {
                        let elt = self.unwrap_rec(form.vector_ref(j));
                        self.register_location(&mut new_vec.vector()[i], elt);
                        Thread::current().write_barrier(new_vec.vector());
                        new_vec.vector_set(j, elt);
                    }

                    Self::fill_history(self.history.entry(form), new_vec);

                    return new_vec;
                }
            }

            Self::fill_history(self.history.entry(form), form);
        }

        form
    }
}

pub fn scm_unwrap_syntax(form: Value, immutable: bool) -> Value {
    let mut ctx = UnwrapCtx {
        history: HashMap::with_hasher_and_capacity(RandomState::new(), 16),
        refs: HashMap::with_hasher_and_capacity(RandomState::new(), 16),
        immutable,
    };

    let form = ctx.unwrap_rec(form);

    ctx.patch_locations();

    form
}

pub fn scm_identifier_global_binding(id: Handle<Identifier>) -> Option<Handle<GLOC>> {
    let z = scm_outermost_identifier(id);

    scm_find_binding(z.module.module(), z.name.symbol(), 0)
}

pub fn scm_identifier_global_ref(id: Handle<Identifier>) -> Result<(Value, Handle<GLOC>), Value> {
    let gloc = scm_identifier_global_binding(id);

    if let Some(gloc) = gloc {
        return Ok((gloc.value, gloc));
    }

    Err(make_string(
        Thread::current(),
        &format!("unbound variable: {}", scm_unwrap_identifier(id)),
    )
    .into())
}

pub fn scm_identifier_global_set(id: Handle<Identifier>, val: Value) -> Result<Handle<GLOC>, Value> {
    let z = scm_outermost_identifier(id);

    let gloc = scm_find_binding(z.module.module(), z.name.symbol(), SCM_BINDING_STAY_IN_MODULE);

    if gloc.is_none() {
        if let Some(gloc) = scm_find_binding(z.module.module(), z.name.symbol(), 0) {
            return Err(make_string(
                Thread::current(),
                &format!("Can't mutate binding of '{:?}' which is in another module", gloc.name),
            ).into());
        } else {
            return Err(make_string(
                Thread::current(),
                &format!("unbound variable: {}", scm_unwrap_identifier(id)),
            ).into());
        }
    }

    let mut gloc = unsafe { gloc.unwrap_unchecked() };
    if let Some(setter) = gloc.setter {
        setter(gloc, val)?;

        Ok(gloc)
    } else {
        Thread::current().write_barrier(gloc);
        gloc.value = val;

        Ok(gloc)
    }
}

pub fn scm_identifier_to_symbol(id: Value) -> Handle<Symbol> {
    if id.is_symbol() {
        return id.symbol();
    }

    scm_unwrap_identifier(id.identifier())
}

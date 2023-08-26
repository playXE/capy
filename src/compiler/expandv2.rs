use crate::{runtime::{value::Value, object::{ScmIdentifier, scm_car, scm_cdr, scm_cdar, scm_caar, scm_set_cdr, scm_set_car, scm_vector_set, scm_vector_ref}, list::scm_acons}, vm::thread::Thread, gc_protect};

pub fn make_identifier(mut name: Value, mut env: Value, mut frames: Value) -> Value {
    let t = Thread::current();
    let id = gc_protect!(t => name, env, frames => t.make_identifier::<false>());
    let idr = id.cast_as::<ScmIdentifier>();
    idr.name = name;
    idr.env = env;
    idr.frames = frames;

    id
}

pub fn outermost_identifier(mut id: Value) -> Value {
   
    while id.is_identifier() {
        id = id.cast_as::<ScmIdentifier>().name;
    }
    id
}

pub fn unwrap_identifier(id: Value) -> Value {
    outermost_identifier(id).cast_as::<ScmIdentifier>().name
}

pub fn identifier_to_symbol(id: Value) -> Value {
    if id.is_symbol() {
        id
    } else {
        unwrap_identifier(id)
    }
}

pub fn wrap_identifier(mut id: Value) -> Value {
    let t = Thread::current();
    let id = gc_protect!(t => id => t.make_identifier::<false>());
    let idr = id.cast_as::<ScmIdentifier>();
    idr.name = id;
    idr.env = id.cast_as::<ScmIdentifier>().env;
    idr.frames = id.cast_as::<ScmIdentifier>().frames;

    id
}

fn get_binding_frame(var: Value, env: Value) -> Value {
    let mut frame = env;

    while frame.is_pair() {
        if !scm_car(frame).is_pair() {
            frame = scm_cdr(frame);
            continue;
        }

        let mut fp = scm_cdar(frame);

        while fp.is_pair() {
            if scm_caar(fp) == var {
                return frame;
            }

            fp = scm_cdr(fp);
        }

        frame = scm_cdr(frame);
    }

    Value::encode_null_value()
}

pub fn identifier_env(mut id: Value) -> Value {
    let idr = id.cast_as::<ScmIdentifier>();
    if scm_car(idr.frames).is_false() {
        let f = get_binding_frame(idr.name, scm_cdr(idr.frames));
        scm_set_cdr(id, Thread::current(), f);
        scm_set_car(id, Thread::current(), Value::encode_bool_value(true));
    }

    idr.frames
}

pub fn make_cenv(mut env: Value, mut frames: Value) -> Value {
    let t = Thread::current();
    let cenv = gc_protect!(t => env, frames => t.make_vector::<false>(4, Value::encode_null_value()));

    scm_vector_set(cenv, t, 0, env);
    scm_vector_set(cenv, t, 1, frames);

    cenv
}

pub fn cenv_env(cenv: Value) -> Value {
    scm_vector_ref(cenv, 0)
}

pub fn cenv_frames(cenv: Value) -> Value {
    scm_vector_ref(cenv, 1)
}

pub fn cenv_set_env(cenv: Value, env: Value) {
    scm_vector_set(cenv, Thread::current(), 0, env);
}

pub fn cenv_set_frames(cenv: Value, frames: Value) {
    scm_vector_set(cenv, Thread::current(), 1, frames);
}

pub fn cenv_name(cenv: Value) -> Value {
    scm_vector_ref(cenv, 2)
}

pub fn cenv_set_name(cenv: Value, name: Value) {
    scm_vector_set(cenv, Thread::current(), 2, name);
}

pub fn cenv_copy(mut cenv: Value) -> Value {
    let t = Thread::current();
    let ncenv = gc_protect!(t => cenv => t.make_vector::<false>(4, Value::encode_null_value()));

    scm_vector_set(cenv, t, 0, cenv_env(ncenv));
    scm_vector_set(cenv, t, 1, cenv_frames(ncenv));
    scm_vector_set(cenv, t, 2, cenv_name(ncenv));
    cenv
}

fn lookup_int(mut frames: Value, env: Value, name: Value) -> Value {
    let mut y = name;

   
    loop {
        while frames.is_pair() {
            let fp1 = frames;
            let mut vls = scm_cdar(fp1);

            while vls.is_pair() {
                let vp = scm_car(vls);

                if scm_car(vp) == y {
                    return scm_cdr(vp);
                }

                vls = scm_cdr(vls);
            }

            frames = scm_cdr(frames);
        }

        if y.is_identifier() {
            let inner = y.cast_as::<ScmIdentifier>().name;
            if inner.is_identifier() {
                frames = y.cast_as::<ScmIdentifier>().frames;
            }

            y = inner;
        } else {
            break;
        }
    }

    if name.is_symbol() {
        make_identifier(name, env, frames)
    } else {
        name
    }
}

pub fn cenv_lookup(cenv: Value, name: Value) -> Value {
    lookup_int(cenv_frames(cenv), cenv_env(cenv), name)
}

pub fn cenv_extend(mut cenv: Value, frame: Value, typ: Value) -> Value {
    let t = Thread::current();
    let frames = gc_protect!(t => cenv => scm_acons(typ, frame, cenv_frames(cenv)));

    make_cenv(cenv_env(cenv), frames)
}

pub fn cenv_is_toplevel(cenv: Value) -> bool {
    cenv_frames(cenv).is_null()
}

pub fn is_free_identifier_eq(mut id1: Value, mut id2: Value) -> bool {
    if id1.is_identifier() && id2.is_identifier() {
        if id1 == id2 {
            return true;
        }
        let t = Thread::current();
        let b1 = gc_protect!(t => id1, id2 => lookup_int(id1.cast_as::<ScmIdentifier>().frames, identifier_env(id1), id1));
        let b2 = gc_protect!(t => id1, id2 => lookup_int(id2.cast_as::<ScmIdentifier>().frames, identifier_env(id2), id2));
        if b1.is_identifier() && b2.is_identifier() {
            return unwrap_identifier(id1) == unwrap_identifier(id2);
        } else {
            b1 == b2 
        }
    } else {
        false 
    }
}

pub fn er_compare(a: Value, mut b: Value, mut env: Value, mut frames: Value) -> bool {
    if (a.is_identifier() || a.is_symbol()) && (b.is_identifier() || b.is_symbol()) {
        let t = Thread::current();
        let a1 = gc_protect!(t => env, frames, b => lookup_int(env, frames, a));
        let b1 = gc_protect!(t => env, frames => lookup_int(env, frames, b));
        if a1 == b1 {
            return true;
        }

        if a1.is_identifier() && b1.is_identifier() {
            return is_free_identifier_eq(a1, b1);
        }
    }

    false
}
use core::fmt;

use rsgc::system::{object::Handle, traits::Object, weak_reference::WeakReference};

use crate::data::environment::Environment;

use super::binding_group::BindingGroup;

pub enum Env {
    Expired,
    Group(Handle<Environment>),
    Local(Handle<BindingGroup>),
}

impl Object for Env {
    fn trace(&self, visitor: &mut dyn rsgc::system::traits::Visitor) {
        match self {
            Env::Expired => (),
            Env::Group(group) => group.trace(visitor),
            Env::Local(local) => local.trace(visitor),
        }
    }
}

impl fmt::Display for Env {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Expired => write!(f, "expired"),
            Self::Group(env) => write!(f,"global {:p}", *env),
            Self::Local(env) => write!(f,"local {:p}", *env),
        }
    }
}

/// Weak environments are used to persist environments during the macro expansion process,
/// for instance in generated symbols. They can be mapped back to regular environments via the
/// `env` method. If this happens outside of the compilation context, the original environment
/// has expired and `env` returns an expired environment.
pub enum WeakEnv {
    Global(Handle<WeakReference<Environment>>),
    Local(Handle<WeakReference<BindingGroup>>),
}

impl WeakEnv {
    pub fn env(&self) -> Env {
        match self {
            WeakEnv::Global(global) => match global.upgrade() {
                Some(env) => Env::Group(env),
                None => Env::Expired,
            },
            WeakEnv::Local(local) => match local.upgrade() {
                Some(env) => Env::Local(env),
                None => Env::Expired,
            },
        }
    }
}

impl Object for WeakEnv {
    fn trace(&self, visitor: &mut dyn rsgc::system::traits::Visitor) {
        match self {
            WeakEnv::Global(global) => global.trace(visitor),
            WeakEnv::Local(local) => local.trace(visitor),
        }
    }
}

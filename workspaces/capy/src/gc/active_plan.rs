use std::mem::transmute;

use mmtk::vm::ActivePlan;

use crate::runtime::thread::{threads, Thread, ThreadKind};

use super::CapyVM;

pub struct VMActivePlan;

impl ActivePlan<CapyVM> for VMActivePlan {
    fn is_mutator(tls: mmtk::util::VMThread) -> bool {
        unsafe {
            let thread = transmute::<_, &'static Thread>(tls);
            thread.kind == ThreadKind::Mutator
        }
    }

    fn mutator(tls: mmtk::util::VMMutatorThread) -> &'static mut mmtk::Mutator<CapyVM> {
        unsafe {
            let thread = transmute::<_, &'static mut Thread>(tls);
            &mut thread.mmtk().mutator
        }
    }

    fn mutators<'a>() -> Box<dyn Iterator<Item = &'a mut mmtk::Mutator<CapyVM>> + 'a> {
        Box::new(unsafe {
            threads()
                .iter_unlocked()
                .filter(|&&x| (*x).kind == ThreadKind::Mutator)
                .copied()
                .map(|thread| &mut *(*thread).mmtk().mutator)
        })
    }

    fn number_of_mutators() -> usize {
        unsafe {
            threads()
                .iter_unlocked()
                .filter(|&&x| (*x).kind == ThreadKind::Mutator)
                .count()
        }
    }

    fn vm_trace_object<Q: mmtk::ObjectQueue>(
        _queue: &mut Q,
        object: mmtk::util::ObjectReference,
        _worker: &mut mmtk::scheduler::GCWorker<CapyVM>,
    ) -> mmtk::util::ObjectReference {
        object
    }
}

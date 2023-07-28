#![allow(unused_variables)]
use std::{
    mem::{size_of, transmute},
    sync::{Arc, Barrier},
};

use mmtk::{
    memory_manager::{start_control_collector, start_worker},
    util::{
        alloc::fill_alignment_gap,
        copy::{CopySemantics, GCWorkerCopyContext},
        Address, ObjectReference,
    },
    vm::{
        edge_shape::{SimpleEdge, UnimplementedMemorySlice},
        *,
    },

};

use crate::{
    runtime::value::Value,
    utils::round_up,
    vm::{
        safepoint::SafepointSynchronize,
        scm_virtual_machine,
        thread::{threads, Thread, ThreadKind},
    },
};

use self::shadow_stack::visit_roots;
use crate::runtime::object::{scm_car, scm_cdr, ScmCellHeader, ScmCellRef, TypeId};

pub mod memory_region;
pub mod refstorage;
pub mod shadow_stack;
pub mod virtual_memory;

#[derive(Default)]
pub struct CapyVM;

pub struct ScmObjectModel;

pub const FORWARDING_BITS_METADATA_SPEC: VMLocalForwardingBitsSpec = //VMLocalForwardingBitsSpec::side_first();
    VMLocalForwardingBitsSpec::in_header(56);

pub const LOGGING_SIDE_METADATA_SPEC: VMGlobalLogBitSpec = VMGlobalLogBitSpec::in_header(59);
pub const FORWARDING_POINTER_METADATA_SPEC: VMLocalForwardingPointerSpec =
    VMLocalForwardingPointerSpec::in_header(0);

pub const MARKING_METADATA_SPEC: VMLocalMarkBitSpec = VMLocalMarkBitSpec::side_first();//side_after(LOGGING_SIDE_METADATA_SPEC.as_spec());
pub const LOS_METADATA_SPEC: VMLocalLOSMarkNurserySpec = VMLocalLOSMarkNurserySpec::in_header(62);
//VMLocalLOSMarkNurserySpec::side_after(MARKING_METADATA_SPEC.as_spec());//side_after(&MARKING_METADATA_SPEC.as_spec()); 
//VMLocalLOSMarkNurserySpec::in_header(59);

impl ObjectModel<CapyVM> for ScmObjectModel {
    const GLOBAL_LOG_BIT_SPEC: VMGlobalLogBitSpec = LOGGING_SIDE_METADATA_SPEC;
    const LOCAL_FORWARDING_BITS_SPEC: VMLocalForwardingBitsSpec = FORWARDING_BITS_METADATA_SPEC;
    const LOCAL_FORWARDING_POINTER_SPEC: VMLocalForwardingPointerSpec =
        FORWARDING_POINTER_METADATA_SPEC;
    const LOCAL_LOS_MARK_NURSERY_SPEC: VMLocalLOSMarkNurserySpec = LOS_METADATA_SPEC;
    const LOCAL_MARK_BIT_SPEC: VMLocalMarkBitSpec = MARKING_METADATA_SPEC;
    const OBJECT_REF_OFFSET_LOWER_BOUND: isize = 0;
    const UNIFIED_OBJECT_REFERENCE_ADDRESS: bool = true;

    fn address_to_ref(addr: mmtk::util::Address) -> mmtk::util::ObjectReference {
        ObjectReference::from_raw_address(addr)
    }

    fn ref_to_address(object: ObjectReference) -> mmtk::util::Address {
        object.to_raw_address()
    }

    fn copy(
        from: ObjectReference,
        semantics: CopySemantics,
        copy_context: &mut GCWorkerCopyContext<CapyVM>,
    ) -> ObjectReference {
        let size = Self::get_size_when_copied(from);
        let align = Self::get_align_when_copied(from);

        let dst = copy_context.alloc_copy(from, size, align, 0, semantics);
        let src = from.to_raw_address();
        unsafe {
            std::ptr::copy_nonoverlapping(src.to_ptr::<u8>(), dst.to_mut_ptr::<u8>(), size);
        }
        copy_context.post_copy(ObjectReference::from_raw_address(dst), size, semantics);
        ObjectReference::from_raw_address(dst)
    }

    fn copy_to(
        from: ObjectReference,
        to: ObjectReference,
        region: mmtk::util::Address,
    ) -> mmtk::util::Address {
        let need_copy = from != to;
        let bytes = Self::get_current_size(from);
        if need_copy {
            let dst = to.to_raw_address();
            let src = from.to_raw_address();

            unsafe {
                std::ptr::copy_nonoverlapping(src.to_ptr::<u8>(), dst.to_mut_ptr::<u8>(), bytes);
            }
        }
        let start = Self::ref_to_object_start(to);
        if region != Address::ZERO {
            fill_alignment_gap::<CapyVM>(region, start);
        }

        start.add(bytes)
    }

    fn get_reference_when_copied_to(
        _from: ObjectReference,
        to: mmtk::util::Address,
    ) -> ObjectReference {
        ObjectReference::from_raw_address(to)
    }

    fn get_current_size(object: ObjectReference) -> usize {
        let reference = ScmCellRef(object);

        match reference.header().type_id() {
            TypeId::Pair | TypeId::Complex | TypeId::Rational => {
                size_of::<ScmCellHeader>() + size_of::<Value>() * 2
            }

            TypeId::Box => size_of::<ScmCellHeader>() + size_of::<Value>(),

            TypeId::Bignum | TypeId::Vector => {
                let len = reference.slot_ref(1).get_int32();
                size_of::<ScmCellHeader>() + size_of::<Value>() * (len as usize)
            }

            TypeId::String | TypeId::Symbol => {
                let len = reference.slot_ref(1).get_int32();
                round_up(
                    size_of::<ScmCellHeader>() + size_of::<u8>() * (len as usize) + 1, /* for null byte */
                    size_of::<usize>(),
                    0,
                )
            }

            TypeId::Bytevector => {
                let len = reference.slot_ref(1).get_int32();
                round_up(
                    size_of::<ScmCellHeader>() + size_of::<u8>() * (len as usize),
                    size_of::<usize>(),
                    0,
                )
            }

            TypeId::Program => {
                let len = reference.slot_ref(2).get_int32();
                size_of::<ScmCellHeader>() + size_of::<Value>() * (len as usize)
            }

            _ => unreachable!(),
        }
    }

    fn get_size_when_copied(object: ObjectReference) -> usize {
        Self::get_current_size(object)
    }

    fn get_align_when_copied(_: ObjectReference) -> usize {
        std::mem::align_of::<ScmCellHeader>()
    }

    fn get_align_offset_when_copied(_: ObjectReference) -> usize {
        0
    }

    fn get_type_descriptor(_: ObjectReference) -> &'static [i8] {
        &[]
    }

    fn ref_to_object_start(object: ObjectReference) -> mmtk::util::Address {
        object.to_raw_address()
    }

    fn ref_to_header(object: ObjectReference) -> mmtk::util::Address {
        object.to_raw_address()
    }

    fn dump_object(object: ObjectReference) {
        println!("object {:p}", object.to_raw_address().to_ptr::<u8>());
    }
}

pub struct ScmActivePlan;

impl ActivePlan<CapyVM> for ScmActivePlan {
    fn global() -> &'static dyn mmtk::Plan<VM = CapyVM> {
        scm_virtual_machine().mmtk.get_plan()
    }

    fn is_mutator(tls: mmtk::util::VMThread) -> bool {
        unsafe {
            let thread = transmute::<_, *const Thread>(tls);

            (*thread).kind == ThreadKind::Mutator
        }
    }

    fn mutator(tls: mmtk::util::VMMutatorThread) -> &'static mut mmtk::Mutator<CapyVM> {
        unsafe {
            let thread = transmute::<_, *mut Thread>(tls);
            let thread: &'static mut Thread = &mut *thread;
            thread.mutator.assume_init_mut()
        }
    }

    fn mutators<'a>() -> Box<dyn Iterator<Item = &'a mut mmtk::Mutator<CapyVM>> + 'a> {
        unsafe {
            Box::new(
                threads()
                    .iter_unlocked()
                    .filter(|&&x| (*x).kind == ThreadKind::Mutator)
                    .map(|x| x)
                    .map(|&thread| (*thread).mutator.assume_init_mut()),
            )
        }
    }

    fn number_of_mutators() -> usize {
        unsafe { threads().num() }
    }

    fn vm_trace_object<Q: mmtk::ObjectQueue>(
        _queue: &mut Q,
        _object: ObjectReference,
        _worker: &mut mmtk::scheduler::GCWorker<CapyVM>,
    ) -> ObjectReference {
        _object
    }
}

pub struct ScmCollection;

impl Collection<CapyVM> for ScmCollection {
    fn stop_all_mutators<F>(_: mmtk::util::VMWorkerThread, mut mutator_visitor: F)
    where
        F: FnMut(&'static mut mmtk::Mutator<CapyVM>),
    {
        
        unsafe {
            // Sets safepoint page to `PROT_NONE` and waits for all threads to enter safepoint.
            // Some threads might enter without signal handler e.g when invoking `lock()` on Mutexes.
            let mutators = SafepointSynchronize::begin();
            for &mutator in mutators
                .iter()
                .filter(|&&x| (*x).kind == ThreadKind::Mutator)
            {
                mutator_visitor((*mutator).mutator.assume_init_mut());
            }

            scm_virtual_machine().safepoint_lock_data = Some(transmute(mutators));
        }
    }

    fn resume_mutators(_: mmtk::util::VMWorkerThread) {
        let vm = scm_virtual_machine();
        unsafe {
            let mutators = scm_virtual_machine().safepoint_lock_data.take().unwrap();
            SafepointSynchronize::end(mutators);

            // resume all mutators that were blocked by `block_for_gc`
            vm.gc_waiters_lock.notify_all();
        }
    }

    fn block_for_gc(_tls: mmtk::util::VMMutatorThread) {
        let vm = scm_virtual_machine();
        // thread will enter safepoint inside `lock`.
        let mut ml = vm.gc_waiters_lock.lock(true);
        // wait on notification for GC to resume mutators
        ml.wait();
    }

    fn spawn_gc_thread(_tls: mmtk::util::VMThread, ctx: GCThreadContext<CapyVM>) {
        match ctx {
            GCThreadContext::Controller(mut controller) => {
                let b1 = Arc::new(Barrier::new(2));
                let b2 = b1.clone();

                std::thread::spawn(move || unsafe {
                    b2.wait();
                    let tls = Thread::current();
                    tls.register_worker(true);
                    start_control_collector(
                        &scm_virtual_machine().mmtk,
                        transmute(tls),
                        &mut controller,
                    );
                    threads().remove_current_thread();
                });

                b1.wait();
            }

            GCThreadContext::Worker(mut worker) => {
                let b1 = Arc::new(Barrier::new(2));
                let b2 = b1.clone();

                std::thread::spawn(move || unsafe {
                    b2.wait();
                    let tls = Thread::current();
                    tls.register_worker(false);
                    start_worker(&scm_virtual_machine().mmtk, transmute(tls), &mut worker);
                    threads().remove_current_thread();
                });

                b1.wait();
            }
        }
    }

    fn prepare_mutator<T: mmtk::MutatorContext<CapyVM>>(
        _tls_worker: mmtk::util::VMWorkerThread,
        _tls_mutator: mmtk::util::VMMutatorThread,
        _m: &T,
    ) {
    }
}

pub struct ScmReferenceGlue;

impl Finalizable for Value {
    fn get_reference(&self) -> ObjectReference {
        todo!()
    }

    fn keep_alive<E: mmtk::scheduler::ProcessEdgesWork>(&mut self, trace: &mut E) {
        todo!()
    }

    fn set_reference(&mut self, object: ObjectReference) {
        todo!()
    }
}

impl ReferenceGlue<CapyVM> for ScmReferenceGlue {
    type FinalizableType = Value;
    fn clear_referent(new_reference: ObjectReference) {
        unimplemented!()
    }

    fn enqueue_references(references: &[ObjectReference], tls: mmtk::util::VMWorkerThread) {
        unimplemented!()
    }
    fn get_referent(object: ObjectReference) -> ObjectReference {
        unimplemented!()
    }

    fn is_referent_cleared(referent: ObjectReference) -> bool {
        unimplemented!()
    }

    fn set_referent(reff: ObjectReference, referent: ObjectReference) {
        unimplemented!()
    }
}

pub struct ScmScanning;

impl Scanning<CapyVM> for ScmScanning {
    fn scan_object<EV: EdgeVisitor<<CapyVM as VMBinding>::VMEdge>>(
        _: mmtk::util::VMWorkerThread,
        object: ObjectReference,
        edge_visitor: &mut EV,
    ) {
        let reference = ScmCellRef(object);

        match reference.header().type_id() {
            TypeId::Pair => {
                let car = scm_car(reference);
                let cdr = scm_cdr(reference);

                if car.is_object() {
                    edge_visitor.visit_edge(reference.edge(1 * size_of::<Value>()));
                }

                if cdr.is_object() {
                    edge_visitor.visit_edge(reference.edge(2 * size_of::<Value>()));
                }
            }

            TypeId::Box => {
                let value = reference.slot_ref(1);

                if value.is_object() {
                    edge_visitor.visit_edge(reference.edge(1 * size_of::<Value>()));
                }
            }

            TypeId::Vector => {
                let len = reference.slot_ref(1).get_int32();
                for i in 0..len {
                    let value = reference.slot_ref(2 + i as usize);

                    if value.is_object() {
                        edge_visitor
                            .visit_edge(reference.edge((2 + i as usize) * size_of::<Value>()));
                    }
                }
            }

            TypeId::Program => {
                let len = reference.slot_ref(2).get_int32();
                for i in 0..len {
                    let value = reference.slot_ref(3 + i as usize);

                    if value.is_object() {
                        edge_visitor
                            .visit_edge(reference.edge((3 + i as usize) * size_of::<Value>()));
                    }
                }
            }

            _ => (),
        }
    }

    fn notify_initial_thread_scan_complete(_partial_scan: bool, _tls: mmtk::util::VMWorkerThread) {}

    fn scan_roots_in_all_mutator_threads(
        _: mmtk::util::VMWorkerThread,
        mut factory: impl RootsWorkFactory<<CapyVM as VMBinding>::VMEdge>,
    ) {
        unsafe {
            for &mutator in threads().iter_unlocked() {
                let mutator = &mut *mutator;
                let mut edges = vec![];
                visit_roots(mutator.stackchain, &mut edges);
                for handle in mutator.handles.assume_init_ref().iterate_for_gc() {
                    let edge = SimpleEdge::from_address(handle.location());
                    edges.push(edge);
                }
                edges.dedup_by(|a, b| a.as_address() == b.as_address());
                factory.create_process_edge_roots_work(edges);
            }
        }
    }

    fn scan_vm_specific_roots(
        _tls: mmtk::util::VMWorkerThread,
        mut factory: impl RootsWorkFactory<<CapyVM as VMBinding>::VMEdge>,
    ) {
        let vm = scm_virtual_machine();
        let mut edges = vec![];
        for (_, value) in vm.symtable.iter() {
            let value: &ScmCellRef = unsafe { std::mem::transmute(value) };
            let edge = SimpleEdge::from_address(Address::from_ptr(value));
            edges.push(edge);
        }
        unsafe {
            vm.images.visit_roots(&mut factory);
        }
        edges.dedup_by(|a, b| a.as_address() == b.as_address());
        factory.create_process_edge_roots_work(edges);
    }

    fn scan_roots_in_mutator_thread(
        _tls: mmtk::util::VMWorkerThread,
        mutator: &'static mut mmtk::Mutator<CapyVM>,
        mut factory: impl RootsWorkFactory<<CapyVM as VMBinding>::VMEdge>,
    ) {
        /*unsafe {
            let tls = mutator.get_tls();
            if tls.0 .0.is_null() {
                return;
            }
            let tls: &'static mut Thread = transmute(tls);

            let mut edges = vec![];
            visit_roots(tls.stackchain, &mut edges);

            for handle in tls.handles.assume_init_ref().iterate_for_gc() {
                let edge = SimpleEdge::from_address(handle.location());
                edges.push(edge);
            }
            edges.dedup_by(|a, b| a.as_address() == b.as_address());
            println!("{:?}", edges);
            factory.create_process_edge_roots_work(edges);
        }*/
    }

    fn supports_return_barrier() -> bool {
        false
    }

    fn prepare_for_roots_re_scanning() {}
}

impl VMBinding for CapyVM {
    type VMObjectModel = ScmObjectModel;
    type VMEdge = SimpleEdge;
    type VMActivePlan = ScmActivePlan;
    type VMCollection = ScmCollection;
    type VMMemorySlice = UnimplementedMemorySlice;
    type VMReferenceGlue = ScmReferenceGlue;
    type VMScanning = ScmScanning;
    const USE_ALLOCATION_OFFSET: bool = false;
}

#![allow(unused_variables)]
use std::{
    mem::{size_of, transmute},
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc, Barrier,
    },
};

use mmtk::{
    memory_manager::{start_control_collector, start_worker},
    util::{
        alloc::{fill_alignment_gap, AllocatorSelector},
        copy::{CopySemantics, GCWorkerCopyContext},
        Address, ObjectReference,
    },
    vm::{
        edge_shape::{SimpleEdge, UnimplementedMemorySlice},
        *,
    },
    AllocationSemantics, Mutator, MutatorContext,
};

use crate::{
    interpreter::stackframe::{FrameKind, ScmFrame, StackElement},
    runtime::{
        arith::ScmBigInteger,
        control::{ScmContinuation, VMCont},
        environment::ScmEnvironment,
        hashtable::{
            inplace_rehash_weak_hashtable, HashTableRec, ScmHashTable, WeakHashTableRec,
            WeakHashtable,
        },
        object::*,
        value::Value,
    },
    utils::round_up,
    vm::{
        safepoint::SafepointSynchronize,
        scm_virtual_machine,
        thread::{threads, Thread, ThreadKind},
    },
};

use crate::runtime::object::{ScmCellHeader, ScmCellRef, TypeId};

pub mod memory_region;
pub mod objstorage;
pub mod refstorage;
pub mod shadow_stack;
pub mod stack;
pub mod virtual_memory;

#[derive(Hash, Clone, Copy, Eq, PartialEq, Debug)]
#[repr(transparent)]
pub struct ObjEdge {
    pub simple: SimpleEdge,
}

impl mmtk::vm::edge_shape::Edge for ObjEdge {
    #[inline]
    fn load(&self) -> ObjectReference {
        self.simple.load()
    }
    #[inline]
    fn store(&self, object: ObjectReference) {
        self.simple.store(object)
    }
    #[inline]
    fn prefetch_load(&self) {
        self.simple.prefetch_load()
    }
    #[inline]
    fn prefetch_store(&self) {
        self.simple.prefetch_store()
    }
}

static GC_CYCLE: AtomicBool = AtomicBool::new(false);

impl ObjEdge {
    #[inline]
    pub fn new(address: Address) -> Self {
        Self {
            simple: SimpleEdge::from_address(address),
        }
    }
    #[inline]
    pub fn from_address_unchecked(address: Address) -> Self {
        Self {
            simple: SimpleEdge::from_address(address),
        }
    }
    #[inline]
    pub fn from_address(address: Address) -> Self {
        let value = unsafe { address.load::<Value>() };
        /*if !value.is_object() || !mmtk::memory_manager::is_mmtk_object(unsafe { Address::from_usize(value.get_raw() as _) } ) {
            panic!("non-object value {:?} in GC cycle", value);
        }*/
        Self {
            simple: SimpleEdge::from_address(address),
        }
    }
}

#[derive(Default)]
pub struct CapyVM;

pub struct ScmObjectModel;

pub const FORWARDING_BITS_METADATA_SPEC: VMLocalForwardingBitsSpec =
    VMLocalForwardingBitsSpec::in_header(57);

pub const LOGGING_SIDE_METADATA_SPEC: VMGlobalLogBitSpec = VMGlobalLogBitSpec::side_first();
pub const FORWARDING_POINTER_METADATA_SPEC: VMLocalForwardingPointerSpec =
    VMLocalForwardingPointerSpec::in_header(0);

pub const MARKING_METADATA_SPEC: VMLocalMarkBitSpec = VMLocalMarkBitSpec::side_first();
pub const LOS_METADATA_SPEC: VMLocalLOSMarkNurserySpec = VMLocalLOSMarkNurserySpec::in_header(62);
impl ObjectModel<CapyVM> for ScmObjectModel {
    const GLOBAL_LOG_BIT_SPEC: VMGlobalLogBitSpec = LOGGING_SIDE_METADATA_SPEC;
    const LOCAL_FORWARDING_BITS_SPEC: VMLocalForwardingBitsSpec = FORWARDING_BITS_METADATA_SPEC;
    const LOCAL_FORWARDING_POINTER_SPEC: VMLocalForwardingPointerSpec =
        FORWARDING_POINTER_METADATA_SPEC;
    const LOCAL_LOS_MARK_NURSERY_SPEC: VMLocalLOSMarkNurserySpec = LOS_METADATA_SPEC;
    const LOCAL_MARK_BIT_SPEC: VMLocalMarkBitSpec = MARKING_METADATA_SPEC;
    const OBJECT_REF_OFFSET_LOWER_BOUND: isize = 0;
    const UNIFIED_OBJECT_REFERENCE_ADDRESS: bool = true;

    #[inline(always)]
    fn address_to_ref(addr: mmtk::util::Address) -> mmtk::util::ObjectReference {
        ObjectReference::from_raw_address(addr)
    }
    #[inline(always)]
    fn ref_to_address(object: ObjectReference) -> mmtk::util::Address {
        object.to_raw_address()
    }
    #[inline(always)]
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

        let to_obj = ObjectReference::from_raw_address(dst);
        copy_context.post_copy(to_obj, size, semantics);
        to_obj
    }
    #[inline(always)]
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
    #[inline(always)]
    fn get_reference_when_copied_to(
        _from: ObjectReference,
        to: mmtk::util::Address,
    ) -> ObjectReference {
        ObjectReference::from_raw_address(to)
    }

    fn get_current_size(object: ObjectReference) -> usize {
        let mut reference = ScmCellRef(unsafe { transmute(object) });

        round_up(
            match reference.header().type_id() {
                TypeId::Pair => size_of::<ScmPair>(),

                TypeId::Box => size_of::<ScmBox>(),

                TypeId::Vector => {
                    (scm_vector_length(reference.into()) as usize * size_of::<Value>())
                        + size_of::<ScmVector>()
                }

                TypeId::Values => {
                    (scm_values_length(reference.into()) as usize * size_of::<Value>())
                        + size_of::<ScmVector>()
                }

                TypeId::Tuple => {
                    (reference.cast_as::<ScmTuple>().length * size_of::<Value>())
                        + size_of::<ScmTuple>()
                }
                TypeId::String => {
                    let len = scm_string_str(reference.into()).len() + 1;
                    let size = size_of::<ScmString>();
                    len + size
                }

                TypeId::Symbol => {
                    let len = scm_symbol_str(reference.into()).len() + 1;
                    let size = size_of::<ScmSymbol>();
                    len + size
                }

                TypeId::Bytevector => {
                    if scm_bytevector_is_mapping(Value::encode_object_value(reference)) {
                        size_of::<ScmBytevector>()
                    } else {
                        let len = scm_bytevector_length(reference.into()) as usize;
                        let size = size_of::<ScmBytevector>();
                        len + size
                    }
                }

                TypeId::Program => {
                    let len = scm_program_num_free_vars(reference.into());
                    len as usize * size_of::<Value>() + size_of::<ScmProgram>()
                }

                TypeId::GLOC => size_of::<ScmGloc>(),

                TypeId::HashTableRec => unsafe {
                    let datum = reference.to_address().to_mut_ptr::<HashTableRec>();
                    let n = (*datum).capacity;

                    size_of::<HashTableRec>() + size_of::<Value>() * ((n + n) as usize - 1)
                },

                TypeId::HashTable => size_of::<ScmHashTable>(),

                TypeId::WeakHashTableRec => unsafe {
                    let datum = reference.to_address().to_mut_ptr::<WeakHashTableRec>();
                    let n = (*datum).capacity;

                    let sz = size_of::<WeakHashTableRec>() + size_of::<Value>() * (n as usize - 1);

                    sz
                },

                TypeId::WeakHashTable => size_of::<WeakHashtable>(),
                TypeId::SyntaxExpander => size_of::<ScmSyntaxExpander>(),
                TypeId::Environment => size_of::<ScmEnvironment>(),
                TypeId::VMCont => {
                    let base = size_of::<VMCont>();

                    base + reference.cast_as::<VMCont>().stack_size * size_of::<StackElement>()
                }
                TypeId::Bignum => {
                    let base = size_of::<ScmBigInteger>();
                    base + (reference.cast_as::<ScmBigInteger>().count as usize - 1)
                        * size_of::<u32>()
                }
                TypeId::Rational => size_of::<ScmRational>(),
                TypeId::Complex => size_of::<ScmComplex>(),
                TypeId::Continuation => size_of::<ScmContinuation>(),
                TypeId::WeakMapping => size_of::<ScmWeakMapping>(),
                TypeId::Frame => size_of::<ScmFrame>(),
                _ => unreachable!(),
            },
            8,
            0,
        )
    }

    fn get_size_when_copied(object: ObjectReference) -> usize {
        Self::get_current_size(object)
    }
    #[inline(always)]
    fn get_align_when_copied(_: ObjectReference) -> usize {
        std::mem::align_of::<ScmCellHeader>()
    }
    #[inline(always)]
    fn get_align_offset_when_copied(_: ObjectReference) -> usize {
        0
    }

    fn get_type_descriptor(_: ObjectReference) -> &'static [i8] {
        &[]
    }
    #[inline(always)]
    fn ref_to_object_start(object: ObjectReference) -> mmtk::util::Address {
        object.to_raw_address()
    }
    #[inline(always)]
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
                    .map(|&thread| &mut **(*thread).mutator.assume_init_mut()),
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
        {
            panic!(
                "cannot trace object {:p}",
                _object.to_raw_address().to_ptr::<u8>(),
            );
        }
    }
}

pub struct ScmCollection;

impl Collection<CapyVM> for ScmCollection {
    fn out_of_memory(_tls: mmtk::util::VMThread, err_kind: mmtk::util::alloc::AllocationError) {
        eprintln!("Out of memory: {:?}", err_kind);
        std::process::abort();
    }
    fn stop_all_mutators<F>(_: mmtk::util::VMWorkerThread, mut mutator_visitor: F)
    where
        F: FnMut(&'static mut mmtk::Mutator<CapyVM>),
    {
        GC_CYCLE.store(true, Ordering::Relaxed);
        unsafe {
            // Sets safepoint page to `PROT_NONE` and waits for all threads to enter safepoint.
            // Some threads might enter without signal handler e.g when invoking `lock()` on Mutexes.
            let mutators = SafepointSynchronize::begin();
            for &mutator in mutators
                .iter()
                .filter(|&&x| (*x).kind == ThreadKind::Mutator)
            {
                (*mutator).flush_cleaner_queue_in_gc();
                mutator_visitor((*mutator).mutator.assume_init_mut());
            }
            scm_virtual_machine().gc_counter += 1;
            scm_virtual_machine().safepoint_lock_data = Some(transmute(mutators));
        }
    }

    fn resume_mutators(_: mmtk::util::VMWorkerThread) {
        let vm = scm_virtual_machine();
        unsafe {
            GC_CYCLE.store(false, Ordering::Relaxed);
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
        let mut reference = ScmCellRef(unsafe { transmute(object) });
        match reference.header().type_id() {
            TypeId::Pair => {
                let pair = reference.cast_as::<ScmPair>();

                if pair.car.is_object() {
                    let edge = ObjEdge::from_address(Address::from_mut_ptr(&mut pair.car));
                    edge_visitor.visit_edge(edge);
                }

                if pair.cdr.is_object() {
                    let edge = ObjEdge::from_address(Address::from_mut_ptr(&mut pair.cdr));
                    edge_visitor.visit_edge(edge);
                }
            }

            TypeId::Box => {
                let boxed = reference.cast_as::<ScmBox>();

                boxed.value.visit_edge(edge_visitor);
            }

            TypeId::GLOC => {
                let gloc = reference.cast_as::<ScmGloc>();
                if gloc.value.is_object() {
                    let edge = ObjEdge::from_address(Address::from_mut_ptr(&mut gloc.value));
                    edge_visitor.visit_edge(edge);
                }

                if gloc.name.is_object() {
                    let edge = ObjEdge::from_address(Address::from_mut_ptr(&mut gloc.name));
                    edge_visitor.visit_edge(edge);
                }
            }

            TypeId::Program => {
                let program = Value::encode_object_value(reference);
                for i in 0..scm_program_num_free_vars(program) {
                    let free_var = scm_program_free_var_mut(program, i);
                    if free_var.is_object() {
                        let edge = ObjEdge::from_address(Address::from_mut_ptr(free_var));
                        edge_visitor.visit_edge(edge);
                    }
                }

                /*program
                .cast_as::<ScmProgram>()
                .constants
                .visit_edge(edge_visitor);*/
                if program.cast_as::<ScmProgram>().constants.is_object() {
                    let edge = ObjEdge::from_address(Address::from_mut_ptr(
                        &mut program.cast_as::<ScmProgram>().constants,
                    ));
                    edge_visitor.visit_edge(edge);
                }
            }

            TypeId::Subroutine => {
                let subr = reference.cast_as::<ScmSubroutine>();
                if subr.name.is_object() {
                    let edge = ObjEdge::from_address(Address::from_mut_ptr(&mut subr.name));
                    edge_visitor.visit_edge(edge);
                }
            }

            TypeId::Vector => {
                let vector = Value::encode_object_value(reference);

                for i in 0..scm_vector_length(vector) {
                    let element = scm_vector_ref_mut(vector, i);
                    if element.is_object() {
                        let edge = ObjEdge::from_address(Address::from_mut_ptr(element));
                        edge_visitor.visit_edge(edge);
                    }
                }
            }

            TypeId::Values => {
                let values = Value::encode_object_value(reference);
                for i in 0..scm_values_length(values) {
                    let element = scm_values_ref_mut(values, i);
                    if element.is_object() {
                        let edge = ObjEdge::from_address(Address::from_mut_ptr(element));
                        edge_visitor.visit_edge(edge);
                    }
                }
            }

            TypeId::Tuple => {
                let tuple = reference.cast_as::<ScmTuple>();
                for i in 0..tuple.length {
                    unsafe {
                        let value = &mut *(tuple.values.as_mut_ptr().add(i as _));
                        if value.is_object() {
                            let edge = ObjEdge::from_address(Address::from_mut_ptr(value));
                            edge_visitor.visit_edge(edge);
                        }
                    }
                }
            }

            TypeId::HashTableRec => unsafe {
                let datum = reference.to_address().to_mut_ptr::<HashTableRec>();
                let n = (*datum).capacity;
                for i in 0..(n + n) {
                    let elt_ptr = (*datum).elts.as_mut_ptr().add(i as _);

                    if (*elt_ptr).is_object() {
                        let edge = ObjEdge::from_address(Address::from_mut_ptr(elt_ptr));
                        edge_visitor.visit_edge(edge);
                    }
                }
            },

            TypeId::HashTable => {
                let hash_table = reference.cast_as::<ScmHashTable>();
                if !hash_table.datum.is_null() {
                    let datum_edge =
                        ObjEdge::from_address(Address::from_mut_ptr(&mut hash_table.datum));

                    edge_visitor.visit_edge(datum_edge);
                }

                hash_table.rehash = true;
            }

            TypeId::Environment => {
                let env = reference.cast_as::<ScmEnvironment>();
                env.ht.visit_edge(edge_visitor);
                env.name.visit_edge(edge_visitor);
            }
            TypeId::VMCont => {
                let cont = reference.cast_as::<VMCont>();
                unsafe {
                    cont.visit_edges(edge_visitor);
                }
            }

            TypeId::Continuation => {
                let cont = reference.cast_as::<ScmContinuation>();
                cont.visit_edges(edge_visitor);
            }

            TypeId::WeakMapping => {
                // do not trace `value` as strong reference here, weak-mapping behaves like ephemeron here.
                scm_virtual_machine()
                    .weakmapping_registry
                    .lock(false)
                    .push(object);
            }

            TypeId::WeakHashTableRec => {
                let datum = reference.cast_as::<WeakHashTableRec>();

                for i in 0..datum.capacity {
                    unsafe {
                        let elt = datum.elts.as_mut_ptr().add(i as _);
                        let val = elt.read();
                        if val.is_empty() || val.is_undefined() {
                            continue;
                        }

                        if (*elt).type_of() == TypeId::WeakMapping {
                            let wmap = (*elt).cast_as::<ScmWeakMapping>();
                            if !wmap.key.is_object() {
                                elt.write(Value::encode_undefined_value());
                                datum.live -= 1;
                            } else {
                                let edge = ObjEdge::from_address(Address::from_mut_ptr(elt));
                                edge_visitor.visit_edge(edge);
                            }
                        }
                    }
                }
            }

            TypeId::WeakHashTable => {
                let weakhashtable = reference.cast_as::<WeakHashtable>();
                if !weakhashtable.datum.is_null() {
                    let datum_edge =
                        ObjEdge::from_address(Address::from_mut_ptr(&mut weakhashtable.datum));
                    edge_visitor.visit_edge(datum_edge);

                    unsafe {
                        for i in 0..(*weakhashtable.datum).capacity {
                            let elt = (*weakhashtable.datum).elts.as_mut_ptr().add(i as _).read();
                            if elt.is_empty() || elt.is_undefined() {
                                continue;
                            }
                            let key = elt.cast_as::<ScmWeakMapping>().key;
                            assert!(
                                key.is_object()
                                    && mmtk::memory_manager::is_in_mmtk_spaces::<CapyVM>(
                                        transmute(key)
                                    )
                            );
                        }
                    }
                }

                scm_virtual_machine()
                    .weakhashtable_registry
                    .lock(false)
                    .push(object);
            }

            TypeId::Rational => {
                let rat = reference.cast_as::<ScmRational>();
                if rat.numerator.is_object() {
                    let edge = ObjEdge::from_address(Address::from_mut_ptr(&mut rat.numerator));
                    edge_visitor.visit_edge(edge);
                }

                if rat.denominator.is_object() {
                    let edge = ObjEdge::from_address(Address::from_mut_ptr(&mut rat.denominator));
                    edge_visitor.visit_edge(edge);
                }
            }

            TypeId::Complex => {
                let cmplx = reference.cast_as::<ScmComplex>();
                if cmplx.real.is_object() {
                    let edge = ObjEdge::from_address(Address::from_mut_ptr(&mut cmplx.real));
                    edge_visitor.visit_edge(edge);
                }

                if cmplx.imag.is_object() {
                    let edge = ObjEdge::from_address(Address::from_mut_ptr(&mut cmplx.imag));
                    edge_visitor.visit_edge(edge);
                }
            }

            TypeId::Frame => {
                let frame = reference.cast_as::<ScmFrame>();

                match frame.kind {
                    FrameKind::Cont => {
                        /* trace VMCont type, it is heap allocated and stores stack copy */
                        let cont = &mut frame.stack_holder as *mut *mut () as *mut *mut VMCont;
                        let edge = ObjEdge::from_address(Address::from_mut_ptr(cont));
                        edge_visitor.visit_edge(edge);
                    }

                    FrameKind::VM => {
                        /* no need to trace frames with VM as holder, VM is not an object */
                    }
                }
            }
            _ => (),
        }
    }

    fn notify_initial_thread_scan_complete(_partial_scan: bool, _tls: mmtk::util::VMWorkerThread) {}

    fn scan_vm_specific_roots(
        _tls: mmtk::util::VMWorkerThread,
        mut factory: impl RootsWorkFactory<<CapyVM as VMBinding>::VMEdge>,
    ) {
        let vm = scm_virtual_machine();

        vm.scan_roots(&mut factory);
    }

    fn scan_roots_in_mutator_thread(
        _tls: mmtk::util::VMWorkerThread,
        mutator: &'static mut mmtk::Mutator<CapyVM>,
        mut factory: impl RootsWorkFactory<<CapyVM as VMBinding>::VMEdge>,
    ) {
        unsafe {
            let tls = mutator.get_tls();
            if tls.0 .0.is_null() {
                return;
            }
            let tls: &'static mut Thread = transmute(tls);

            let mut edges = vec![];
            for handle in tls.handles.assume_init_ref().iterate_for_gc() {
                let edge = ObjEdge::from_address(handle.location());
                edges.push(edge);
            }
            tls.interpreter().mark_stack_for_roots(&mut factory);
            tls.interpreter().shadow_stack.walk_roots(&mut factory);
            factory.create_process_edge_roots_work(edges);
        }
    }

    fn supports_return_barrier() -> bool {
        false
    }

    fn prepare_for_roots_re_scanning() {}

    fn process_weak_refs(
        worker: &mut mmtk::scheduler::GCWorker<CapyVM>,
        tracer_context: impl ObjectTracerContext<CapyVM>,
    ) -> bool {
        let vm = scm_virtual_machine();
        // Process weak reference queue. Objects are appended here if we see `WeakMapping` in `scan_object`.
        let mut enqueued = false;
        let mut weaks = vm.weakmapping_registry.lock(false);

        tracer_context.with_tracer(worker, |tracer| {
            while let Some(mapping_ref) = weaks.pop() {
                unsafe {
                    let mapping = &mut *tracer
                        .trace_object(mapping_ref)
                        .to_address::<CapyVM>()
                        .to_mut_ptr::<ScmWeakMapping>();
                    debug_assert!(mapping.key.is_object());
                    let key = mapping.key.get_object().to_address();
                    let keyref = ObjectReference::from_address::<CapyVM>(key);

                    if keyref.is_reachable() {
                        let fwd = tracer.trace_object(keyref).to_address::<CapyVM>();

                        mapping.key = Value::encode_object_value(ScmCellRef::from_address(fwd));
                        enqueued = true;
                        if mapping.value.is_object() {
                            mapping.value = Value::encode_object_value(ScmCellRef::from_address(
                                tracer
                                    .trace_object(ObjectReference::from_raw_address(
                                        mapping.value.get_object().to_address(),
                                    ))
                                    .to_address::<CapyVM>(),
                            ));
                        }
                    } else {
                        mapping.key = Value::encode_bool_value(false);
                        mapping.value = Value::encode_bool_value(false);
                    }
                }
            }
        });

        // do not process finalization queue yet, we can resurrect objects from ephemerons
        if enqueued {
            return true;
        }

        let mut queue = vm.finalization_registry.lock(false);
        let mut new_queue = Vec::with_capacity(queue.len());

        while let Some((object, cleaner)) = queue.pop() {
            if object.is_reachable() {
                new_queue.push((object.get_forwarded_object().unwrap_or(object), cleaner));
            } else {
                match cleaner {
                    CleanerType::Drop(drop) => {
                        drop(object.to_raw_address().to_mut_ptr());
                    }

                    CleanerType::Callback(callback) => {
                        callback();
                    }
                }
            }
        }

        let mut weakhashtabs = vm.weakhashtable_registry.lock(false);

        while let Some(ht) = weakhashtabs.pop() {
            let ht = ht.get_forwarded_object().unwrap_or(ht);
            let ht = ht.to_address::<CapyVM>().to_mut_ptr::<WeakHashtable>();
            inplace_rehash_weak_hashtable(Value::encode_object_value(ScmCellRef(ht as _)));
        }

        let ienv = vm.interaction_environment.get_object();
        let iref = ObjectReference::from_address::<CapyVM>(ienv.to_address());
        assert!(iref.is_reachable());
        false
    }
}

impl VMBinding for CapyVM {
    type VMObjectModel = ScmObjectModel;
    type VMEdge = ObjEdge;
    type VMActivePlan = ScmActivePlan;
    type VMCollection = ScmCollection;
    type VMMemorySlice = UnimplementedMemorySlice<ObjEdge>;
    type VMReferenceGlue = ScmReferenceGlue;
    type VMScanning = ScmScanning;
    const USE_ALLOCATION_OFFSET: bool = false;
}

// FIXME: Waiting for this PR to get merged: https://github.com/mmtk/mmtk-core/pull/889
pub fn fast_path_allocator() -> fn(&mut Mutator<CapyVM>, usize) -> Address {
    let vm = scm_virtual_machine();
    let selector =
        mmtk::memory_manager::get_allocator_mapping(&vm.mmtk, AllocationSemantics::Default);

    fn slow(mutator: &mut Mutator<CapyVM>, size: usize) -> Address {
        mutator.alloc(size, 8, 0, AllocationSemantics::Default)
    }

    match selector {
        AllocatorSelector::Malloc(_)
        | AllocatorSelector::FreeList(_)
        | AllocatorSelector::LargeObject(_) => slow,
        _ => todo!(),
    }
}

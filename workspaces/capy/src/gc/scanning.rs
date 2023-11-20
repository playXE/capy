use super::{edges::ScmEdge, CapyVM};
use crate::runtime::{
    cell::{CellFeature, CellReference, CellTag, Pair, Rational, Vector},
    thread::Thread,
    Runtime,
};
use mmtk::{vm::Scanning, MutatorContext};
use std::{marker::PhantomData, mem::transmute};

pub struct VMScanning;

impl Scanning<CapyVM> for VMScanning {
    fn process_weak_refs(
        _worker: &mut mmtk::scheduler::GCWorker<CapyVM>,
        _tracer_context: impl mmtk::vm::ObjectTracerContext<CapyVM>,
    ) -> bool {
        false
    }
    fn scan_object_and_trace_edges<OT: mmtk::vm::ObjectTracer>(
        _tls: mmtk::util::VMWorkerThread,
        _object: mmtk::util::ObjectReference,
        _object_tracer: &mut OT,
    ) {
        unreachable!()
    }

    fn scan_object<EV: mmtk::vm::EdgeVisitor<<CapyVM as mmtk::vm::VMBinding>::VMEdge>>(
        _tls: mmtk::util::VMWorkerThread,
        object: mmtk::util::ObjectReference,
        edge_visitor: &mut EV,
    ) {
        let cell = CellReference(object, PhantomData::<*mut ()>);
        println!("scan {:p}", cell.to_ptr());
        match cell.cell_tag() {
            CellTag::PAIR => {
                let cell = cell.pair();
                if cell.car().is_cell() {
                    edge_visitor.visit_edge(cell.edge(CellReference::<Pair>::CAR_OFFSET));
                }

                if cell.cdr().is_cell() {
                    edge_visitor.visit_edge(cell.edge(CellReference::<Pair>::CDR_OFFSET));
                }
            }

            CellTag::RATIONAL => {
                let cell = cell.rational();
                if cell.numerator().is_cell() {
                    edge_visitor.visit_edge(cell.edge(CellReference::<Rational>::NUMERATOR_OFFSET));
                }

                if cell.denominator().is_cell() {
                    edge_visitor
                        .visit_edge(cell.edge(CellReference::<Rational>::DENOMINATOR_OFFSET));
                }
            }

            CellTag::FLONUM => (),

            tag if tag.feature() == CellFeature::VectorLike => {
                let length = cell.word_ref(CellReference::<Vector>::LENGTH_OFFSET);
                for i in 0..length {
                    let edge = cell.edge(1 + i as usize);
                    if cell.value_ref(1 + i as usize).is_cell() {
                        edge_visitor.visit_edge(edge);
                    }
                }
            }

            tag if tag.feature() == CellFeature::BytevectorLike => { /* no-op */ }
            CellTag::UNDEFINED
            | CellTag::UNSPECIFIED
            | CellTag::NULL
            | CellTag::EOF
            | CellTag::TRUE
            | CellTag::FALSE => (),
            _ => {
                todo!("{:x}", cell.cell_tag().0)
            }
        }
    }

    fn scan_roots_in_mutator_thread(
        _tls: mmtk::util::VMWorkerThread,
        mutator: &'static mut mmtk::Mutator<CapyVM>,
        mut factory: impl mmtk::vm::RootsWorkFactory<ScmEdge>,
    ) {
        unsafe {
            let tls = mutator.get_tls();
            if tls.0 .0.is_null() {
                return;
            }

            let tls: &'static mut Thread = transmute(tls);

            tls.shadow_stack().mark_roots(&mut factory);
        }
    }

    fn scan_vm_specific_roots(
        _tls: mmtk::util::VMWorkerThread,
        factory: impl mmtk::vm::RootsWorkFactory<<CapyVM as mmtk::vm::VMBinding>::VMEdge>,
    ) {
        unsafe {
            Runtime::get().scan_roots(factory);
        }
    }

    fn notify_initial_thread_scan_complete(_partial_scan: bool, _tls: mmtk::util::VMWorkerThread) {}

    fn support_edge_enqueuing(
        _tls: mmtk::util::VMWorkerThread,
        _object: mmtk::util::ObjectReference,
    ) -> bool {
        true
    }

    fn supports_return_barrier() -> bool {
        false
    }

    fn prepare_for_roots_re_scanning() {}
}

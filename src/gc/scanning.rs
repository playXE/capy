use std::{marker::PhantomData, mem::transmute};

use mmtk::{vm::Scanning, MutatorContext};

use crate::runtime::{cell::{CellReference, CellTag, CellFeature, Pair, Rational}, thread::Thread};

use super::CapyVM;

pub struct VMScanning;

impl Scanning<CapyVM> for VMScanning {
    fn process_weak_refs(
            _worker: &mut mmtk::scheduler::GCWorker<CapyVM>,
            _tracer_context: impl mmtk::vm::ObjectTracerContext<CapyVM>,
        ) -> bool {
        false 
    }

    fn scan_object<EV: mmtk::vm::EdgeVisitor<<CapyVM as mmtk::vm::VMBinding>::VMEdge>>(
            _tls: mmtk::util::VMWorkerThread,
            object: mmtk::util::ObjectReference,
            edge_visitor: &mut EV,
        ) {
        let cell = CellReference(object, PhantomData::<*mut ()>);

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
                    edge_visitor.visit_edge(cell.edge(CellReference::<Rational>::DENOMINATOR_OFFSET));
                }
            }

            tag if tag.feature() == CellFeature::VectorLike => {
                let length = cell.u64_ref(1);
                for i in 0..length {
                    let edge = cell.edge(2 + i as usize);
                    if cell.word_ref(2 + i as usize).is_cell() {
                        edge_visitor.visit_edge(edge);
                    }
                }
            }

            tag if tag.feature() == CellFeature::BytevectorLike => {
                /* no-op */
            }

            _ => {
                todo!("{:x}", cell.cell_tag().0)
            }
        }
    }

    fn scan_roots_in_mutator_thread(
            _tls: mmtk::util::VMWorkerThread,
            mutator: &'static mut mmtk::Mutator<CapyVM>,
            mut factory: impl mmtk::vm::RootsWorkFactory<<CapyVM as mmtk::vm::VMBinding>::VMEdge>,
        ) {
        unsafe {
            let tls = mutator.get_tls();
            if tls.0.0.is_null() {
                return;
            }

            let tls: &'static mut Thread = transmute(tls);

            tls.vm().mark_stack(&mut factory);
            tls.shadow_stack().mark_roots(&mut factory);
        }
    }

    fn scan_vm_specific_roots(tls: mmtk::util::VMWorkerThread, factory: impl mmtk::vm::RootsWorkFactory<<CapyVM as mmtk::vm::VMBinding>::VMEdge>) {
        
    }
    
    fn notify_initial_thread_scan_complete(_partial_scan: bool, _tls: mmtk::util::VMWorkerThread) {
        
    }
    
    fn support_edge_enqueuing(_tls: mmtk::util::VMWorkerThread, _object: mmtk::util::ObjectReference) -> bool {
        true 
    }

    fn supports_return_barrier() -> bool {
        false 
    }

    fn prepare_for_roots_re_scanning() {
        
    }

    
}
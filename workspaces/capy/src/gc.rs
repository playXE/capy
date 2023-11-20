#[derive(Default)]
pub struct CapyVM;

use mmtk::{
    util::ObjectReference,
    vm::{ReferenceGlue, VMBinding},
};

use self::edges::ScmEdge;

pub mod active_plan;
pub mod collection;
pub mod edges;
pub mod memory_region;
pub mod object_model;
pub mod ptr_compr;
pub mod safepoint;
pub mod scanning;
pub mod shadow_stack;
pub mod stack;
pub mod signals;
pub mod virtual_memory;

impl VMBinding for CapyVM {
    type VMObjectModel = object_model::VMObjectModel;
    type VMScanning = scanning::VMScanning;
    type VMCollection = collection::VMCollection;
    type VMActivePlan = active_plan::VMActivePlan;
    type VMEdge = ScmEdge;
    type VMMemorySlice = ScmEdge;
    type VMReferenceGlue = VMReferenceGlue;

    const MAX_ALIGNMENT: usize = 8;
    const MIN_ALIGNMENT: usize = 8;
}

pub struct VMReferenceGlue;

impl ReferenceGlue<CapyVM> for VMReferenceGlue {
    type FinalizableType = ObjectReference;
    fn clear_referent(_new_reference: mmtk::util::ObjectReference) {
        unimplemented!()
    }

    fn enqueue_references(
        _references: &[mmtk::util::ObjectReference],
        _tls: mmtk::util::VMWorkerThread,
    ) {
        todo!()
    }

    fn get_referent(_object: mmtk::util::ObjectReference) -> mmtk::util::ObjectReference {
        todo!()
    }

    fn is_referent_cleared(_referent: mmtk::util::ObjectReference) -> bool {
        todo!()
    }

    fn set_referent(_reff: mmtk::util::ObjectReference, _referent: mmtk::util::ObjectReference) {
        todo!()
    }
}

#[macro_export]
// This macro is invoked in define_vm_metadata_global_spec or define_vm_metadata_local_spec.
// Use those two to define a new VM metadata spec.
macro_rules! define_vm_metadata_spec {
    ($spec_name: ident, $is_global: expr, $log_num_bits: expr, $side_min_obj_size: expr) => {
        pub struct $spec_name(::mmtk::util::metadata::MetadataSpec);
        impl $spec_name {
            pub const LOG_NUM_BITS: usize = $log_num_bits;
            pub const IS_GLOBAL: bool = $is_global;
            pub const fn in_header(bit_offset: isize) -> Self {
                Self(mmtk::util::metadata::MetadataSpec::InHeader(
                    mmtk::util::metadata::header_metadata::HeaderMetadataSpec {
                        bit_offset,
                        num_of_bits: 1 << Self::LOG_NUM_BITS,
                }))
            }
            pub const fn side_first() -> Self {
                if Self::IS_GLOBAL {
                    Self(::mmtk::util::metadata::MetadataSpec::OnSide(::mmtk::util::metadata::side_metadata::SideMetadataSpec {
                        name: stringify!($spec_name),
                        is_global: Self::IS_GLOBAL,
                        offset: ::mmtk::util::metadata::side_metadata::GLOBAL_SIDE_METADATA_VM_BASE_OFFSET,
                        log_num_of_bits: Self::LOG_NUM_BITS,
                        log_bytes_in_region: $side_min_obj_size as usize,
                    }))
                } else {
                    Self(::mmtk::util::metadata::MetadataSpec::OnSide(::mmtk::util::metadata::side_metadata::SideMetadataSpec {
                        name: stringify!($spec_name),
                        is_global: Self::IS_GLOBAL,
                        offset: ::mmtk::util::metadata::side_metadata::LOCAL_SIDE_METADATA_VM_BASE_OFFSET,
                        log_num_of_bits: Self::LOG_NUM_BITS,
                        log_bytes_in_region: $side_min_obj_size as usize,
                    }))
                }
            }
            pub const fn side_after(spec: &::mmtk::util::metadata::MetadataSpec) -> Self {
                assert!(spec.is_on_side());
                let side_spec = spec.extract_side_spec();
                assert!(side_spec.is_global == Self::IS_GLOBAL);
                Self(::mmtk::util::metadata::MetadataSpec::OnSide(::mmtk::util::metadata::side_metadata::SideMetadataSpec {
                    name: stringify!($spec_name),
                    is_global: Self::IS_GLOBAL,
                    offset: ::mmtk::util::metadata::side_metadata::SideMetadataOffset::layout_after(side_spec),
                    log_num_of_bits: Self::LOG_NUM_BITS,
                    log_bytes_in_region: $side_min_obj_size as usize,
                }))
            }
            pub const fn as_spec(&self) -> &::mmtk::util::metadata::MetadataSpec {
                &self.0
            }
            #[inline(always)]
            pub const fn num_bits(&self) -> usize {
                1 << $log_num_bits
            }
        }
        impl std::ops::Deref for $spec_name {
            type Target = ::mmtk::util::metadata::MetadataSpec;
            #[inline(always)]
            fn deref(&self) -> &Self::Target {
                self.as_spec()
            }
        }
    };
}

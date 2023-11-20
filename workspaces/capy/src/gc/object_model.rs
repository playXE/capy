use std::{
    marker::PhantomData,
    mem::{align_of, size_of},
};

use mmtk::{
    util::{alloc::fill_alignment_gap, Address, ObjectReference},
    vm::ObjectModel,
};

use crate::runtime::{cell::*, utils::round_up_usize, value::Value};

use super::CapyVM;

pub struct VMObjectModel;

impl VMObjectModel {
    #[inline(always)]
    pub fn raw_bytes_used(cell: CellReference) -> usize {
        round_up_usize(
            match cell.cell_tag() {
                CellTag::PAIR | CellTag::RATIONAL | CellTag::COMPLEX => 2 * size_of::<Value>(),
                CellTag::PROGRAM => {
                    let nfreevars = cell.word_ref(CellReference::<Program>::NUM_FREE_VARS_OFFSET);
                    size_of::<Value>() * 2 + (size_of::<Value>() * nfreevars as usize)
                }

                CellTag::FLONUM => size_of::<f64>(),

                tag if tag.feature() == CellFeature::VectorLike => {
                    let length = cell.word_ref(CellReference::<Vector>::LENGTH_OFFSET);
                    size_of::<Value>() * (1 + length as usize)
                }

                tag if tag.feature() == CellFeature::BytevectorLike => {
                    let length = cell.word_ref(CellReference::<Bytevector>::LENGTH_OFFSET);
                    1 * size_of::<Value>() + length as usize
                }
                
                _ => {
                    todo!()
                }
            } + size_of::<SchemeHeader>(),
            8,
            0,
        )
    }

    #[inline(always)]
    pub fn bytes_used(cell: CellReference) -> usize {
        let mut size = Self::raw_bytes_used(cell);

        if cell.header().cell_hash() == HASH_STATE_HASHED_AND_MOVED as u32 {
            size += HASHCODE_SIZE;
        }

        size
    }
    #[inline(always)]
    pub fn bytes_required_when_copied(cell: CellReference) -> usize {
        let mut size = Self::raw_bytes_used(cell);

        if cell.header().cell_hash() != HASH_STATE_UNHASHED as u32 {
            size += HASHCODE_SIZE;
        }

        size
    }

    #[inline(always)]
    fn move_object(
        immut_to_address: Address,
        from_obj: ObjectReference,
        immut_to_obj: ObjectReference,
        num_bytes: usize,
    ) -> ObjectReference {
        let mut to_address = immut_to_address;
        let mut to_obj = immut_to_obj;

        assert!(to_address.is_zero() || to_obj.to_raw_address().is_zero());

        let mut copy_bytes = num_bytes;
        let mut obj_ref_offset = OBJECT_REF_OFFSET;
        let hash_state;

        let cell = CellReference::<()>(from_obj, PhantomData);
        hash_state = cell.header().cell_hash() as usize;

        if hash_state == HASH_STATE_HASHED {
            // We do not copy the hashcode, but we do allocate it
            copy_bytes -= HASHCODE_SIZE;

            if to_obj.to_raw_address().is_zero() {
                // The hashcode is the first word, so we copy to object one word higher
                to_address += HASHCODE_SIZE;
            }
        } else if hash_state == HASH_STATE_HASHED_AND_MOVED {
            // Simple operation (no hash state change), but one word larger header
            obj_ref_offset += HASHCODE_SIZE as isize;
        }

        if !to_obj.to_raw_address().is_zero() {
            to_address = to_obj.to_raw_address() + (-obj_ref_offset);
        }

        // Low memory word of source object
        let from_address = from_obj.to_raw_address() + (-obj_ref_offset);

        unsafe {
            std::ptr::copy_nonoverlapping(
                from_address.to_ptr::<u8>(),
                to_address.to_mut_ptr::<u8>(),
                copy_bytes,
            );
        }

        if to_obj.is_null() {
            to_obj = ObjectReference::from_raw_address(to_address + obj_ref_offset);
        } else {
            debug_assert_eq!(to_obj.to_raw_address(), to_address + obj_ref_offset);
        }


        if hash_state == HASH_STATE_HASHED {
            let cell = CellReference::<()>(from_obj, PhantomData);
            let hashcode = cell.hashcode();
            let to_cell = CellReference::<()>(to_obj, PhantomData);
            to_cell.hashcode_after_copy(hashcode);
            to_cell
                .header_mut()
                .set_cell_hash(HASH_STATE_HASHED_AND_MOVED as _);
        }

        to_obj
    }
    fn get_offset_for_alignment(object: ObjectReference) -> usize {
        let mut offset = OBJECT_REF_OFFSET as usize;
        let cell = CellReference::<()>(object, PhantomData);

        if cell.header().cell_hash() != HASH_STATE_UNHASHED as u32 {
            offset += HASHCODE_SIZE;
        }

        offset
    }

    fn object_start_ref(object: ObjectReference) -> Address {
        let cell = CellReference::<()>(object, PhantomData);

        if cell.header().cell_hash() == HASH_STATE_HASHED_AND_MOVED as u32 {
            return object.to_raw_address() - HASHCODE_SIZE;
        }

        object.to_raw_address()
    }
}

impl ObjectModel<CapyVM> for VMObjectModel {
    const GLOBAL_LOG_BIT_SPEC: mmtk::vm::VMGlobalLogBitSpec = LOGGING_SIDE_METADATA_SPEC;
    const LOCAL_FORWARDING_BITS_SPEC: mmtk::vm::VMLocalForwardingBitsSpec =
        FORWARDING_BITS_METADATA_SPEC;
    const LOCAL_FORWARDING_POINTER_SPEC: mmtk::vm::VMLocalForwardingPointerSpec =
        FORWARDING_POINTER_METADATA_SPEC;
    const LOCAL_LOS_MARK_NURSERY_SPEC: mmtk::vm::VMLocalLOSMarkNurserySpec = LOS_METADATA_SPEC;
    const LOCAL_MARK_BIT_SPEC: mmtk::vm::VMLocalMarkBitSpec = MARKING_METADATA_SPEC;

    const UNIFIED_OBJECT_REFERENCE_ADDRESS: bool = false;
    const VM_WORST_CASE_COPY_EXPANSION: f64 = 1.5;
    const OBJECT_REF_OFFSET_LOWER_BOUND: isize = -(HASHCODE_OFFSET + OBJECT_REF_OFFSET);

    fn ref_to_address(object: mmtk::util::ObjectReference) -> mmtk::util::Address {
        object.to_raw_address()
    }

    fn address_to_ref(addr: mmtk::util::Address) -> mmtk::util::ObjectReference {
        ObjectReference::from_raw_address(addr)
    }

    #[inline(always)]
    fn ref_to_object_start(object: mmtk::util::ObjectReference) -> mmtk::util::Address {
        let cell = CellReference::<()>(object, PhantomData);
        let hash_state = cell.header().cell_hash();

        if hash_state == HASH_STATE_HASHED_AND_MOVED as u32 {
            return object.to_raw_address() + (-(HASHCODE_SIZE as isize + OBJECT_REF_OFFSET));
        }
        object.to_raw_address() + (-OBJECT_REF_OFFSET)
    }

    fn ref_to_header(object: mmtk::util::ObjectReference) -> mmtk::util::Address {
        object.to_raw_address() + (-OBJECT_REF_OFFSET)
    }

    fn get_type_descriptor(_: ObjectReference) -> &'static [i8] {
        unimplemented!()
    }

    fn copy(
        from: ObjectReference,
        semantics: mmtk::util::copy::CopySemantics,
        copy_context: &mut mmtk::util::copy::GCWorkerCopyContext<CapyVM>,
    ) -> ObjectReference {
        let bytes = Self::bytes_required_when_copied(CellReference::<()>(from, PhantomData));
        let align = Self::get_align_when_copied(from);
        let offset = Self::get_align_offset_when_copied(from);
       
        let region = copy_context.alloc_copy(from, bytes, align, offset, semantics);

        let to_obj = Self::move_object(region, from, ObjectReference::NULL, bytes);

        copy_context.post_copy(to_obj, bytes, semantics);
        to_obj
    }

    fn copy_to(
        from: ObjectReference,
        to: ObjectReference,
        region: mmtk::util::Address,
    ) -> mmtk::util::Address {
        let copy = from != to;

        let bytes = if copy {
            let bytes = Self::bytes_required_when_copied(CellReference::<()>(from, PhantomData));
            Self::move_object(unsafe { Address::zero() }, from, to, bytes);
            bytes
        } else {
            Self::bytes_used(CellReference::<()>(from, PhantomData))
        };

        let start = Self::object_start_ref(to);
        fill_alignment_gap::<CapyVM>(region, start);

        start + bytes
    }

    fn get_current_size(object: ObjectReference) -> usize {
        let cell = CellReference(object, PhantomData::<*mut ()>);

        VMObjectModel::bytes_used(cell)
    }

    fn get_align_when_copied(_object: ObjectReference) -> usize {
        align_of::<usize>()
    }

    fn get_size_when_copied(object: ObjectReference) -> usize {
        Self::get_current_size(object)
    }

    fn get_align_offset_when_copied(object: ObjectReference) -> usize {
        Self::get_offset_for_alignment(object)
    }

    fn get_reference_when_copied_to(
        from: ObjectReference,
        to: mmtk::util::Address,
    ) -> ObjectReference {
        let mut res = to;
        let cell = CellReference::<()>(from, PhantomData);

        if cell.header().cell_hash() != HASH_STATE_UNHASHED as u32 {
            res += HASHCODE_SIZE;
        }

        ObjectReference::from_raw_address(res + OBJECT_REF_OFFSET)
    }

    fn dump_object(object: ObjectReference) {
        println!("#<object {:p}>", object.to_raw_address().to_ptr::<()>())
    }
}

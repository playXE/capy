use b3::{BasicBlockBuilder, ValueId};
use rsgc::thread::Thread;

pub fn write_barrier(builder: &mut BasicBlockBuilder, obj: ValueId, thread: ValueId) {
    let off = builder.const64(8);
    let obj = builder.binary(b3::Opcode::Sub, obj, off);
    let cm_in_progress = Thread::cm_in_progress_offset();

    let cm_in_progress = builder.load8z(thread, cm_in_progress as i32, None, None);
    let done = builder.procedure.add_block(1.0);
    let check_ssb = builder.procedure.add_block(1.0);
    let do_wb = builder.procedure.add_block(1.0);
    let slow = builder.procedure.add_block(1.0);
    let zero = builder.const32(0);
    let not_in_progress = builder.binary(b3::Opcode::Equal, zero, cm_in_progress);

    builder.branch(not_in_progress, done, (check_ssb, b3::Frequency::Normal));

    builder.block = check_ssb;

    let index_offset = Thread::satb_index_offset();

    let index = builder.load(b3::Type::Int64, thread, index_offset as i32, None, None);
    let zero64 = builder.const64(0);
    let cmp = builder.binary(b3::Opcode::NotEqual, index, zero64);
    builder.branch(cmp, do_wb, (slow, b3::Frequency::Rare));

    builder.block = do_wb;

    let buffer_offset = Thread::satb_buffer_offset();

    let one = builder.const64(1);
    let new_index = builder.binary(b3::Opcode::Sub, index, one);

    let buffer = builder.load(b3::Type::Int64, thread, buffer_offset as i32, None, None);
    let entry = builder.binary(b3::Opcode::Add, buffer, new_index);
    builder.store(obj, entry, 0, None, None);
    builder.store(new_index, thread, index_offset as i32, None, None);
    builder.jump(Some(done));

    builder.block = slow;

    let func = builder.const64(write_barrier_slow as i64);
    builder.ccall(b3::Type::Void, func, &[thread, obj], b3::Effects::for_call());
    builder.jump(Some(done));

    builder.block = done;
}

unsafe extern "C" fn write_barrier_slow(thread: &mut Thread, obj: *mut u8) {
    thread.flush_ssb();
    thread.raw_write_barrier(obj.cast());
}

use std::mem::size_of;
use std::ptr::addr_of;

use super::encode::*;
use super::opcodes::*;
use std::fmt::Write;

pub fn disassemble<T: std::fmt::Write>(code: &[u8], f: &mut T) -> std::fmt::Result {
    let mut stream = code.as_ptr();

    unsafe {
        let end = stream.add(code.len());

        while stream < end {
            let start_read = stream;
            let op = stream.read();
            stream = stream.add(1);
            let mut tmp = String::new();

            match op {
                OP_BRANCH => {
                    let op = OpBranch::read(stream);
                    stream = stream.add(size_of::<OpBranch>());
                    let next = stream.offset(addr_of!(op.offset).read_unaligned() as isize);
                    let offset_from_start = next as isize - code.as_ptr() as isize;
                    write!(
                        tmp,
                        "branch {}(=>{})",
                        addr_of!(op.offset).read_unaligned(),
                        offset_from_start
                    )?;
                }

                OP_BRANCH_IF => {
                    let op = OpBranchIf::read(stream);
                    stream = stream.add(size_of::<OpBranchIf>());
                    let next = stream.offset(addr_of!(op.offset).read_unaligned() as isize);
                    let offset_from_start = next as isize - code.as_ptr() as isize;
                    write!(
                        tmp,
                        "branch.true {}, {}(=>{})",
                        addr_of!(op.src).read_unaligned(),
                        addr_of!(op.offset).read_unaligned(),
                        offset_from_start
                    )?;
                }
                OP_BRANCH_IF_NOT => {
                    let op = OpBranchIfNot::read(stream);
                    stream = stream.add(size_of::<OpBranchIfNot>());
                    let next = stream.offset(addr_of!(op.offset).read_unaligned() as isize);
                    let offset_from_start = next as isize - code.as_ptr() as isize;
                    write!(
                        tmp,
                        "branch.false {}, {}(=>{})",
                        addr_of!(op.src).read_unaligned(),
                        addr_of!(op.offset).read_unaligned(),
                        offset_from_start
                    )?;
                }
                _ => {
                    disassemble_from_stream(op, &mut stream, &mut tmp)?;
                }
            }

            let start_read_ix = start_read as usize - code.as_ptr() as usize;
            let end_read_ix = stream as usize - code.as_ptr() as usize;

            let bytes = &code[start_read_ix..end_read_ix];

            let bytes = bytes
                .iter()
                .map(|x| format!("{:x}", x))
                .collect::<Vec<_>>()
                .join(" ");

            writeln!(
                f,
                "{:0w$}: {:<24} {}",
                start_read_ix,
                bytes,
                tmp,
                w = code.len().to_string().len(),
            )?;
        }
    }

    Ok(())
}

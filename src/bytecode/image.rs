#![allow(dead_code)]
use crate::{
    gc::virtual_memory::{PlatformVirtualMemory, VirtualMemory},
    runtime::{
        environment::environment_get_cell,
        fasl::FASLReader,
        object::{
            scm_car, scm_cdr, scm_symbol_str, scm_vector_as_slice, scm_vector_length,
            scm_vector_ref, ScmProgram,
        },
        symbol::scm_intern,
        value::Value,
    },
    vm::{scm_virtual_machine, sync::mutex::RawMutex, thread::Thread},
};
use mmtk::{
    memory_manager::object_reference_write,
    util::Address,
    vm::{edge_shape::SimpleEdge, RootsWorkFactory},
};
use std::{io::Cursor, mem::transmute, sync::Arc};

use super::opcodes::{disassemble, CAPY_BYTECODE_MAGIC};

pub struct Image {
    pub code: Vec<u8>,
    pub constants: Value,
    pub data: Value,
    pub programs: Vec<Value>,
    pub debug_offsets: Value,
    pub entry_program: Value,
}

impl Image {
    pub(crate) unsafe fn visit_roots(&self, factory: &mut impl RootsWorkFactory<SimpleEdge>) {
        let edge = SimpleEdge::from_address(Address::from_ptr(&self.constants));
        let data = SimpleEdge::from_address(Address::from_ptr(&self.data));
        let entry = SimpleEdge::from_address(Address::from_ptr(&self.entry_program));
        for program in self.programs.iter() {
            let program = SimpleEdge::from_address(Address::from_ptr(program));
            factory.create_process_edge_roots_work(vec![program]);
        }
        let debug = SimpleEdge::from_address(Address::from_ptr(&self.debug_offsets));
        factory.create_process_edge_roots_work(vec![edge, data, entry, debug]);
    }

    pub fn disassemble(&self) {
        println!("Image at {:p}", self);
        println!("Code section:");
        for i in 0..scm_vector_length(self.data) {
            let data = scm_vector_ref(self.data, i);

            let start = scm_vector_ref(data, 0).get_int32();
            let end = scm_vector_ref(data, 1).get_int32();
            let nargs = scm_vector_ref(data, 2);
            let variadic = scm_vector_ref(data, 3);
            let name = scm_vector_ref(data, 4);

            // do disassembly:
            {
                let code = &self.code[start as usize..end as usize];
                println!(
                    "program at {:p}, '{}' (nargs: {}, variadic? {}):",
                    code.as_ptr(),
                    if name.is_symbol() {
                        scm_symbol_str(name)
                    } else {
                        "<unnamed>"
                    },
                    nargs.get_int32(),
                    variadic.get_bool()
                );
                disassemble::<true>(code);
            }
        }
        //println!("Constants section:");
        {
            for _ in 0..scm_vector_length(self.constants) {}
        }
    }
}

pub fn load_image_from_memory(
    memory: &[u8],
    resolver: Option<&dyn Fn(Value) -> Value>,
) -> Result<Arc<Image>, Value> {
    unsafe {
        mmtk::memory_manager::disable_collection(&scm_virtual_machine().mmtk);
        let magic = u32::from_le_bytes([memory[0], memory[1], memory[2], memory[3]]);
        if magic != CAPY_BYTECODE_MAGIC {
            return Err(scm_intern("invalid-image"));
        }

        let code_len = u32::from_le_bytes([memory[4], memory[5], memory[6], memory[7]]) as usize;

        let code = memory[8..8 + code_len].to_vec();

        let constants = &memory[8 + code_len..];

        let mut cursor = Cursor::new(constants);
        let mut reader = FASLReader::<true, _>::new(
            &mut cursor,
            Some(&code),
            resolver.unwrap_or_else(|| {
                &|name| {
                    environment_get_cell(scm_virtual_machine().interaction_environment, name)
                        .unwrap()
                }
            }),
        );
        reader.get_lites().unwrap();
        let section = reader.get_datum().unwrap();

        let constants = scm_vector_ref(section, 0);
        let data = scm_vector_ref(section, 1);
        let entry_program = scm_vector_ref(section, 2);
        let debug_offsets = scm_vector_ref(section, 3);

        for program in reader.programs.iter_mut() {
            let edge = SimpleEdge::from_address(Address::from_mut_ptr(
                &mut program.cast_as::<ScmProgram>().constants,
            ));

            object_reference_write(
                Thread::current().mutator(),
                transmute(*program),
                edge,
                transmute(constants),
            );

            assert!(
                program.cast_as::<ScmProgram>().constants == constants
                    && program.cast_as::<ScmProgram>().constants.is_vector()
            );
        }

        let image = Arc::new(Image {
            data,
            constants,
            entry_program,
            debug_offsets,
            programs: std::mem::take(&mut reader.programs),
            code,
        });

        let vm = scm_virtual_machine();

        vm.images.lock.lock(true);
        vm.images.images.push(image.clone());
        vm.images.lock.unlock();
        mmtk::memory_manager::enable_collection(&scm_virtual_machine().mmtk);
        if scm_virtual_machine().disassemble {
            image.disassemble();
        }

        Ok(image)
    }
}

/// This function leaks memory it allocates.
fn alloc_aligned(len: usize, alignment: usize) -> *mut u8 {
    if alignment == 8 {
        unsafe { libc::malloc(len) as *mut u8 }
    } else if alignment == VirtualMemory::<PlatformVirtualMemory>::page_size() {
        let mem =
            VirtualMemory::<PlatformVirtualMemory>::allocate_aligned(len, alignment, false, "ELF")
                .expect("Failed to allocate memory for ELF");
        let ptr = mem.address();
        std::mem::forget(mem);
        ptr
    } else {
        unsafe {
            let mut ret = libc::malloc(len + alignment - 1) as *mut u8;

            if ret.is_null() {
                eprintln!("Failed to allocate memory for ELF");
                std::process::exit(1);
            }

            ret = align(ret as usize, alignment) as *mut u8;

            return ret;
        }
    }
}

#[allow(dead_code)]
const fn is_aligned(x: usize, align: usize) -> bool {
    (x & (align - 1)) == 0
}

const fn align(x: usize, align: usize) -> usize {
    (x + align - 1) & !(align - 1)
}

pub struct ImageRegistry {
    pub lock: RawMutex,
    pub images: Vec<Arc<Image>>,
}

impl ImageRegistry {
    pub fn new() -> ImageRegistry {
        ImageRegistry {
            lock: RawMutex::INIT,
            images: Vec::new(),
        }
    }

    pub(crate) unsafe fn visit_roots(&self, mut factory: impl RootsWorkFactory<SimpleEdge>) {
        for image in self.images.iter() {
            image.visit_roots(&mut factory);
        }
    }

    pub fn lookup_image(&self, ip: *const u8) -> Option<Arc<Image>> {
        for image in self.images.iter() {
            let start = image.code.as_ptr();
            let end = unsafe { start.add(image.code.len() as usize) };
            if ip >= start && ip < end {
                return Some(image.clone());
            }
        }
        None
    }

    /// Searches for image where `ip` is located, and then searches
    /// for debug offset for `ip` in that image.
    ///
    /// Returns `(<filename> <line> <column>)` if found, `None` otherwise.
    pub fn debug_info(&self, ip: *const u8) -> Option<(Value, u32, u32)> {
        let image = self.lookup_image(ip)?;

        let debug = image.debug_offsets;
        let target_pc = (ip as usize - image.code.as_ptr() as usize) as u32;

        let slice = scm_vector_as_slice(debug);

        let high = slice.partition_point(|data| scm_car(*data).get_int32() as u32 <= target_pc);
      
        for data in slice[..high].iter().rev() {
            let offset = scm_car(*data);
            let vector = scm_cdr(*data);
            let pc = offset.get_int32() as u32;
           
            if pc <= target_pc {
                return Some((
                    scm_vector_ref(vector, 0),
                    scm_vector_ref(vector, 1).get_int32() as u32,
                    scm_vector_ref(vector, 2).get_int32() as u32,
                ));
            }
        }

        None
    }
}

use crate::{
    gc::virtual_memory::{PlatformVirtualMemory, VirtualMemory},
    runtime::{object::ScmCellRef, symbol::scm_intern, value::Value},
    vm::{scm_virtual_machine, sync::mutex::RawMutex, thread::Thread},
};
use mmtk::{
    util::Address,
    vm::{edge_shape::SimpleEdge, RootsWorkFactory},
};
use std::{
    collections::HashMap,
    mem::size_of,
    sync::{atomic::AtomicU32, Arc},
};

pub struct DebugEntry {
    pub funcname: u32,
    pub line: u32,
    pub column: u32,
    pub file: u32,
}

pub struct Image {
    pub code_size: u32,
    pub data_size: u32,
    pub strtab_size: u32,
    pub dbg_size: u32,
    pub initialized_size: AtomicU32,
    pub entry: u32,
    pub entry_program: Value,
    pub image: *mut u8,
    pub debuginfo: Vec<(u32, DebugEntry)>,
}

impl Image {
    pub fn data(&self) -> *mut u8 {
        unsafe {
            self.image
                .add(self.code_size as usize + self.strtab_size as usize)
        }
    }

    pub fn strtab(&self) -> *mut u8 {
        unsafe { self.image.add(self.code_size as usize) }
    }

    pub fn string_at(&self, offset: u32) -> &str {
        unsafe {
            let mut ptr = self.strtab().add(offset as usize);
            let len = ptr.cast::<u32>().read();
            ptr = ptr.add(4);

            std::str::from_utf8_unchecked(std::slice::from_raw_parts(ptr, len as usize))
        }
    }

    pub fn bvector_at(&self, offset: u32) -> &[u8] {
        unsafe {
            let mut ptr = self.strtab().add(offset as usize);
            let len = ptr.cast::<u32>().read();
            ptr = ptr.add(4);

            std::slice::from_raw_parts(ptr, len as usize)
        }
    }

    pub fn code(&self) -> *mut u8 {
        self.image
    }

    pub(crate) unsafe fn visit_roots(&self, factory: &mut impl RootsWorkFactory<SimpleEdge>) {
        let mut cursor = self.data();
        let end = cursor.add(
            self.initialized_size
                .load(std::sync::atomic::Ordering::SeqCst) as usize
                * size_of::<Value>(),
        );
        let mut edges = Vec::new();
        edges.push(SimpleEdge::from_address(Address::from_ptr(
            &self.entry_program,
        )));
        while cursor < end {
            let object = cursor.cast::<ScmCellRef>();

            let edge = SimpleEdge::from_address(Address::from_ptr(object));
            edges.push(edge);
            cursor = cursor.add(size_of::<Value>());
        }

        factory.create_process_edge_roots_work(edges);
    }
}

const MAGIC: u32 = 0x7f454c46;

pub fn load_image_from_memory(
    memory: &[u8],
    resolver: Option<&dyn Fn(&str) -> Result<Value, Value>>,
) -> Result<Arc<Image>, Value> {
    let magic = u32::from_le_bytes([memory[0], memory[1], memory[2], memory[3]]);
    if MAGIC != magic {
        return Err(Thread::current()
            .make_string::<false>("Invalid image")
            .into());
    }
    let code_size = u32::from_le_bytes([memory[4], memory[5], memory[6], memory[7]]);
    let data_size = u32::from_le_bytes([memory[8], memory[9], memory[10], memory[11]]);
    let strtab_size = u32::from_le_bytes([memory[12], memory[13], memory[14], memory[15]]);
    let entry = u32::from_le_bytes([memory[16], memory[17], memory[18], memory[19]]);
    let dbg_size = u32::from_le_bytes([memory[20], memory[21], memory[22], memory[23]]);

    let image_start = &memory[24..];

    let map = alloc_aligned(image_start.len(), 8);

    unsafe {
        map.copy_from_nonoverlapping(image_start.as_ptr(), image_start.len());

        let entry_program = Thread::current().make_program::<true>(map.add(entry as usize), 0);

        let image = Arc::new(Image {
            code_size,
            data_size,
            strtab_size,
            dbg_size,
            entry_program: entry_program.into(),
            initialized_size: AtomicU32::new(0),
            entry,
            image: map,
            debuginfo: Vec::new(),
        });

        let vm = scm_virtual_machine();

        vm.images.lock.lock(true);
        vm.images.images.push(image.clone());
        vm.images.lock.unlock();

        let mut strings = HashMap::with_capacity(128);

        // phase 1: resolve strings
        {
            let mut cursor = image.strtab();
            let end = cursor.add(image.strtab_size as usize);
            while cursor < end {
                let offset = cursor.cast::<u32>().read();
                let string = image.string_at(offset);
                strings.insert(offset, string);
                cursor = cursor.add(4 + string.len());
            }
        }

        // phase 2: build constants to proper state
        {
            let mut cursor = image.data();
            let end = cursor.add(image.data_size as usize);
            while cursor < end {
                let raw = cursor.cast::<(u32, u32)>().read();

                match raw.0 {
                    0x00 => {
                        let string = image.string_at(raw.1);
                        let string = Thread::current().make_string::<true>(string);
                        cursor
                            .cast::<Value>()
                            .write(string);
                    }

                    0x01 => {
                        let symbol = image.string_at(raw.1);
                        let symbol = scm_intern(symbol);
                        cursor.cast::<Value>().write(symbol);
                    }

                    0x02 => {
                        let bvector = image.bvector_at(raw.1);
                        let bvector = Thread::current().make_bytevector_from_slice::<true>(bvector);
                        cursor
                            .cast::<Value>()
                            .write(bvector);
                    }

                    0x03 => {
                        let name = image.string_at(raw.1);

                        let gloc = if let Some(resolver) = resolver {
                            resolver(name)?
                        } else {
                            Thread::current()
                                .make_gloc(scm_intern(name), Value::encode_undefined_value())
                        };

                        cursor.cast::<Value>().write(gloc);
                    }

                    0x04 => {
                        let ix = raw.1;

                        let code = image.code().add(ix as usize);
                        let program = Thread::current().make_program::<true>(code, 0);
                        cursor
                            .cast::<Value>()
                            .write(program);
                    }

                    _ => {
                        return Err(Thread::current()
                            .make_string::<false>("Invalid image")
                            .into())
                    }
                }
                image
                    .initialized_size
                    .fetch_add(1, std::sync::atomic::Ordering::SeqCst);
                cursor = cursor.add(size_of::<Value>());
            }
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

    pub(crate) unsafe fn visit_roots(&self, factory: &mut impl RootsWorkFactory<SimpleEdge>) {
        for image in self.images.iter() {
            image.visit_roots(factory);
        }
    }

    pub fn lookup_image(&self, pc: *const u8) -> Option<Arc<Image>> {
        for image in self.images.iter() {
            let start = image.code();
            let end = unsafe { start.add(image.code_size as usize) };
            if pc >= start && pc < end {
                return Some(image.clone());
            }
        }
        None
    }
}

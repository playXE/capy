//! # The Shadow Stack
//!
//! Unlike many GC algorithms which rely on a cooperative code generator to compile stack maps, this algorithm
//! carefully maintains a vector of stack roots. This so-called "shadow stack" mirrors the machine stack.
//! Maintaining this data structure is slower than using a stack map compiled into the executable as constant data,
//! but has a significant portability advantage because it requires no special support from the target code generator,
//! and does not require tricky platform-specific code to crawl the machine stack.

use std::intrinsics::unlikely;

use mmtk::{util::Address, vm::RootsWorkFactory};

use crate::runtime::value::Value;

use super::ObjEdge;

/// Implementation of shadow-stack that uses separate stack for roots.
/// Roots are appended to the stack before operation that may trigger GC and
/// restored after the operation.
///
/// Before we used shadow-stack that was implemented as a linked list of roots.
/// It is significantly slower and produces non-optimal code while this code
/// is a simple push/pop to separate stack.
///
/// This stack is automatically resized if it is not big enough to hold all roots.
/// Default size is 1024 values.
///
pub struct ShadowStack {
    root_stack_base: *mut Value,
    root_stack_top: *mut Value,
    root_stack_limit: *mut Value,
    root_stack_memory: *mut Value,
    root_stack_size: usize,
}

impl ShadowStack {
    pub const fn new() -> Self {
        Self {
            root_stack_base: std::ptr::null_mut(),
            root_stack_top: std::ptr::null_mut(),
            root_stack_limit: std::ptr::null_mut(),
            root_stack_memory: std::ptr::null_mut(),
            root_stack_size: 0,
        }
    }

    pub(crate) fn init(&mut self) {
        self.root_stack_size = 1024;
        self.root_stack_memory = {
            mmtk::memory_manager::malloc(self.root_stack_size * std::mem::size_of::<Value>())
                .to_mut_ptr::<Value>()
        };
        self.root_stack_base = self.root_stack_memory;
        self.root_stack_top = self.root_stack_memory;
        self.root_stack_limit = unsafe { self.root_stack_memory.add(self.root_stack_size) };
    }
    #[cold]
    fn expand(&mut self) {
        let root_stack_size = (self.root_stack_size as f64 * 1.3) as usize;

        unsafe {
            let new_root_stack_memory =
                mmtk::memory_manager::malloc(root_stack_size * std::mem::size_of::<Value>())
                    .to_mut_ptr::<Value>();

            std::ptr::copy_nonoverlapping(
                self.root_stack_memory,
                new_root_stack_memory,
                self.root_stack_size,
            );

            mmtk::memory_manager::free(Address::from_mut_ptr(self.root_stack_memory));

            self.root_stack_memory = new_root_stack_memory;
            self.root_stack_size = root_stack_size;

            self.root_stack_base = self.root_stack_memory;
            self.root_stack_top = self.root_stack_base;
            self.root_stack_limit = self.root_stack_base.add(self.root_stack_size);
        }
    }
    #[inline(always)]
    pub fn push_to_save(&mut self, value: Value) {
        unsafe {
            if unlikely(self.root_stack_top == self.root_stack_limit) {
                self.expand();
            }
            self.root_stack_top.write(value);
            self.root_stack_top = self.root_stack_top.add(1);
        }
    }

    #[inline(always)]
    pub fn pop_to_restore(&mut self) -> Value {
        unsafe {
            self.root_stack_top = self.root_stack_top.sub(1);
            let value = self.root_stack_top.read();

            value
        }
    }

    /// Walk all roots in this shadow-stack.
    ///
    /// # Safety
    ///
    /// Should be invoked only by GC code.
    pub unsafe fn walk_roots(&mut self, factory: &mut impl RootsWorkFactory<ObjEdge>) {
        let mut edges = Vec::with_capacity(64);
        let mut ptr = self.root_stack_base;
        while ptr < self.root_stack_top {
            if (*ptr).is_object() {
                edges.push(ObjEdge::from_address(Address::from_mut_ptr(ptr)));
            }
            if edges.len() > 64 {
                factory.create_process_edge_roots_work(std::mem::take(&mut edges));
            }

            ptr = ptr.add(1);
        }

        if !edges.is_empty() {
            factory.create_process_edge_roots_work(edges);
        }
    }

    pub fn offset_for_save(&self) -> usize {
        unsafe { self.root_stack_top.offset_from(self.root_stack_base) as usize }
    }

    pub unsafe fn restore_from_offset(&mut self, offset: usize) {
        self.root_stack_top = self.root_stack_base.add(offset);
    }
}

/// Pushes variables onto the shadow-stack and pops them once expression is evaluated.
///
/// # Example
///
/// ```text
/// let mut x = Value::encode_int32(42);
/// gc_protect!(Thread::current() => x => { println!("GC might happen here!") });
///
/// ```
#[macro_export]
macro_rules! gc_protect {
    ($thread: expr => $($var: ident),* => $e: expr) => {
        {
            $(
                $thread.shadow_stack.push_to_save($var);
            )*

            let result = {
                let mut guard = $crate::gc::shadow_stack::DropGuard::new(|| {
                    let t = $crate::vm::thread::Thread::current();
                    $(
                        stringify!($var);
                        let _ =  t.shadow_stack.pop_to_restore();
                    )*
                });
                let result = $e;
                guard.guard.take();
                result
            };

            gc_protect!(@pop $thread; ($($var),*) -> ());
            result
        }
    };

    (@pop $thread: expr; () -> ($($reversed: ident),*)) => {
        $(
            #[allow(unused_assignments)]
            {
                $reversed = $thread.shadow_stack.pop_to_restore();
            }
        )*
    };

    (@pop $thread: expr; ($first: ident $(, $rest:ident)*) -> ($($reversed: ident),*)) => {
        gc_protect!(@pop $thread; ($($rest),*) -> ($first$(, $reversed)*));
    }
}

pub struct DropGuard<T, F>
where
    F: FnOnce() -> T,
{
    pub guard: Option<F>,
}

impl<T, F> DropGuard<T, F>
where
    F: FnOnce() -> T,
{
    pub fn new(guard: F) -> Self {
        Self { guard: Some(guard) }
    }
}

impl<T, F> Drop for DropGuard<T, F>
where
    F: FnOnce() -> T,
{
    fn drop(&mut self) {
        if let Some(guard) = self.guard.take() {
            let _ = guard();
        }
    }
}

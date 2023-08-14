//! # The Shadow Stack
//!
//! Unlike many GC algorithms which rely on a cooperative code generator to compile stack maps, this algorithm
//! carefully maintains a linked list of stack roots. This so-called "shadow stack" mirrors the machine stack.
//! Maintaining this data structure is slower than using a stack map compiled into the executable as constant data,
//! but has a significant portability advantage because it requires no special support from the target code generator,
//! and does not require tricky platform-specific code to crawl the machine stack.

use super::*;
use std::intrinsics::unlikely;
use std::ops::{Deref, DerefMut};
use std::{marker::PhantomData, ptr::NonNull};

/// The map for a single function's stack frame. It is compiled as a constant
/// for each invocation of [`gc_frame!`](crate::gc_frame).
#[repr(C)]
pub struct FrameMap<const N: usize> {
    pub num_roots: u32,
}

impl<const N: usize> FrameMap<N> {
    #[inline]
    pub const fn new(num_roots: u32, meta: [*const u8; N]) -> FrameMap<N> {
        Self { num_roots }
    }

    #[inline]
    pub const fn as_unsized(this: *const Self) -> *const FrameMap<0> {
        this.cast()
    }
}

/// A link in the dynamic shadow stack. One of these is embedded in
/// the stack frame of each function on the call stack.
#[repr(C)]
pub struct StackEntry<const N: usize> {
    pub next: *mut StackEntry<0>,
    pub map: NonNull<FrameMap<0>>,
    pub roots: [*const u8; N],
}

impl<const N: usize> StackEntry<N> {
    #[inline]
    pub const unsafe fn new<const M: usize>(
        next: *mut StackEntry<0>,
        map: *const FrameMap<M>,
        roots: [*const u8; N],
    ) -> Self {
        Self {
            next,
            map: NonNull::new_unchecked(FrameMap::as_unsized(map) as _),
            roots,
        }
    }
    pub fn roots(&mut self) -> &mut [*const u8] {
        unsafe {
            let num_roots = self.map.as_ref().num_roots;
            let roots = std::slice::from_raw_parts_mut(self.roots.as_mut_ptr(), num_roots as _);
            roots
        }
    }

    #[inline]
    pub const fn as_unsized(this: *const Self) -> *const StackEntry<0> {
        this.cast()
    }
}

pub type StackChain = *mut StackEntry<0>;

/// Pushes stack entry into stack entry chain
#[inline]
pub unsafe fn push_gcframe(root: &mut StackChain, frame: *mut StackEntry<0>) {
    *root = frame;
}
/// Pops stack entry from stack entry chain
#[inline]
pub unsafe fn pop_gcframe(root: &mut StackChain, frame: *mut StackEntry<0>) {
    debug_assert!(!root.is_null());
    debug_assert_eq!(*root, frame);
    *root = (**root).next;
}

/// Visits each stack entry registered in `root` chain.
pub unsafe fn visit_roots(root: StackChain, edges: &mut Vec<SimpleEdge>) {
    let mut entry = root;

    while !entry.is_null() {
        let roots = (*entry).roots();

        for (i, root) in roots.iter().copied().enumerate() {
            let ptr = root.cast::<Value>();

            if (*ptr).is_object() {
                let edge = SimpleEdge::from_address(Address::from_ptr(ptr));

                edges.push(edge);
            }
        }

        entry = (*entry).next;
    }
}

/// Simple struct that holds stack entry and entire stack chain. It automatically
/// pops its stack entry from stack chain when dropped
pub struct GcFrameRegistration<'a> {
    frame: *mut StackEntry<0>,
    chain: *mut StackChain,
    _phantom: PhantomData<&'a StackEntry<0>>,
}

impl<'a> GcFrameRegistration<'a> {
    #[inline(always)]
    pub fn new<const N: usize>(chain: *mut StackChain, frame: &'a StackEntry<N>) -> Self {
        let frame = StackEntry::as_unsized(frame) as *mut StackEntry<0>;
        unsafe {
            push_gcframe(&mut *chain, frame);
        }
        Self {
            frame,
            chain,
            _phantom: PhantomData,
        }
    }
}
impl<'a> Drop for GcFrameRegistration<'a> {
    #[inline(always)]
    fn drop(&mut self) {
        unsafe {
            pop_gcframe(&mut *self.chain, self.frame);
        }
    }
}

pub struct Rooted {
    pub value: *mut Value,
}

impl Rooted {
    pub fn as_ref(&self) -> &Value {
        unsafe { &*self.value }
    }

    pub fn as_mut(&mut self) -> &mut Value {
        unsafe { &mut *self.value }
    }

    pub fn get_copy(&self) -> Value {
        unsafe { self.value.read() }
    }

    pub fn set(&mut self, value: Value) -> Value {
        std::mem::replace(self.as_mut(), value)
    }
}

#[macro_export]
macro_rules! count {
    () => (0usize);
    ( $x:tt $($xs:tt)* ) => (1usize + $crate::count!($($xs)*));
}

unsafe impl<const N: usize> Send for FrameMap<N> {}
unsafe impl<const N: usize> Sync for FrameMap<N> {}

/// Constructs frame of GCed variables on stack. All variables remain rooted until end of the scope.
#[macro_export]
macro_rules! gc_frame {
    ($chain: expr => $($i: ident = $e: expr),*) => {
        $(
            let mut $i = $e;
        )*

        let frame_map = {
            const __ROOT_COUNT: usize = $crate::count!($($i)*);
            $crate::gc::shadow_stack::FrameMap::new(__ROOT_COUNT as u32,[
            $(
                &$e as *const _ as *const u8
            ),*
        ]) };
        let stack_entry = {
            #[allow(unused_unsafe)]
            unsafe {$crate::gc::shadow_stack::StackEntry::new(*$chain, &frame_map, [
                $( &mut $i as *mut _ as *mut u8 ),*
            ])}
        };
        #[allow(unused_unsafe)]
        let _stack_entry_registration = unsafe{$crate::gc::shadow_stack::GcFrameRegistration::new($chain,&stack_entry)};

        $crate::gc_frame!(@parse stack_entry, 0, $($i)*);
    };
    ($chain: expr => $($i: ident: $t: ty),*) => {
        let stack_entry ={
            const __ROOT_COUNT: usize = $crate::count!($($i)*);
            static __FRAME_MAP: $crate::memory::roots::FrameMap<{__ROOT_COUNT}>  = $crate::memory::roots::FrameMap::new(__ROOT_COUNT as u32,[
                $(
                    $crate::memory::roots::get_root_meta_of::<$t>()
                ),*
            ]);

            #[allow(unused_unsafe)]
            unsafe {$crate::memory::roots::StackEntry::new(*$chain, &__FRAME_MAP, [
                $( &mut $i as *mut _ as *mut u8 ),*
            ])}
        };

        #[allow(unused_unsafe)]
        let _stack_entry_registration = unsafe{$crate::memory::roots::GcFrameRegistration::new($chain,&stack_entry)};

        $crate::gc_frame!(@parse stack_entry, 0, $($i)*);
    };
    (@parse $stack_entry: ident, $n: expr, $i: ident $($is: ident)*) => {
        #[allow(unused_unsafe,unused_mut)]
        let mut $i = unsafe {$crate::gc::shadow_stack::gcroot_of_type(($stack_entry).roots[$n], &$i)};
        $crate::gc_frame!(@parse $stack_entry, ($n + 1), $($is)*)
    };

    (@parse $stack_entry: ident, $n: expr,) => {};
}

pub unsafe fn gcroot_of_type(ptr: *const u8, _to: &Value) -> Rooted {
    Rooted {
        value: ptr as *const *const u8 as *mut Value,
    }
}

impl Deref for Rooted {
    type Target = Value;
    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

impl DerefMut for Rooted {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.as_mut()
    }
}

impl std::fmt::Pointer for Rooted {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Rooted({:p})", self.value)
    }
}

/// Implementation of shadow-stack that uses separate stack for roots.
/// Roots are appended to the stack before operation that may trigger GC and
/// restored after the operation. This stack in reality is way more cheaper than 
/// [`gc_frame!`] that uses native stack to create shadow-stack frames. This is due
/// to the fact that 1) we do not consume native stack memory and do not mess with 
/// references to stack variables and 2) compiler is better to generate better code
/// becaose of 1). 
/// 
/// This stack is automatically resized if it is not big enough to hold all roots. 
/// Default size is 1024 values.
pub struct ShadowStack {
    root_stack_base: *mut Value,
    root_stack_top: *mut Value,
    root_stack_limit: *mut Value,
    root_stack_memory: *mut Value,
    root_stack_size: usize 
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
            mmtk::memory_manager::malloc(
                self.root_stack_size * std::mem::size_of::<Value>(),
            ).to_mut_ptr::<Value>()
        };
        self.root_stack_base = self.root_stack_memory;
        self.root_stack_top = self.root_stack_memory;
        self.root_stack_limit = unsafe {
            self.root_stack_memory.add(self.root_stack_size)
        };
    }   
    #[cold]
    fn expand(&mut self) {
        let root_stack_size = (self.root_stack_size as f64 * 1.3) as usize;

        unsafe {
            let new_root_stack_memory = mmtk::memory_manager::malloc(
                root_stack_size * std::mem::size_of::<Value>(),
            ).to_mut_ptr::<Value>();

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
    pub unsafe fn walk_roots(&mut self, factory: &mut impl RootsWorkFactory<SimpleEdge>) {

        let mut edges = Vec::with_capacity(64);
        let mut ptr = self.root_stack_base;
        while ptr < self.root_stack_top {   
            edges.push(SimpleEdge::from_address(Address::from_mut_ptr(ptr)));
            if edges.len() > 64 {
                factory.create_process_edge_roots_work(std::mem::take(&mut edges));
            }
            ptr = ptr.add(1);
        }

        if !edges.is_empty() {
            factory.create_process_edge_roots_work(edges);
        }
    }
}

/// Pushes variables onto the shadow-stack and pops them once expression is evaluated.
/// 
/// # Example
/// 
/// ```rust
/// 
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
            let result = $e;

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
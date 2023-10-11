/*use crate::{gc::shadow_stack::ShadowStack, interpreter::InterpreterState, vm::thread::Thread};
use generator::Generator;
use intrusive_collections::intrusive_adapter;
use intrusive_collections::LinkedList;
use intrusive_collections::LinkedListLink;
use intrusive_collections::UnsafeRef;
intrusive_adapter! {
    pub AllCoroutinesAdapter = UnsafeRef<CoroutineState>: CoroutineState {
        all_coroutines_link: LinkedListLink
    }
}

pub struct CoroutineState {
    pub interpreter: InterpreterState,
    pub shadow_stack: ShadowStack,
    /// Thread that runs the current coroutine.
    pub(crate) current_thread: *mut Thread,
    pub(crate) generator: Generator<'static, (), ()>,
    pub(crate) all_coroutines_link: LinkedListLink,
}
*/

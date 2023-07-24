use std::sync::atomic::AtomicUsize;

/// RefStorage supports management of off-heap references to objects allocated
/// in the MMTk heap.  An RefStorage object provides a set of Scheme object
/// references (ref values), which clients refer to via oop* handles to the
/// associated OopStorage entries.  Clients allocate entries to create a
/// (possibly weak) reference to a Scheme object, use that reference, and release
/// the reference when no longer needed.
///
/// The garbage collector must know about all OopStorage objects and their
/// reference strength.  OopStorage provides the garbage collector with support
/// for iteration over all the allocated entries.
///
/// There are several categories of interaction with an OopStorage object.
///
/// (1) allocation and release of entries, by the mutator or the VM.
/// (2) iteration by the garbage collector, possibly concurrent with mutator.
/// (3) iteration by other, non-GC, tools (only at safepoints).
/// (4) cleanup of unused internal storage, possibly concurrent with mutator.
///
/// A goal of RefStorage is to make these interactions thread-safe, while
/// minimizing potential lock contention issues within and between these
/// categories.  In particular, support for concurrent iteration by the garbage
/// collector, under certain restrictions, is required.  Further, it must not
/// block nor be blocked by other operations for long periods.
///
/// Internally, RefStorage is a set of Block objects, from which entries are
/// allocated and released.  A block contains an oop[] and a bitmask indicating
/// which entries are in use (have been allocated and not yet released).  New
/// blocks are constructed and added to the storage object when an entry
/// allocation request is made and there are no blocks with unused entries.
/// Blocks may be removed and deleted when empty.
///

pub struct RefStroage {
 
}
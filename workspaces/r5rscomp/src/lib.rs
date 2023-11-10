//! R5RS Compiler Library.
//!
//! Simple R5RS Scheme compiler. Compiles to CapyScheme bytecode, it supports all features of R5RS except `syntax-case` macros,
//! instead `syntax-rules` macros are supported. The design of this compiler is to be as simple as possible yet produce good quality code
//! and at the same time to not depend on GC or any other automatic memory management. We do not want compile-time code execution here.
//!
//! Defines TreeIL for optimization passes, expander and other utilities to help you compile R5RS Scheme. This library can be used for all kinds
//! of Scheme implementations, it does not depend on any particular runtime.
//! 
//! 
//! # Compilation Passes
//! 
//! 1. Expander: converts Scheme forms into TreeIL forms.
//! 2. Primitive recognition: recognizes primitive references from `(funcall $global-ref <name>)` and replaces
//!    them with `(primref <name>)` forms.
//! 3. Primitive rewriter: Simplifies primitive applications, e.g `(funcall (primref +) (const x) (const y) (const z))` is rewritten 
//!    to `(bind ([tmp (funcall (primref +) (const x) (const y))]) (funcall (primref +) (const tmp) (const z)))`.
//! 4. Optimizer: constant-folding, CSE, inlining, etc.
//! 5. Assignment eliminations. Converts `(mutate (variable x) (const 42))` to `(funcall (primref box-set!) (variable x) (const 42))`
//!    and all mutable variable references to `(funcall (primref box-ref) (variable x))`. Variable bindings are converted to `(bind ([x (funcall (primref make-box) ...)]) ...)`.
//! 4. Loop recovery: converts tail-recursive calls into loops when possible.
//! 

pub mod reader;
pub mod sexpr;
pub mod tree_il;
pub mod rc;
pub mod env;
pub mod syntaxrules;
pub mod expander;
pub mod cp0;
pub mod recover_let;
pub mod fix_letrec;
pub mod cps;
pub mod anf;
pub mod loops;
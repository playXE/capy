# Capy Scheme

R6RS/R7RS Scheme implementation. 

# Features
- MMTk as GC library, provides Immix, GenImmix, StickyImmix and lots of other collectors.
- Possible to ahead-of-time compile Scheme files to bytecode
- Fast interpreter loop, JIT is planned in the future.

# TODO
- JIT compiler
- Low-level interpreter written in assembly
- Actual macro-expander in Scheme
- Write interpreter for REPL and macro expansion in Scheme (most likely it will be closure generating interpreter)

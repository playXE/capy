# Capy Scheme

R6RS/R7RS Scheme implementation.

# Features

- Code is compiled to CPS, no stack-overflows are possible at runtime, free `call/cc`.
- Concurrent GC with both incremental-update and SATB modes (TODO: Document how to switch GC modes).
- JIT compilation using Cranelift as backend.
- As minimal as possible runtime in Rust, R6RS and most of R5RS features are going to be implemented
  on top of that runtime using alexpander + psyntax.


# To be done

- Define alL R4RS/R5RS procedures in Rust
- `dynamic-wind`, proper `call/cc` and others. 
- Self-host alexpander.scm
- Self-host psyntax from Larceny. 
- Improve runtime performance:
  - We compile to CPS but Cranelift has no support for tail calls so we use trampolines, this reduces performance by a lot
  - Compile code to ANF, perform optimizations on it and only then do CPS. 
- Port reclaiming: at the moment ports are not closed automatically.

# How to run

Simply run `cargo run`. It will load `test.scm` from current directory, compile, and evaluate it printing the result.

# Source walkthrough

- `src/compiler.rs`: All compilation stages are defined there. 
- `src/jit/cranelift.rs`: JIT implementation
- `src/jit/cranelift/inlining.rs`: Inlining of primitives
- `src/jit/thunks.rs`: Functions required by JITed code (e.g flushign write barriers). 
- `src/vm.rs`: Virtual machine state and trampoline into Scheme code implementation.
- `src/value.rs`: Scheme Value representation.
- `src/ports_v2.rs`: Internal Scheme ports API implementation.
- `src/pp.rs`: Simple pretty-print implementation.
- `src/structure.rs`: Racket-like `struct` functions.
- `src/number.rs`: numerical tower (TBD).
- `src/fun.rs`: some useful functions to work with Scheme procedures.
- `src/error.rs`: Exception types and function definitions.
- `src/list.rs`: Various list functions.

GC is in another repo: [RSGC](https://github.com/playXE/rsgc/)

# Resources

[Matt Might's](https://matt.might.net/) blog helped me build CPS transformations and closure conversion
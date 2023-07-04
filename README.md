# Capy Scheme

Scheme programming language implementation focused on performance.

# Features
- Concurrent GC with small pause times (thanks to [rsgc](https://github.com/playxe/rsgc))
- Module system based on what Gauche Scheme has (`define-module`, `require`, use` etc)
- Fast interpreter with native-like calling convention

# Near future TODO
- Baseline JIT with minimal amount of speculation (turning tail-calls into loops, inline caching arithmetic etc)
- Standard library written in Scheme
- call/cc

# General TODO

These features might be added at any time, maybe in near future, maybe in very far away future:

 - syntax-case macros
 - R6RS/R7RS-large compatability
 - Optimizing speculative JIT with deoptimization to interpreter in case of speculation failures

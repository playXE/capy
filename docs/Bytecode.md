# Bytecode design

Bytecode files should contain:
- List of modules defined in a file
- List of imported modules
- List of `(require 'feature)`
- Symbol table
- List of bytecode functions
- Index into list of functions for "main" function, used to execute the module
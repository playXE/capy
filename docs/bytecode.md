# Bytecode specification

CapyScheme emits its own bytecode format. Previously ELF + DWARF were considered as a potential target, but due to complication of implementation it was decided to use minimalistic bytecode format instead. 

## Bytecode image format

```
+================================+ <- header start
|      magic value: 32 btis      |
+--------------------------------+
|      code size: 32 bits        | 
+================================+ <- header end, code start
|                                |
|       code section             |
|                                |
+================================+ <- code end, FASL start
|                                |
|       FASL section             |
|                                |
+================================+ <- FASL end

```

## FASL 

FASL (fast-loading) format is used to encode constants, debug information and other information that might be produced by compiler. 

FASL section in bytecode image *always* encodes a vector as first value.

- firsst vector entry is vector of constant values
- second vector entry is program data: low/high PCs, name, number of arguments of procedures
- third vector entry is an entrypoint program. When bytecode image is loaded this entrypoint is executed.
- fourth vector entry is debug information. It is an optional section, it includes mapping from PC to filename/line/column.


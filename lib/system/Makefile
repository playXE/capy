# lib/system/usual.scm always has to be first, it provides macros to simplify bootstrap code
SRCS = 

SRCS += lib/system/usual.scm
SRCS += lib/system/boot.scm
SRCS += lib/system/sysparam.scm
SRCS += lib/system/format.scm
SRCS += lib/system/control.scm
SRCS += lib/system/eval.scm
SRCS += lib/system/records.scm
SRCS += lib/system/condition.scm
SRCS += lib/system/exception.scm
SRCS += lib/system/number.scm
SRCS += lib/system/bignum.scm
SRCS += lib/system/logical.scm
SRCS += lib/system/sys.scm
SRCS += lib/system/iosys.scm
SRCS += lib/system/conio.scm
SRCS += lib/system/customio.scm
SRCS += lib/system/stringio.scm
SRCS += lib/system/fileio.scm
SRCS += lib/system/bytevectorio.scm
SRCS += lib/system/portio.scm
SRCS += lib/system/stdio.scm
SRCS += lib/system/print.scm
SRCS += lib/system/printer.scm
SRCS += lib/system/hashtable.scm
SRCS += lib/system/reader.scm
SRCS += lib/system/enum.scm
SRCS += lib/system/require.scm
SRCS += lib/system/load.scm
SRCS += lib/system/expander/r7rs-includer.scm
SRCS += lib/system/expander/runtime.scm
SRCS += lib/system/expander/expand.scm
SRCS += lib/system/main.scm


all:
	cargo run --bin capy-boot -- $(SRCS)
# lib/system/usual.scm always has to be first
SRCS = 
SRCS += lib/system/usual.scm
SRCS += lib/system/boot.scm
SRCS += lib/system/control.scm
SRCS += lib/system/eval.scm
SRCS += lib/system/records.scm
SRCS += lib/system/condition.scm
SRCS += lib/system/exception.scm


all:
	cargo run --bin capy-boot -- $(SRCS)
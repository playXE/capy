//! File used by C compiler when using C as JIT backend

#include <stdint.h>

#define VALUE_INTP(val) ((val & 1) != 0)
#define VALUE_INT(val) (int32_t)(val >> 1)
#define VALUE_MAKE_INT(val) ((uint64_t)val << 1 | 1)

#define VALUE_HEAP_TAG(val) (*(uint8_t*)val)
#define VALUE_HEAPP(val) (!(val & 1) && VALUE_HEAP_TAG(val) > 6)
#define ACCESS(ty, val, off) (*(ty*)((uint8_t*)val + off))
#define ACCESS_REF(val, off) ((ty*)((uint8_t*)val + off))

typedef uint64_t value_t;

extern value_t listify(void* vm, value_t* argv, uint64_t argc);
extern value_t make_wrong_arity(void* vm, uint64_t argc, uint64_t maxa, uint64_t mina, value_t* argv);
extern value_t make_boxed(void* vm, value_t val);
extern void grow_tail_rands(void* vm, uint64_t to);
extern value_t make_closure(void* vm, void* code, uint64_t nenv, uint64_t mina, uint64_t maxa);
extern value_t undefined_global(void* vm, value_t cell);
extern value_t flush_ssb_and_do_wb(void* vm, value_t val);
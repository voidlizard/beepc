#ifndef __beepvm_h_builtins_h_
#define __beepvm_h_builtins_h_

#include "beepvm.h"

#define BUILTINS_NUM 128 

typedef VM_WORD (*BEEPVM_BUITIN)(void*);

#define BEEPVM_FUNC_NAME(name) beepvm__##name
#define BEEPVM_FUNC_EXTERN_DECL(name) extern VM_WORD beepvm__##name(void *)

VM_WORD beepvm__debug_dump_mem(void *data);
void beepvm__dump(void *vm);

VM_WORD beepvm__nothing(void *data);
VM_WORD beepvm__putc(void*);
VM_WORD beepvm__put_int(void*);
VM_WORD beepvm__puts(void*);
VM_WORD beepvm__putsn(void*);
VM_WORD beepvm__sleep_ms(void*);
VM_WORD beepvm__nothing(void*);

VM_WORD beepvm__strtoul(void *data);
VM_WORD beepvm__utoa(void *data);
VM_WORD beepvm__strconcat(void *data);
VM_WORD beepvm__startswith(void *data);
VM_WORD beepvm__strdup(void *data);
VM_WORD beepvm__streq(void *data);
VM_WORD beepvm__strlen(void *data);
VM_WORD beepvm__strsub(void *data);
VM_WORD beepvm__strnth(void *data);
VM_WORD beepvm__strfind(void *data);
VM_WORD beepvm__strmake(void *data);
VM_WORD beepvm__strfindall(void *data);
VM_WORD beepvm__strfindsub(void *data);

VM_WORD beepvm__api_push_arg(void *data, VM_WORD val);

VM_WORD beepvm__api_str(void *vm, char *asciiz);
VM_WORD beepvm__api_strlen(void *vm, VM_WORD *s);
VM_WORD* beepvm__api_alloc(void *vm, VM_WORD sz);

VM_WORD beepvm__api_cons(void *data);
VM_WORD *beepvm__api_alloc_vect(VM *vm, VM_WORD sz);

VM_WORD beepvm__vect_make(void *data);
VM_WORD beepvm__vect_get(void *data);
VM_WORD beepvm__vect_set(void *data);
VM_WORD beepvm__vect_len(void *data);

VM_WORD beepvm__mem_stats_update(void *data);
VM_WORD beepvm__mem_stats_get(void *data);

void beepvm__astack_overflow(VM *vm);
void beepvm__astack_underflow(VM *vm);
void beepvm__rstack_overflow(VM *vm);
void beepvm__rstack_underflow(VM *vm);
void beepvm__out_of_heap_memory(VM *vm);

void beepvm__fatal_error_handle(void *data, beepvm_errno err);

extern const BEEPVM_BUITIN _builtins[BUILTINS_NUM];

#endif

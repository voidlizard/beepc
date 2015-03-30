#ifndef  __beepvm_h_
#define  __beepvm_h_

#include <stdint.h>

#define VM_WORD WORD

#include "beepvm_mem.h"

typedef const uint8_t  INSTRUCTION;
typedef uint16_t VM_WORD;

#include "beepvm_opcodes"

#ifdef BEEPVM_LOWENDIAN
#define WORDEND(X) ((((X)&0x00FF)<<8)|((X)>>8))
#endif

#ifdef BEEPVM_HIENDIAN
#define WORDEND(X) (X)
#endif

#ifndef BEEPVM_CODE_SIZE_OVERRIDE
#define BEEPVM_CODE_SIZE 8192
#else
#define BEEPVM_CODE_SIZE BEEPVM_CODE_SIZE_OVERRIDE
#endif

#ifndef BEEPVM_ASTACK_SIZE_OVERRIDE
#define BEEPVM_ASTACK_SIZE 128
#else
#define BEEPVM_ASTACK_SIZE BEEPVM_ASTACK_SIZE_OVERRIDE
#endif

#ifndef BEEPVM_RSTACK_SIZE_OVERRIDE
#define BEEPVM_RSTACK_SIZE 64 
#else
#define BEEPVM_RSTACK_SIZE BEEPVM_RSTACK_SIZE_OVERRIDE
#endif

#ifndef BEEPVM_HEAP_SIZE_OVERRIDE
#define BEEPVM_HEAP_SIZE 512 
#else
#define BEEPVM_HEAP_SIZE BEEPVM_HEAP_SIZE_OVERRIDE 
#endif

// MEM_CHOOSE_FIRST - выбор первого подходящего блока памяти из HEAP
#define MEM_CHOOSE_FIRST 

#define MEMCODE_START 0
#define MEMCODE_END   MEMCODE_START+BEEPVM_CODE_SIZE-1

#define HEAP_START    MEMCODE_END+1
#define HEAP_END      HEAP_START+BEEPVM_HEAP_SIZE-1

#define HEAP_FLAG 0x8000

#define AOFF(X) ((X)&(~HEAP_FLAG))

#define ISHEAP(X) ((X)&HEAP_FLAG)

#define PTRCODE(VM, X) ((VM_WORD*)((char*)(((VM)->code)) + AOFF((X))))

#define PTRHEAP(VM, X) ((VM_WORD*)((char*)(((VM)->heap)) + AOFF((X))))

#define ADDR2PTR_H(H, X) (ISHEAP((X)) ? ((VM_WORD*)(((char*)(H))+AOFF((X)))) : ((VM_WORD*)0))

#define ADDR2PTR(VM, X) (ISHEAP((X)) ? PTRHEAP((VM),(X)) : PTRCODE((VM),(X)) )

#define PHEAP2ADDR(VM, P) (HEAP_FLAG | (((char*)(P)) - ((char*)((VM)->heap))))

#define BEEPVM_STRSIZE(l) ((l)/2 + (l%2) + 1)

#define BEEPVM_SETFRAME(vm,N,frame) ((frame) = ((vm)->astack_top-(N)))

#define BEEPVM_ARGP(frame,n)  ((frame)+(n))

#define BEEPVM_DROPFRAME(vm,frame) (vm)->astack_top = (frame)

#define BEEP_TRUE  1
#define BEEP_FALSE 0
#define BEEP_NIL   BEEP_FALSE

typedef enum {
    VM_NO_HEAP      = 100,
    VM_ASTACK_OVER  = 101,
    VM_ASTACK_UNDER = 102,
    VM_RSTACK_OVER  = 103,
    VM_RSTACK_UNDER = 104
} beepvm_errno;


typedef struct {
	uint16_t ip;
	VM_WORD  astack[BEEPVM_ASTACK_SIZE];
	VM_WORD  rstack[BEEPVM_RSTACK_SIZE];
	VM_WORD  heap[BEEPVM_HEAP_SIZE];
	VM_WORD *heap_end;
	VM_WORD *astack_top;
	VM_WORD *rstack_top;
	VM_WORD *frame;
    void    *err_jmp;
    mem_info_t memstats;
	INSTRUCTION *code;
} VM;

void beepvm_run(VM *vm, INSTRUCTION *code); 

#endif


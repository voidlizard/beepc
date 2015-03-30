#include "beepvm.h"
#include "beepvm_builtins.h"
#include "beepvm_mem.h"

#include <setjmp.h>

#ifdef BEEPVM_JMP_INTERPETER

#define OP(N) \
L_##N: \

#define OPCODES_BEGIN
#define OPCODES_END

#define NEXT() goto *instr[code[++ip]]
#define JUMP(A) ip = A; goto *instr[code[ip]]

#else

#define OPCODES_BEGIN while(1) { switch(code[ip]) {
#define OPCODES_END }}
#define JUMP(X) ip = (X); continue
#define NEXT() ip++; continue
#define OP(N)  case N:

#endif

#define LWLOAD(what)  \
(what) = code[++ip]; (what) |= (code[++ip]<<8);

#define astack_overflow_fatal()    beepvm__astack_overflow(vm);
#define astack_underflow_fatal()   beepvm__astack_underflow(vm);
#define rstack_overflow_fatal()    beepvm__rstack_overflow(vm);
#define rstack_underflow_fatal()   beepvm__rstack_underflow(vm);
#define out_of_heap_memory_fatal() beepvm__out_of_heap_memory(vm);


#define STP(s) (s##_top)
#define SBT(s) (s##_bottom)

#define spush(s, v) ((STP(s) != SBT(s) && ((*STP(s)++ = (v)), 1)) || 0)
#define spop(s, v)  ((STP(s) > s && (((v) = *--STP(s)), 1)) || 0)
#define top(s)      (*(STP(s)-1))

#define PUSH(s, v)  if( !(spush(s, (v))) ) {s##_overflow_fatal(); return;}
#define POP(s, v)   if( !(spop(s, (v))) )  {s##_underflow_fatal(); return;}
#define TOP(s) top(s)

#ifdef X86_DBG
#include <stdio.h>
#define DBG_LOG_FMT(fmt, args) printf("[DBG] " fmt "\n", args);
#else
#define DBG_LOG_FMT(fmt, args)
#endif

void beepvm_init(VM *vm, INSTRUCTION* code)
{
	vm->ip    = 0;
	vm->frame = 0;
	vm->astack_top  = vm->astack;
	vm->rstack_top  = vm->rstack;
	vm->code = code;
	vm->heap_end = vm->heap + BEEPVM_HEAP_SIZE;
	mem_init(vm->heap);
    vm->memstats.allocated      = 0;
    vm->memstats.free           = 0;
    vm->memstats.max_free_block = 0;
}

#define SYNC(x) {\
	{vm->ip = ip; vm->frame = frame; vm->astack_top = astack_top;} \
	(x); \
	{ astack_top = vm->astack_top;} \
}

//#include "raccoon_hardware.h"

void beepvm_run(VM *vm, INSTRUCTION *code) {

    #ifdef BEEPVM_JMP_INTERPETER

    static const void* instr[256] = {
#include "beepvm_instr"
    };

    #endif

	uint16_t ip   = 0;
	uint16_t lit  = 0;
	VM_WORD *astack;
	VM_WORD *rstack;
	VM_WORD *astack_top, *astack_bottom;
	VM_WORD *rstack_top, *rstack_bottom;
	VM_WORD *frame = 0;
	VM_WORD *memp  = 0;
	VM_WORD  tmp   = 0;
	VM_WORD  ttmp  = 0;
	VM_WORD  ttmp2 = 0;
    jmp_buf err_jmp_buf;
    int i          = 0;

	/* VM INIT */
	beepvm_init(vm, code);
	astack        = vm->astack;
	rstack        = vm->rstack;
	astack_top    = astack;
	astack_bottom = astack + BEEPVM_ASTACK_SIZE;
	rstack_top    = rstack;
	rstack_bottom = rstack + BEEPVM_RSTACK_SIZE;
	/* VM INIT END */

    /* VM CRASH HANDLER BEGIN */
    vm->err_jmp = (void*)&err_jmp_buf;
    i = setjmp(err_jmp_buf);
    if(  0 != i ) {
        DBG_LOG_FMT("%s", "VM FATAL ERROR");
        beepvm__fatal_error_handle(vm, i);
        return;
    }
    /* VM CRASH HANDLER END   */


	/* VM BEGIN */
    #ifdef BEEPVM_JMP_INTERPETER
	JUMP(0);
    #endif
	/* VM END */

	OPCODES_BEGIN
        OP(NOP) {
            NEXT();
        }
        OP(ALLOC) {
			POP(astack, lit);
			SYNC(memp = mem_alloc_force_gc(vm->heap,   vm->heap_end, 
			                          vm->astack, vm->astack_top, lit));
			if( !memp ) {
				{out_of_heap_memory_fatal()};
			}
			ttmp = PHEAP2ADDR(vm, memp);
			PUSH(astack, ttmp);
            NEXT();
        }
		OP(CONS) {
			SYNC(memp = mem_alloc_force_gc(vm->heap,   vm->heap_end, 
			                          vm->astack, vm->astack_top, 2)); /* TWO WORDS */
			if( !memp ) {
				{out_of_heap_memory_fatal()};
			}
			POP(astack, lit);
			*(memp+1) = lit;
			POP(astack, lit);
			*memp = lit;
			ttmp = PHEAP2ADDR(vm, memp);
			PUSH(astack, ttmp);
            NEXT();
		}
        OP(GC) {
			SYNC(mem_gc(vm->heap, vm->heap_end, vm->astack, vm->astack_top));
            NEXT();
        }
        OP(CGC) {
			SYNC(mem_gc_O1(vm->heap, vm->heap_end, vm->astack, vm->astack_top));
            NEXT();
        }
        OP(RW) {
			POP(astack, lit);
			memp = ADDR2PTR(vm, lit);
			PUSH(astack, *memp);
            NEXT();
        }
        OP(WW) {
			POP(astack, lit);
			POP(astack, ttmp);
			memp = ADDR2PTR(vm, ttmp);
			*memp = lit;
            NEXT();
        }
        OP(TBADDR) {
			POP(astack, ttmp);                /* OFFSET */
            TOP(astack) += (sizeof(VM_WORD)*ttmp);
            NEXT();
        }
        OP(TBCW) {
            LWLOAD(lit);                      /* OFFSET */
			POP(astack, ttmp);                /* VAL    */
			memp = ADDR2PTR(vm, TOP(astack)); /* ADDR ON TOP */
			*(memp+lit) = ttmp;
            NEXT();
        }
        OP(TBCWD) {
            LWLOAD(lit);                      /* OFFSET */
			POP(astack, ttmp);                /* VAL    */
			POP(astack, ttmp2);               /* ADDR   */
			memp = ADDR2PTR(vm, ttmp2);
			*(memp+lit) = ttmp;
            NEXT();
        }
        OP(TBCR) {
			LWLOAD(lit);                      /* OFFSET */
			POP(astack, ttmp);                /* ADDR */
			memp = ADDR2PTR(vm, ttmp); 
			ttmp = *(memp+lit);
			PUSH(astack, ttmp);
            NEXT();
        }
        OP(LW) {
            NEXT();
        }
		OP(NDUP) {
            LWLOAD(lit);
			astack_top += lit;
			if( SBT(astack) <= STP(astack) ) {astack_overflow_fatal();}
			NEXT();
		}
        OP(STMP) {
            POP(astack, tmp);
            NEXT();
        }
        OP(LTMP) {
            PUSH(astack, tmp);
            NEXT();
        }
        OP(SF) {
            LWLOAD(lit);
			ttmp = (VM_WORD)(astack_top-astack);
			PUSH(rstack, ttmp)
			ttmp = (VM_WORD)(frame-astack);
			PUSH(rstack, ttmp)
			frame = astack_top - lit;
            NEXT();
        }
        OP(FS) {
            LWLOAD(lit);
			POP(rstack, ttmp);
			frame = astack + ttmp;
			POP(rstack, ttmp);
			astack_top = astack + ttmp - lit;
            NEXT();
        }
        OP(CALL) {
            LWLOAD(lit);
            PUSH(rstack, (ip+1));
            JUMP(lit);
        }
        OP(CALLT) {
            POP(astack, tmp);
            PUSH(rstack, (ip+1));
            JUMP(tmp);
        }
        OP(NCALL) {
            LWLOAD(lit);
            SYNC(tmp = _builtins[lit](vm));
            NEXT();
        }
        OP(NCALLT) {
            POP(astack, tmp);
            SYNC(tmp = _builtins[tmp](vm));
            NEXT();
        }
        OP(RET) {
            POP(rstack, lit); /* return addr */
            JUMP(lit);
        }
        OP(JMP) {
			LWLOAD(lit);
            JUMP(lit);
        }
        OP(JNZ) {
            LWLOAD(lit);
            POP(astack, ttmp);
            if( ttmp ) {
                JUMP(lit);
            }
            NEXT();
        }
        OP(JZ) {
            LWLOAD(lit);
            POP(astack, ttmp);
            if( !ttmp ) {
                JUMP(lit);
            }
            NEXT();
        }
        OP(NEG) {
            TOP(astack) = -TOP(astack);
            NEXT();
        }
        OP(INCA) {
            TOP(astack)++;
            TOP(astack)++;
            NEXT();
        }
        OP(INC) {
            TOP(astack)++;
            NEXT();
        }
        OP(DEC) {
            TOP(astack)--;
            NEXT();
        }
        OP(ADD) {
            POP(astack, lit);
            TOP(astack) += lit;
            NEXT();
        }
        OP(SUB) {
            POP(astack, lit);
            TOP(astack) -= lit;
            NEXT();
        }
        OP(MUL) {
            POP(astack, lit);
            TOP(astack) *= lit;
            NEXT();
        }
        OP(DIV) {
            POP(astack, lit);
            TOP(astack) /= lit; // #TODO: zero division handling?
            NEXT();
        }
        OP(MOD) {
            POP(astack, lit);
            TOP(astack) %= lit;
            NEXT();
        }
        OP(AND) {
            POP(astack, lit);
            TOP(astack) &= lit;
            NEXT();
        }
        OP(OR) {
            POP(astack, lit);
            TOP(astack) |= lit;
            NEXT();
        }
        OP(XOR) {
            POP(astack, lit);
            TOP(astack) ^= lit;
            NEXT();
        }
        OP(INV) {
            TOP(astack) = ~TOP(astack);
            NEXT();
        }
        OP(SHL) {
            POP(astack, lit);
            TOP(astack) <<= lit;
            NEXT();
        }
        OP(SHR) {
            POP(astack, lit);
            TOP(astack) >>= lit;
            NEXT();
        }
        OP(LE) {
            POP(astack, lit);
            TOP(astack) = (TOP(astack) < lit ? 1 : 0);
            NEXT();
        }
        OP(GT) {
            POP(astack, lit);
            TOP(astack) = (TOP(astack) > lit ? 1 : 0);
            NEXT();
        }
        OP(EQ) {
            POP(astack, lit);
            TOP(astack) = (TOP(astack) == lit ? 1 : 0);
            NEXT();
        }
        OP(NEQ) {
            POP(astack, lit);
            TOP(astack) = (TOP(astack) != lit ? 1 : 0);
            NEXT();
        }
        OP(LEQ) {
            POP(astack, lit);
            TOP(astack) = (TOP(astack) <= lit ? 1 : 0);
            NEXT();
        }
        OP(GEQ) {
            POP(astack, lit);
            TOP(astack) = (TOP(astack) >= lit ? 1 : 0);
            NEXT();
        }
        OP(NN) {
            TOP(astack) = !!TOP(astack);
            NEXT();
        }
        OP(NOT) {
            TOP(astack) = (TOP(astack)) != 0 ? 0 : 1;
            NEXT();
        }
        OP(DUP) {
            lit = TOP(astack);
            PUSH(astack, lit);
            NEXT();
        }
        OP(DROP) {
            POP(astack, lit);
            NEXT();
        }
        OP(LIT) {
            LWLOAD(lit);
            PUSH(astack, lit);
            NEXT();
        }
		OP(TRUE) {
            PUSH(astack, 1);
			NEXT();
		}
		OP(FALSE) {
            PUSH(astack, 0);
			NEXT();
		}
        OP(LOADF) {
            LWLOAD(lit);
            PUSH(astack, *(frame+lit));
            NEXT();
        }
        OP(STOREF) {
            LWLOAD(lit);
            POP(astack, ttmp);
            *(frame+lit) = ttmp;
            NEXT();
        }
        OP(LOAD0) {
            PUSH(astack, *(frame+0));
            NEXT();
        }
        OP(LOAD1) {
            PUSH(astack, *(frame+1));
            NEXT();
        }
        OP(LOAD2) {
            PUSH(astack, *(frame+2));
            NEXT();
        }
        OP(LOAD3) {
            PUSH(astack, *(frame+3));
            NEXT();
        }
        OP(LOAD4) {
            PUSH(astack, *(frame+4));
            NEXT();
        }
        OP(LOAD5) {
            PUSH(astack, *(frame+5));
            NEXT();
        }
        OP(LOAD6) {
            PUSH(astack, *(frame+6));
            NEXT();
        }
        OP(LOAD7) {
            PUSH(astack, *(frame+7));
            NEXT();
        }
        OP(STORE0) {
            POP(astack, lit);
            *(frame+0) = lit;
            NEXT();
        }
        OP(STORE1) {
            POP(astack, lit);
            *(frame+1) = lit;
            NEXT();
        }
        OP(STORE2) {
            POP(astack, lit);
            *(frame+2) = lit;
            NEXT();
        }
        OP(STORE3) {
            POP(astack, lit);
            *(frame+3) = lit;
            NEXT();
        }
        OP(STORE4) {
            POP(astack, lit);
            *(frame+4) = lit;
            NEXT();
        }
        OP(STORE5) {
            POP(astack, lit);
            *(frame+5) = lit;
            NEXT();
        }
        OP(STORE6) {
            POP(astack, lit);
            *(frame+6) = lit;
            NEXT();
        }
        OP(STORE7) {
            POP(astack, lit);
            *(frame+7) = lit;
            NEXT();
        }
        OP(DOWN) {
			SYNC(frame=frame);
            return;
        }
	OPCODES_END
}


void beepvm__astack_overflow(VM *vm)
{
    longjmp(*(jmp_buf*)vm->err_jmp, VM_ASTACK_OVER);
}

void beepvm__astack_underflow(VM *vm)
{
    longjmp(*(jmp_buf*)vm->err_jmp, VM_ASTACK_UNDER);
}

void beepvm__rstack_overflow(VM *vm)
{
    longjmp(*(jmp_buf*)vm->err_jmp, VM_RSTACK_OVER);
}

void beepvm__rstack_underflow(VM *vm)
{
    longjmp(*(jmp_buf*)vm->err_jmp, VM_RSTACK_UNDER);
}

void beepvm__out_of_heap_memory(VM *vm)
{
    longjmp(*(jmp_buf*)vm->err_jmp, VM_NO_HEAP);
}



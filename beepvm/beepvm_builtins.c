#include <unistd.h>
#include <stdio.h>
#include "beepvm_builtins.h"

VM_WORD beepvm__sleep_ms(void *data)
{
    VM* vm = (VM*)data;
	VM_WORD d = 0;
    VM_WORD *frame = 0;

    BEEPVM_SETFRAME(vm,1,frame);

	d = * BEEPVM_ARGP(frame, 0);
	fflush(stdout);
	usleep(d*1000);
	
    BEEPVM_DROPFRAME(vm, frame);

    return 0;
}

VM_WORD beepvm__debug_dump_mem(void *data)
{
	int i = 0;
	VM *vm = (VM*)data;
    VM_WORD *hp = vm->heap;
    VM_WORD *frame = 0;

    BEEPVM_SETFRAME(vm,0,frame);

	VM_WORD *hp_end = vm->heap_end;
	for(; hp < hp_end; hp++, i++ )
	{
		if( i % 16 == 0) printf("\n%04X: ", i);
		printf("%04X ", *hp);
	}
	printf("\n\n");
	
    BEEPVM_DROPFRAME(vm, frame);

    return 0;
}


VM_WORD beepvm__putc(void *data)
{
    VM *vm = (VM*)data;
    VM_WORD chr = 0;
    VM_WORD *frame = 0;

    BEEPVM_SETFRAME(vm,1,frame);
    
    chr    = * BEEPVM_ARGP(frame, 0);

    BEEPVM_DROPFRAME(vm, frame);

	return putchar((char)chr);
}

VM_WORD beepvm__put_int(void *data)
{
    VM *vm = (VM*)data;
    VM_WORD val = 0;
    VM_WORD *frame = 0;

    BEEPVM_SETFRAME(vm,1,frame);

    val    = * BEEPVM_ARGP(frame, 0);

    BEEPVM_DROPFRAME(vm, frame);

    return printf("%04X", val);
}

VM_WORD beepvm__puts(void *data)
{
	VM *vm = (VM*)data;
	VM_WORD addr = 0;
	VM_WORD *p;
    VM_WORD *frame = 0;

    BEEPVM_SETFRAME(vm,1,frame);

	uint16_t len = 0;
	char *ps = 0;
	addr   = * BEEPVM_ARGP(frame, 0);
	p  = ADDR2PTR(vm, addr);
	len = beepvm__api_strlen(data, p);
	ps = (char*)(p+1); 
	while( len-- ) printf("%c",*ps++);
	
    BEEPVM_DROPFRAME(vm, frame);

    return 0;
}

VM_WORD beepvm__putsn(void *data)
{
	beepvm__puts(data);
	printf("\n");
	return 0;
}

void beepvm__dump(void *data)
{
	VM *vm = (VM*)data;
	VM_WORD *p;
	printf("\nDUMP: \n"
           "IP: %04X\n"
           "ASTACK [%d] %04X\n", vm->ip, (uint16_t)(vm->astack_top-vm->astack), *(vm->astack_top));
	for( p = vm->astack; p < vm->astack_top; p++ )
	{
		printf("A[%d]: %04X %08X\n", (unsigned)(p-vm->frame), *p, (unsigned)p);
	}
}

void beepvm__fatal_error_handle(void *data, beepvm_errno err)
{
    switch(err)
    {
        case VM_NO_HEAP:
            printf("*** FATAL: OUT OF HEAP\n");
            break;
        case VM_ASTACK_OVER:
            printf("*** FATAL: A-STACK OVERFLOW\n");
            break;
        case VM_ASTACK_UNDER:
            printf("*** FATAL: A-STACK UNDERFLOW\n");
            break;
        case VM_RSTACK_OVER:
            printf("*** FATAL: R-STACK OVERFLOW\n");
            break;
        case VM_RSTACK_UNDER:
            printf("*** FATAL: R-STACK UNDERFLOW\n");
            break;
    }
}


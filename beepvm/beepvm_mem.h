#ifndef __beepvm_mem_h_
#define __beepvm_mem_h_

#include "beepvm.h"

#define WORD VM_WORD

#include "beepvm_mem_info.h"

void mem_init(WORD *heap);
WORD *mem_alloc(WORD *start, WORD *end, WORD size);
void mem_gc_O1(WORD *begin, WORD *end, WORD *perim, WORD *perim_end);
void mem_gc(WORD *begin, WORD *end, WORD *perim, WORD *perim_end);

WORD *mem_alloc_force_gc(WORD *start, WORD *end, 
                         WORD *pstart, WORD *pend, 
						 WORD size);

void mem_stats(WORD *heap, WORD *heap_end, mem_info_t *info); 

#endif

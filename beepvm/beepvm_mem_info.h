#ifndef __beepvm_mem_info_h
#define __beepvm_mem_info_h

typedef unsigned int WORD;

typedef struct {
    WORD allocated;
    WORD free;
    WORD max_free_block;
} mem_info_t;

#endif

#include "beepvm.h"
#include "beepvm_mem.h"

/*
 *   XYYCCCCC CCCCCCCC
 *   X  - allocated, 1|0
 *   YY - gc mark - white, gray, black
 *   CCCCCCCCCCCCC - block size in WORDS, 13 bit  / 0000 - 1FFFF range
 */

#define HEAD_SIZE sizeof(WORD)/sizeof(WORD)

#define M_GC_WHITE 0
#define M_GC_GRAY  1
#define M_GC_BLACK 2

//#define M_ALLOC_BIT 14
//#define M_COLOR_BIT 12

#define M_ALLOC_BIT 15
#define M_COLOR_BIT 13


#define MASK_ALLOC  ((WORD)(1<<M_ALLOC_BIT))
#define MASK_SIZE   ((1<<M_COLOR_BIT)-1)

#define M_IS_ALLOCATED(p)  (((*(p))&MASK_ALLOC)!=0)
#define M_SET_ALLOCATED(p) ((*(p))|=MASK_ALLOC)
#define M_CLR_ALLOCATED(p) ((*(p))&=~MASK_ALLOC)

#define M_GET_COLOR(p)    (((*(p))>>M_COLOR_BIT)&0x03)
#define M_SET_COLOR(p,c)  ((*(p)) = (((c)&0x03)<<M_COLOR_BIT)|((*(p))&(MASK_ALLOC|MASK_SIZE)))


#define M_GET_SIZE(p)     ((*(p))&MASK_SIZE)
#define M_SET_SIZE(p,a)   ((*(p)) = (((a)&MASK_SIZE))|((*(p))&(~MASK_SIZE)))

void mem_init(WORD *heap)
{
	M_CLR_ALLOCATED(heap);
	M_SET_SIZE(heap, BEEPVM_HEAP_SIZE);
	M_SET_COLOR(heap, M_GC_WHITE);
}

static WORD *mem_next_block(WORD *ptr, WORD *end)
{
	ptr += M_GET_SIZE(ptr); 
	if( ptr >= end ) return 0;
	return ptr;
}


static void mem_block_fill(WORD *head, WORD pattern)
{
	static WORD patt = 0xAAAA;
	WORD *p  = head;
	WORD *pe = p + M_GET_SIZE(head);
	for(++p; p < pe; p++) { *p = patt; patt++; }
}

WORD *mem_alloc_force_gc(WORD *start, WORD *end, 
                         WORD *pstart, WORD *pend, 
						 WORD size)
{
	WORD *memp = 0;
	memp = mem_alloc(start, end, size);
	if( !memp ) {
		mem_gc(start, end, pstart, pend);
		memp = mem_alloc(start, end, size);
	}
	return memp;
}


WORD *mem_alloc(WORD *start, WORD *end, WORD size)
{
	WORD *p = start;
	WORD *last_found = 0;
	WORD sz = size + HEAD_SIZE;
	WORD fsz = 0;

	if( 0 == size )
		return (WORD*)0;

	for( ; p; p = mem_next_block(p, end) ) 
	{
		if( M_IS_ALLOCATED(p) )
			continue;

		fsz = M_GET_SIZE(p);

		if( fsz < sz )
			continue;

#ifdef MEM_CHOOSE_FIRST
    last_found = p;
    break;
#else
		if( fsz == sz ) {
			last_found = p;
			break;
		}

		if( !last_found || fsz < M_GET_SIZE(last_found) )
			last_found = p;
#endif

	}
	if( last_found )
	{
		M_SET_ALLOCATED(last_found);
		fsz = M_GET_SIZE(last_found) - sz;
		p = last_found + sz;
		if( fsz > HEAD_SIZE )
		{
			M_CLR_ALLOCATED(p);
			M_SET_SIZE(p, fsz);
			M_SET_SIZE(last_found, sz);
		} 
		else 
		{
			M_SET_SIZE(last_found, sz+fsz);
		}
		last_found++;
	}
	return last_found;
}

static void mem_compact(WORD *begin, WORD *end)
{
	WORD *p = 0, *pnext = 0, sz = 0;
	p = begin;
	while(p)
	{
		pnext = mem_next_block(p, end);
		if( !pnext ) break;
		if( !M_IS_ALLOCATED(p) && !M_IS_ALLOCATED(pnext) )
		{
			sz = M_GET_SIZE(p) + M_GET_SIZE(pnext);
			M_SET_SIZE(p, sz);
			continue;
		}
		p = pnext;
	}
}

static void mem_set_white(WORD *begin, WORD *end)
{
	WORD *p = 0;
	for( p = begin; p; p = mem_next_block(p, end) )
	{
		if( M_IS_ALLOCATED(p) )
			M_SET_COLOR(p, M_GC_WHITE);
	}
}

static WORD* mem_find_block(WORD *begin, WORD *end, WORD* ptr)
{
	WORD *p = 0;
	if( !ptr ) return (WORD*)0;
	for( p = begin; p; p = mem_next_block(p, end) )
	{
		if( M_IS_ALLOCATED(p) && ptr > p && ptr < p + M_GET_SIZE(p) ) 
			return p;
	}
	return (WORD*)0;
}

static WORD mem_scan_range(WORD *begin, WORD *end, WORD *from, WORD *to, WORD color)
{
	WORD *p, *ptr, num = 0;
	for( p = from; p<to; p++ )
	{
		ptr = mem_find_block(begin, end, ADDR2PTR_H(begin, *p));
		if( !ptr ) continue;
		num++;
		if( M_GET_COLOR(ptr) < color)
			M_SET_COLOR(ptr, color);
	}
	return num;
}

static void mem_scan_grays(WORD *begin, WORD *end)
{
	uint16_t grays = 0;
	WORD *p = 0;
	do {
		grays = 0;
		for( p = begin; p; p = mem_next_block(p, end) )
		{
			if( M_GET_COLOR(p) == M_GC_GRAY ) {
				mem_scan_range(begin, end, p+1, p+M_GET_SIZE(p), M_GC_GRAY);
				M_SET_COLOR(p, M_GC_BLACK);
				grays++;
			}
		}
	} while(grays);
}

static void mem_collect_free(WORD *begin, WORD *end)
{
	WORD *p;
	for( p = begin; p; p = mem_next_block(p, end) )
	{
		if( !M_IS_ALLOCATED(p) )
			continue;

		if( M_GET_COLOR(p) == M_GC_WHITE ) {
			M_CLR_ALLOCATED(p);
            mem_block_fill(p, 0xCCCC);
		}
		else
		{
			M_SET_COLOR(p, M_GC_WHITE);
		}
	}
}

void mem_gc_O1(WORD *begin, WORD *end, WORD *perim, WORD *perim_end)
{
	if( perim == perim_end )
		mem_init(begin);
}

void mem_gc(WORD *begin, WORD *end, WORD *perim, WORD *perim_end)
{
	mem_set_white(begin, end);
	if( !mem_scan_range(begin, end, perim, perim_end, M_GC_GRAY) ) 
	{
		mem_init(begin);
		return;
	}
	mem_scan_grays(begin, end);
	mem_collect_free(begin, end);
    mem_compact(begin, end);
}

void mem_stats(WORD *heap, WORD *heap_end, mem_info_t *info) {
	WORD *p;
    WORD size      = 0;
    WORD allocated = 0;
    WORD free      = 0;
    WORD max       = 0;
	for( p = heap; p; p = mem_next_block(p, heap_end) ) {
        size = M_GET_SIZE(p) - HEAD_SIZE;
        if( M_IS_ALLOCATED(p) ) {
            allocated += (size); 
        } else {
            free += size;
            if(size > max)
                max = size;
        }
    }
    info->allocated      = allocated;
    info->free           = free;
    info->max_free_block = max;
}


#include "beepvm.h"
#include "beepvm_builtins.h"
#include "beepvm_mem.h"
#include <string.h>
#include <stdlib.h>

VM_WORD beepvm__nothing(void *data)
{
	VM *vm = (VM*)data;
	return 0;
}

VM_WORD beepvm__api_push_arg(void *data, VM_WORD val)
{
	VM *vm = (VM*)data;
	if(vm->astack_top != (vm->astack + BEEPVM_ASTACK_SIZE))
	{
	   *(vm->astack_top++) = val;
	}
	else
	{
	   beepvm__astack_overflow(vm);
	}
	return 0;
}

VM_WORD beepvm__api_strlen(void *vm, VM_WORD *s)
{
	return *s;
}

VM_WORD beepvm__strnth(void *data)
{
	VM_WORD *sp, len, off, addr;
	char *cp, *ce;
	VM *vm = (VM*)data;
	
    VM_WORD *frame = 0;

    BEEPVM_SETFRAME(vm,2,frame);

    off    = * BEEPVM_ARGP(frame, 1);
	addr   = * BEEPVM_ARGP(frame, 0);

    sp = ADDR2PTR(vm, addr);
	len = beepvm__api_strlen(data, sp);
	cp = (char*)(sp+1);
	ce = cp + len - 1;
	cp += off;
	cp = cp > ce ? ce : cp;

    BEEPVM_DROPFRAME(vm, frame);

    return beepvm__api_push_arg(data, *cp);
}

VM_WORD beepvm__strlen(void *data)
{
	VM_WORD *sp, len, addr;
	VM *vm = (VM*)data;
	
    VM_WORD *frame = 0;

    BEEPVM_SETFRAME(vm,1,frame);

    addr   = * BEEPVM_ARGP(frame, 0);
	
    sp = ADDR2PTR(vm, addr);
	len = beepvm__api_strlen(data, sp);

    BEEPVM_DROPFRAME(vm, frame);

    return beepvm__api_push_arg(data, len);
}

VM_WORD beepvm__strfind(void *data)
{
	VM *vm = (VM*)data;
	VM_WORD addr, off, tok, *sp, len;
	char *ps, *se=0;
	
    VM_WORD *frame = 0;

    BEEPVM_SETFRAME(vm,3,frame);

    tok    = * BEEPVM_ARGP(frame, 2);
	off    = * BEEPVM_ARGP(frame, 1);
	addr   = * BEEPVM_ARGP(frame, 0);
	
    sp = ADDR2PTR(vm, addr);
	len = beepvm__api_strlen(data, sp);
	ps = (char*)(sp+1);
	se = ps+len;
	ps += off;
	for(; ps < se; ps++, off++ ) {
		if( *ps == (char)tok ) break;
	}

    BEEPVM_DROPFRAME(vm, frame);

    return beepvm__api_push_arg(data, off);
}

VM_WORD beepvm__strsub(void *data)
{
	VM_WORD *sp, off, len, slen, addr;
	VM *vm = (VM*)data;
	char *se, *src_p, *src_e, *dst;
    VM_WORD *frame = 0;

    BEEPVM_SETFRAME(vm,3,frame);

	len    = * BEEPVM_ARGP(frame, 2);
	off    = * BEEPVM_ARGP(frame, 1);
	addr   = * BEEPVM_ARGP(frame, 0);

    sp = ADDR2PTR(vm, addr);
	slen = beepvm__api_strlen(data, sp);
	sp++;
	se = ((char*)sp)+slen;
	src_p = ((char*)sp) + off;
	src_e = src_p + len;
	src_p =  src_p > se ? se : src_p;
	src_e =  src_e > se ? se : src_e;
	len = (VM_WORD)(src_e - src_p);
	sp = mem_alloc_force_gc(vm->heap, vm->heap_end, 
							  vm->astack, vm->astack_top,
							  len/2 + len%2 + 1);
	if( !sp ) {
		 beepvm__out_of_heap_memory(vm);
	} 
    else 
    {
        addr = PHEAP2ADDR(vm, sp);
        *sp++ = len;
        dst = (char*)sp; 
        for( ; src_p < src_e; src_p++ ) *dst++ = *src_p;
	}

    BEEPVM_DROPFRAME(vm, frame);

    return beepvm__api_push_arg(vm, addr);
}

VM_WORD beepvm__strmake(void *data)
{
	VM *vm = (VM*)data;
	VM_WORD len, chr, lw, *memp;
	char *p, *pe;

    VM_WORD *frame = 0;

    BEEPVM_SETFRAME(vm,2,frame);

	chr    = * BEEPVM_ARGP(frame, 1);
	len    = * BEEPVM_ARGP(frame, 0);

	lw = BEEPVM_STRSIZE(len);
	memp = beepvm__api_alloc(data, lw);

	p   = (char*)(memp+1);
	pe  = p + len;

	for(; p < pe; p++) *p = (char)chr;

	memp[0] = len;

    BEEPVM_DROPFRAME(vm, frame);

	return beepvm__api_push_arg(vm, PHEAP2ADDR(vm, memp));
}


VM_WORD beepvm__strdup(void *data)
{
	VM *vm = (VM*)data;
	VM_WORD *sp, *memp, *retp, len, lw;
	VM_WORD addr;
    VM_WORD *frame = 0;

    BEEPVM_SETFRAME(vm,1,frame);

	addr   = * BEEPVM_ARGP(frame, 0);
	sp = ADDR2PTR(vm, addr);
	len = beepvm__api_strlen(data, sp); 
	lw = len/2 + len%2 + 1;
	memp = mem_alloc_force_gc(vm->heap, vm->heap_end, 
							  vm->astack, vm->astack_top,
							  lw);
	if( !memp ) {
		 beepvm__out_of_heap_memory(vm);
	}
    else {
        retp = memp;
        addr = PHEAP2ADDR(vm, memp);
        while(lw--) *retp++ = *sp++;
    }

    BEEPVM_DROPFRAME(vm, frame);

    return beepvm__api_push_arg(data, addr);
}

VM_WORD beepvm__streq(void *data)
{
	VM *vm = (VM*)data;
	VM_WORD *s1, *s2, addr1, addr2, len, res = BEEP_TRUE;
	
    VM_WORD *frame = 0;

    BEEPVM_SETFRAME(vm,2,frame);

    addr1  = * BEEPVM_ARGP(frame, 1);
	addr2  = * BEEPVM_ARGP(frame, 0);
	
    s2 = ADDR2PTR(vm, addr1);
	s1 = ADDR2PTR(vm, addr2);
	len = beepvm__api_strlen(data, s1);
	len = len/2 + len%2 + 1;
	while( len-- ) { if( *s1++ != *s2++ ) { res = BEEP_FALSE; break; } }
   	
    BEEPVM_DROPFRAME(vm, frame);

    return beepvm__api_push_arg(data, res);
}

VM_WORD beepvm__startswith(void *data)
{
	VM *vm = (VM*)data;
	VM_WORD addr1, addr2, *w1, *w2, len, ret = BEEP_FALSE;
	char *s1, *s1e, *s2;
	
    VM_WORD *frame = 0;

    BEEPVM_SETFRAME(vm,2,frame);

    addr2  = * BEEPVM_ARGP(frame, 1);
	addr1  = * BEEPVM_ARGP(frame, 0);
	
    w1 = ADDR2PTR(vm, addr1);
	w2 = ADDR2PTR(vm, addr2);
	len = beepvm__api_strlen(data, w2);
	s1  = (char*)(w2+1);
	s1e = s1 + len;
	s2  = (char*)(w1+1);
	do {
		if( len > beepvm__api_strlen(data, w1) ) break;
		ret = BEEP_TRUE;
		for( ; s1 < s1e ; s1++, s2++)
			if( *s1 != *s2 ) { ret = BEEP_FALSE; break; };

	}while(0);
	
    BEEPVM_DROPFRAME(vm, frame);

    return beepvm__api_push_arg(data, ret);
}

VM_WORD beepvm__strconcat(void *data)
{
	VM *vm = (VM*)data;
	VM_WORD addr1, addr2, *memp1, *memp2, *memp3, len1, len2;
	char *dst, *src, *srce;
    VM_WORD *frame = 0;

    BEEPVM_SETFRAME(vm,2,frame);

	addr2  = * BEEPVM_ARGP(frame, 1);
	addr1  = * BEEPVM_ARGP(frame, 0);

    memp1 = ADDR2PTR(vm, addr1);
	memp2 = ADDR2PTR(vm, addr2);
	len1 = beepvm__api_strlen(vm, memp1);
	len2 = beepvm__api_strlen(vm, memp2);

	memp3 = beepvm__api_alloc(vm, (BEEPVM_STRSIZE((len1+len2))));
	if( !memp3 ) {
		 beepvm__out_of_heap_memory(vm);
	}
    else {
        dst  = (char*)(memp3+1);
        src  = (char*)(memp1+1);
        srce = src+len1; 
        for(; src != srce; src++) *dst++ = *src;
        src  = (char*)(memp2+1);
        srce = src+len2; 
        for(; src != srce; src++) *dst++ = *src;
        memp3[0] = (len1+len2);
	}

    BEEPVM_DROPFRAME(vm, frame);

    return beepvm__api_push_arg(data, PHEAP2ADDR(vm, memp3));
}

VM_WORD beepvm__strtoul(void *data)
{
	VM *vm = (VM*)data;
	VM_WORD val = 0, base = 10, addr, *memp, len;
	char tmp[11] = { 0 };
	char *p = tmp;
	
    VM_WORD *frame = 0;

    BEEPVM_SETFRAME(vm,2,frame);

	base   = * BEEPVM_ARGP(frame, 1);
	addr   = * BEEPVM_ARGP(frame, 0);

	memp = ADDR2PTR(vm, addr);
	len = beepvm__api_strlen(vm,memp);
	memcpy(tmp, (char*)(memp+1), len>10?10:len);
	val = strtoul(tmp, 0, base);

    BEEPVM_DROPFRAME(vm, frame);

	return beepvm__api_push_arg(data, val);
}

VM_WORD beepvm__utoa(void *data)
{
	VM *vm = (VM*)data;
	VM_WORD val = 0, base = 10;
	char buf[10] = { 0 };
    char *zero = "0", *result;
	int i = sizeof(buf)-2;
	
    VM_WORD *frame = 0;

    BEEPVM_SETFRAME(vm,2,frame);

    base   = * BEEPVM_ARGP(frame, 1);
	val    = * BEEPVM_ARGP(frame, 0);

	if( !val )
	{
        result = zero;
	}
    else {
        for(; val && i ; --i, val /= base)
            buf[i] = "0123456789ABCDEF"[val % base];
        result = &buf[i+1];
    }

    BEEPVM_DROPFRAME(vm, frame);

	return beepvm__api_str(vm, result);
}

VM_WORD *beepvm__api_alloc(void *data, VM_WORD sz)
{
	VM *vm = (VM*)data;
	VM_WORD *memp = 0;
	memp = mem_alloc_force_gc(vm->heap, vm->heap_end, vm->astack, vm->astack_top, sz);
	if( !memp ) {
		 beepvm__out_of_heap_memory(vm);
		 return 0;
	}
	return memp;
}

VM_WORD beepvm__api_str(void *data, char *asciiz)
{
	uint16_t len = 0, lw = 0;
	VM_WORD *memp = 0;
	VM_WORD ttmp;
	char    *p, *pend;
	VM *vm = (VM*)data;
	len = strlen(asciiz);
	lw  = len/2 + len%2;
	memp = mem_alloc_force_gc(vm->heap, vm->heap_end, vm->astack, vm->astack_top, lw + 1);
	if( !memp ) {
		 beepvm__out_of_heap_memory(vm);
         return 0;
	}
	ttmp = PHEAP2ADDR(vm, memp);
	*memp++ = len;
	if( len ) {
		p    = (char*)memp;
		pend = p+len;
		for(; p < pend; p++, asciiz++ ) *p = *asciiz;
	}
	return beepvm__api_push_arg(data, ttmp);
}

VM_WORD beepvm__api_cons(void *data)
{
	VM *vm = (VM*)data;
	VM_WORD *memp;
    VM_WORD *frame = 0;

    BEEPVM_SETFRAME(vm,2,frame);

	memp = mem_alloc_force_gc(vm->heap, vm->heap_end, vm->astack, vm->astack_top, 2);
	if( !memp ) {
		 beepvm__out_of_heap_memory(vm);
		 return 0;
	}
	*memp     = * BEEPVM_ARGP(frame, 1);
	*(memp+1) = * BEEPVM_ARGP(frame, 0);

    BEEPVM_DROPFRAME(vm, frame);

    return beepvm__api_push_arg(data, PHEAP2ADDR(vm, memp));
}

VM_WORD *beepvm__api_alloc_vect(VM *vm, VM_WORD sz)
{
    VM_WORD *memp = 0;
    (void)vm;
	memp = beepvm__api_alloc(vm, 1+sz);
    if( memp )
        memp[0] = sz;
    return memp;
}

VM_WORD beepvm__vect_len(void *data)
{
	VM *vm = (VM*)data;
    VM_WORD *memp = 0, addr = 0;
 
    VM_WORD *frame = 0;

    BEEPVM_SETFRAME(vm,1,frame);

    addr   = * BEEPVM_ARGP(frame, 0);

    memp = ADDR2PTR(vm, addr);

    BEEPVM_DROPFRAME(vm, frame);

    return beepvm__api_push_arg(vm, (VM_WORD)memp[0]);
}

VM_WORD beepvm__vect_get(void *data)
{
	VM *vm = (VM*)data;
    VM_WORD *frame, *memp = 0, addr = 0, idx = 0;
 
    BEEPVM_SETFRAME(vm,2,frame);
    
    addr = * BEEPVM_ARGP(frame, 0);
    idx  = * BEEPVM_ARGP(frame, 1);

    memp = ADDR2PTR(vm, addr);

    BEEPVM_DROPFRAME(vm, frame);
    return beepvm__api_push_arg(vm, (VM_WORD)memp[1+idx]); //#FIXME: bounding ?
}

VM_WORD beepvm__vect_set(void *data)
{
	VM *vm = (VM*)data;
    VM_WORD *frame, *memp = 0, addr = 0, idx = 0, val = 0;

    BEEPVM_SETFRAME(vm,3,frame);

    addr = * BEEPVM_ARGP(frame,0);
    idx  = * BEEPVM_ARGP(frame,1);
    val  = * BEEPVM_ARGP(frame,2);

    memp = ADDR2PTR(vm, addr);
 
    if( idx < memp[0] )
        memp[1+idx] = val; 

    BEEPVM_DROPFRAME(vm, frame);
    return 0;
}

VM_WORD beepvm__strfindall(void *data)
{
	VM *vm = (VM*)data;
    VM_WORD *frame;
	VM_WORD addr, tok, *sp, len = 0, cnt = 0, *memp = 0, *pp = 0, pos = 0;
	char *ps, *se=0;

    BEEPVM_SETFRAME(vm,2,frame);

	addr = * BEEPVM_ARGP(frame,0);
    tok  = * BEEPVM_ARGP(frame,1);
	
    sp = ADDR2PTR(vm, addr);
	len = beepvm__api_strlen(data, sp);

    ps = (char*)(sp+1);
	se = ps+len;
	for(; ps < se; ps++ ) {
		if( *ps == (char)tok ) cnt++;
	}

    memp = beepvm__api_alloc_vect(vm, cnt);

    if( memp ) { 

        pp = memp + 1;
        ps = (char*)(sp+1);
        se = ps+len;

        for(pos=0; ps < se; ps++, pos++) {
            if( *ps == (char)tok )
                *pp++ = pos;
        }
    }

    BEEPVM_DROPFRAME(vm, frame);
	return beepvm__api_push_arg(data, PHEAP2ADDR(vm, memp));
}

/*
 *
 *  NOT SO NAIVE ALGORITHM IMPL. O(1) mem, O(m+n) avg, O(m*n) worst
 */
static int memfinds(void *haystack, size_t n, void *needle, size_t m)
{
    size_t  j = 0, k = 1, l = 2;
    unsigned char*  y = (unsigned char*) haystack;
    unsigned char*  x = (unsigned char*) needle;

    if (m > n || !m || !n)
        return n;

    if (m > 1) {

        if (x[0] == x[1]) {
            k = 2;
            l = 1;
        }
        while (j <= n-m) {
            if (x[1] != y[j+1]) {
                j += k;
            } else {
                if (!memcmp(x+2, y+j+2, m-2) && x[0] == y[j])
                    return j;
                j += l;
            }
        }
    } else {
        for(j = 0; j < n; j++) if( y[j] == x[0] ) return j; 
    }
    return  n;
}


VM_WORD beepvm__strfindsub(void *data)
{
	VM *vm = (VM*)data;
	VM_WORD addr1, addr2;
    VM_WORD *sp1 = 0, *sp2 = 0;
    VM_WORD result;

    VM_WORD *frame = 0;

    BEEPVM_SETFRAME(vm,2,frame);

	addr1 = * BEEPVM_ARGP(frame, 0);
	addr2 = * BEEPVM_ARGP(frame, 1);
    
    sp1 = ADDR2PTR(vm, addr1);
    sp2 = ADDR2PTR(vm, addr2);

    result = (VM_WORD)memfinds((char*)(sp1+1), (uint16_t)beepvm__api_strlen(vm, sp1),
                               (char*)(sp2+1), (uint16_t)beepvm__api_strlen(vm, sp2));

    BEEPVM_DROPFRAME(vm, frame);

    return beepvm__api_push_arg(data, result);
}

VM_WORD beepvm__mem_stats_update(void *data) {
	VM *vm = (VM*)data;
    VM_WORD *frame = 0;

    BEEPVM_SETFRAME(vm,0,frame);

    mem_stats(vm->heap, vm->heap_end, &(vm->memstats));

    BEEPVM_DROPFRAME(vm, frame);
	return 0;
}

VM_WORD beepvm__mem_stats_get(void *data) {
	VM *vm = (VM*)data;
    VM_WORD *frame = 0;
    VM_WORD result = 0, prop = 0;

    BEEPVM_SETFRAME(vm,1,frame);

	prop = * BEEPVM_ARGP(frame, 0);

    switch(prop) {
        case 0: result = vm->memstats.allocated;
                break;
        case 1: result = vm->memstats.free;
                break;
        case 2: result = vm->memstats.max_free_block;
                break;
    }

    BEEPVM_DROPFRAME(vm, frame);
    return beepvm__api_push_arg(data, result);
}


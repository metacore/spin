/*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 */
/*
 * HISTORY
 * 31-May-97  David Becker at the University of Washington
 *	Renamed Memory.c to malloc.c
 *	Provides spin_malloc/spin_free interface for underlying kernel
 *	memory allocator.  Used by spin C code and m3 runtime.
 *	Moved incredible_pal_hack() to start/src/TARGET/spin_program.c
 *
 * 21-Apr-96  Stefan Savage (savage) at the University of Washington
 *	Added HISTORY field
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/proc.h>
#include <sys/kernel.h>
#include <sys/malloc.h>

int spin_malloc_debug = 0;

/*******************************************************
 * MALLOC SUPPORT 
 */

#ifdef __FreeBSD__
void
spin_free(void *p)
{
	if (spin_malloc_debug) {
		printf("spin_free %lx\n",p);
	}
	free(p, M_SPIN); 
}

char *
spin_malloc(long size)
{
	char *p;
	if (spin_malloc_debug) {
		printf("spin_malloc %d bytes at ", size); 
	}
	p = (char*)malloc(size, M_SPIN, 0);	/* Better wait. */
	if (!p)  {
		printf("spin_malloc out of space: %d bytes requested\n",size);
	}
	
	if (spin_malloc_debug) {
		printf("%lx\n",p); 
	}
	return p;
}
#endif

#ifdef OSF
void
spin_free(void *p)
{
	GET_CALLER(caller); /* machine/pcb.h */

	if (spin_malloc_debug) {
		printf("spin_free ra = %lx, freeing %lx\n",caller,p);
	}
	if(p == NULL)
	    return;
	kfree((vm_offset_t)p,0); 
}

char *
spin_malloc(long size)
{
	GET_CALLER(caller); /* machine/pcb.h */
	char *p;
	if (spin_malloc_debug) {
		printf("spin_malloc, ra = %lx allocating %d bytes at ", caller, size); 
	}
	p = (char*)kalloc(size);	/* Better wait. */
	if (!p)  {
		printf("spin_malloc out of space: %d bytes requested\n",size);
	}
	
	if (spin_malloc_debug) {
		printf("0x%lx\n",p); 
	}
	return p;
}

void *
memcpy(void *dst, void *src, long n)
{
    bcopy(src, dst, n);
    return dst;
}
#endif


void *
memmove(void *dst, void *src, long n)
{
    bcopy(src, dst, n);
    return src;
}

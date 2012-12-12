/*
 * Copyright (c) 1990 University of Utah.
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	from: @(#)swap_pager.h	7.1 (Berkeley) 12/5/90
 *	$Id: swap_pager.h,v 1.1.1.1 1996/08/15 03:24:52 fgray Exp $
 */

/*
 * Modifications to the block allocation data structure by John S. Dyson
 * 18 Dec 93.
 */

#ifndef	_SWAP_PAGER_
#define	_SWAP_PAGER_	1

/*
 * SWB_NPAGES can be set to any value from 1 to 16 pages per allocation,
 * however, due to the allocation spilling into non-swap pager backed memory,
 * suggest keeping SWB_NPAGES small (1-4).  If high performance is manditory
 * perhaps up to 8 pages might be in order????
 * Above problem has been fixed, now we support 16 pages per block.  Unused
 * space is recovered by the swap pager now...
 */
#define SWB_NPAGES 8
struct swblock {
	unsigned short swb_valid;	/* bitmask for valid pages */
	unsigned short swb_locked;	/* block locked */
	int swb_block[SWB_NPAGES];	/* unfortunately int instead of daddr_t */
};
typedef struct swblock *sw_blk_t;

/*
 * Swap pager private data.
 */
struct swpager {
	vm_size_t sw_osize;	/* size of object we are backing (bytes) */
	int sw_nblocks;		/* number of blocks in list (sw_blk_t units) */
	int sw_allocsize;	/* amount of space actually allocated */
	sw_blk_t sw_blocks;	/* pointer to list of swap blocks */
	short sw_flags;		/* flags */
	short sw_poip;		/* pageouts in progress */
	short sw_piip;		/* pageins in progress */
};
typedef struct swpager *sw_pager_t;

#define	SW_WANTED	0x01
#define SW_NAMED	0x02

#ifdef KERNEL

void swap_pager_init(void);
vm_pager_t swap_pager_alloc(void *, vm_size_t, vm_prot_t, vm_offset_t);
void swap_pager_dealloc(vm_pager_t);
boolean_t swap_pager_getpage(vm_pager_t, vm_page_t, boolean_t);
boolean_t swap_pager_putpage(vm_pager_t, vm_page_t, boolean_t);
boolean_t swap_pager_getmulti(vm_pager_t, vm_page_t *, int, int, boolean_t);
boolean_t swap_pager_haspage(vm_pager_t, vm_offset_t);
int swap_pager_io(sw_pager_t, vm_page_t *, int, int, int);
void swap_pager_iodone(struct buf *);
boolean_t swap_pager_clean();
void swap_pager_copy __P((vm_pager_t, vm_offset_t, vm_pager_t, vm_offset_t, vm_offset_t));
void swap_pager_freespace __P((vm_pager_t, vm_offset_t, vm_offset_t));

extern struct pagerops swappagerops;

#endif

#endif				/* _SWAP_PAGER_ */

/* 
 * Copyright (c) 1996 The University of Utah and
 * the Computer Systems Laboratory at the University of Utah (CSL).
 * All rights reserved.
 *
 * Permission to use, copy, modify and distribute this software is hereby
 * granted provided that (1) source code retains these copyright, permission,
 * and disclaimer notices, and (2) redistributions including binaries
 * reproduce the notices in supporting documentation, and (3) all advertising
 * materials mentioning features or use of this software display the following
 * acknowledgement: ``This product includes software developed by the
 * Computer Systems Laboratory at the University of Utah.''
 *
 * THE UNIVERSITY OF UTAH AND CSL ALLOW FREE USE OF THIS SOFTWARE IN ITS "AS
 * IS" CONDITION.  THE UNIVERSITY OF UTAH AND CSL DISCLAIM ANY LIABILITY OF
 * ANY KIND FOR ANY DAMAGES WHATSOEVER RESULTING FROM THE USE OF THIS SOFTWARE.
 *
 * CSL requests users of this software to return to csl-dist@cs.utah.edu any
 * improvements that they make and grant CSL redistribution rights.
 */

#include "lmm.h"

void lmm_remove_free(lmm_t *lmm, void *block, vm_size_t block_size)
{
	vm_offset_t rstart = (vm_offset_t)block;
	vm_offset_t rend = rstart + block_size;
	assert(rend > rstart);

	while (rstart < rend)
	{
		vm_offset_t size;
		lmm_flags_t flags;
		void *ptr;

		lmm_find_free(lmm, &rstart, &size, &flags);
		assert(rstart >= (vm_offset_t)block);
		if ((size == 0) || (rstart >= rend))
			break;
		if (rstart + size > rend)
			size = rend - rstart;
		ptr = lmm_alloc_gen(lmm, size, flags, 0, 0,
				    rstart, size);
		assert((vm_offset_t)ptr == rstart);
	}
}


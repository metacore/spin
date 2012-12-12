/* 
 * Copyright (c) 1995 The University of Utah and
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

void lmm_free(lmm_t *lmm, void *block, vm_size_t size)
{
	struct lmm_region *reg;
	struct lmm_node *node = (struct lmm_node*)
				((vm_offset_t)block & ~ALIGN_MASK);
	struct lmm_node *prevnode, *nextnode;

	assert(lmm != 0);
	assert(block != 0);
	assert(size > 0);

	size = (((vm_offset_t)block & ALIGN_MASK) + size + ALIGN_MASK)
		& ~ALIGN_MASK;

	/* First find the region to add this block to.  */
	for (reg = lmm->regions; ; reg = reg->next)
	{
		assert(reg != 0);
		assert((reg->nodes == 0 && reg->free == 0)
		       || ((vm_offset_t)reg->nodes >= reg->min));

		if (((vm_offset_t)node >= reg->min)
		    && ((vm_offset_t)node < reg->max))
			break;
	}

	/* Record the newly freed space in the region's free space counter.  */
	reg->free += size;
	assert(reg->free <= reg->max - reg->min);

	/* Now find the location in that region's free list
	   at which to add the node.  */
	for (prevnode = 0, nextnode = reg->nodes;
	     (nextnode != 0) && (nextnode < node);
	     prevnode = nextnode, nextnode = nextnode->next);

	/* Coalesce the new free chunk into the previous chunk if possible.  */
	if ((prevnode) &&
	    ((vm_offset_t)prevnode + prevnode->size >= (vm_offset_t)node))
	{
		assert((vm_offset_t)prevnode + prevnode->size
		       == (vm_offset_t)node);

		/* Coalesce prevnode with nextnode if possible.  */
		if (((vm_offset_t)nextnode)
		    && ((vm_offset_t)node + size >= (vm_offset_t)nextnode))
		{
			assert((vm_offset_t)node + size
			       == (vm_offset_t)nextnode);

			prevnode->size += size + nextnode->size;
			prevnode->next = nextnode->next;
		}
		else
		{
			/* Not possible -
			   just grow prevnode around newly freed memory.  */
			prevnode->size += size;
		}
	}
	else
	{
		/* Insert the new node into the free list.  */
		if (prevnode)
			prevnode->next = node;
		else
			reg->nodes = node;

		/* Try coalescing the new node with the nextnode.  */
		if ((nextnode) &&
		    (vm_offset_t)node + size >= (vm_offset_t)nextnode)
		{
			node->size = size + nextnode->size;
			node->next = nextnode->next;
		}
		else
		{
			node->size = size;
			node->next = nextnode;
		}
	}
}


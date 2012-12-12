/* 
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY 
 * 05-Oct-95  Emin Gun Sirer (egs) at the University of Washington
 *	Changed to update current_pmap correctly.
 *      Incredible pal hack is temporarily commented out.
 *
 * 24-Aug-95  Emin Gun Sirer (egs) at the University of Washington
 *	Changed to use a SPIN custom pal entry to set the ptbr without
 *	having to do an insane thread context switch.
 *
 * 22-Aug-95  Emin Gun Sirer (egs) at the University of Washington
 *      Removed spl()'s since the exclusion is done in M3 already.
 *      Deleted pmap_activate.
 *      Set asn to 1 as per Stefan's suggestion.
 *
 */
#include <kern/thread.h>
#include <vm/pmap.h>
#include <machine/pmap.h>
#include <machine/spl.h>
#include <mach/kern_return.h>
#include <mach/vm_param.h>

extern vm_offset_t avail_start, avail_end;

extern pmap_t kernel_pmap;
extern pmap_t current_pmap;
extern vm_offset_t     bootpcb_va;

#define OLD_INSANE_HACK 1

Activate(pmap_t pmap)
{
#ifdef OLD_INSANE_HACK
	thread_t thread;
	struct pcb *newpcb;
	struct pcb *oldpcb;
	spl_t s;
	vm_offset_t pa,pa2;

	if (current_pmap == pmap) {
		return;
	}
	current_pmap = pmap;
	newpcb = (struct pcb *)bootpcb_va;
	newpcb->pcb_ptbr = KSEG_TO_PHYS(pmap->level1_pt) >> PGSHIFT;
	newpcb->pcb_asn = 1;

	spinswpctx(pmap_resident_extract(kernel_pmap, newpcb), newpcb);
#else
	current_pmap = pmap;
        spinsetptbr(KSEG_TO_PHYS(pmap->level1_pt) >> PGSHIFT);
#endif
	mtpr_tbia();
}

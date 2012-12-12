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

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/kernel.h>
#include <vm/vm.h>

extern pmap_t current_pmap;

Activate(pmap_t pmap)
{
  load_cr3(pmap_extract(kernel_pmap, (vm_offset_t) pmap->pm_pdir));
  current_pmap = pmap;
}

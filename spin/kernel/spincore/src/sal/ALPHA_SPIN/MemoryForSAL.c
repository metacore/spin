/*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 */
/*
 * HISTORY
 * 02-Feb-97  Marc Fiuczynski (mef) at the University of Washington
 *	free/malloc have moved back to sal's kern_malloc.c.
 *
 * 22-Nov-96  becker at the University of Washington
 *	Added TrackMalloc
 *
 * 29-Apr-96  Stefan Savage (savage) at the University of Washington
 *	Moved malloc and free here (to help isolate m3 upcalls to 
 *	spincore).  Also changed vm_pg_alloc/free to use real physical
 *	addresses instead of KSEG addresses.
 *
 * 29-Apr-96  Stefan Savage (savage) at the University of Washington
 *	Cahnged to use real physical (non KSEG) addresses
 *
 * 21-Apr-96  Stefan Savage (savage) at the University of Washington
 *	Created.
 */

#include <sys/malloc.h>
#include <spincore/ALPHA_SPIN/TrackMalloc.h>
#include <spincore/ALPHA_SPIN/TrackDomain.h>

unsigned long bootmalloc=0, bootfree=0;
void trackmalloc(unsigned long size, int type)
{
	if (!(*TrackMalloc__Malloc)) {
		bootmalloc += size;
		return;
		}
		
	switch (type) {
		case M_DSECTION:	/* Domain section usage */
		case M_DSYMBOL:		/* Domain symbol usage */
		case M_DRELOC:		/* Domain relocation usage */
		case M_DMODULE:		/* Domain module usage */
			TrackDomain__Malloc(size); 
		default:
			TrackMalloc__Malloc(size);
		}

}

void trackfree(unsigned long size, int type)
{
	if (!(*TrackMalloc__Free)) {
		bootmalloc += size;
		return;
		}
		
	switch (type) {
		case M_DSECTION:	/* Domain section usage */
		case M_DSYMBOL:		/* Domain symbol usage */
		case M_DRELOC:		/* Domain relocation usage */
		case M_DMODULE:		/* Domain module usage */
			TrackDomain__Free(size); 
		default:
			TrackMalloc__Free(size);
		}
}

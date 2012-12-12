
/*
 * Copyright 1994, 1995, 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 */
/*
 * HISTORY
 * 14-Nov-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added drawing of a rectangle.  Used to blink as the GC is entered
 *	and exited.
 *
 * 29-Jul-96  Charles Garrett (garrett) at the University of Washington
 *	Events.h file is now found by searching the include path.
 *
 * 11-Jul-96 becker at the University of Washington
 *	Added -L boot local flag.
 *
 * 28-May-96 becker at the University of Washington
 *	Moved swap_generic.c symbols here.
 *
 * 13-May-96  Stefan Savage (savage) at the University of Washington
 *	Eliminated dead code previously needed by locore.s and trap.c 
 *
 * 10-May-96 oystr at the University of Washington
 *	Restored select_* routines for use with DEC drivers.
 *
 * 07-May-96  Stefan Savage (savage) at the University of Washington
 *	Change boot() to redirect to debugger instead of halt the processor
 *
 * 29-Apr-96  Stefan Savage (savage) at the University of Washington
 *	Removed call to pmap_scavenge_boot() (for now) because it
 *	screwed up physical pool.
 *
 * 23-Apr-96  Emin Gun Sirer (egs) at the University of Washington
 *	Implemented the lock_* interface with ulocks.
 *
 * 23-Apr-96  Stefan Savage (savage) at the University of Washington
 *	Reclaim scanvenged pages for general consumption.
 *
 * 21-Apr-96  Stefan Savage (savage) at the University of Washington
 *	Separated from upcalls.c and cleaned up.
 *
 * 08-Feb-96  Stefan Savage (savage) at the University of Washington
 *	Did easy cleanup.  Lots more needed.
 *
 * 28-Apr-95  David Becker (becker) at the University of Washington
 *	Created
 */

#include <sys/ioctl.h>
#include <sys/workstation.h>
#include <sys/inputdevice.h>
#include <sys/wsdevice.h>
#include <sys/fbinfo.h>

/* arg = 1 => funky colors, arg = 0, uniform color*/
void drawRect(int funk)
{
    extern struct fb_info 	fb_softc[];
    struct fb_info 		*fbp = &fb_softc[0];
    ws_screen_descriptor 	*sp = &fbp->screen;
    ws_depth_descriptor 	*dp = &fbp->depth[sp->root_depth];
    char 			*fb = dp->physaddr + 0x1000 /* MEF */;

    int				i,j;
    int				width = sp->width;
    
    if(dp->physaddr == NULL) {
      return;
    }

    /* draw from 0,0 to about 100,50 */
    for (j=0; j<50; j++)
    {
	for (i=0; i<100; i++)
	{
	    fb[j*width + i] = (funk? j*5%256 : 0);
	}
    }
}

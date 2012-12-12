/* 
 * Mach Operating System
 * Copyright (c) 1993,1992 Carnegie Mellon University
 * All Rights Reserved.
 * 
 * Permission to use, copy, modify and distribute this software and its
 * documentation is hereby granted, provided that both the copyright
 * notice and this permission notice appear in all copies of the
 * software, derivative works or modified versions, and any portions
 * thereof, and that both notices appear in supporting documentation.
 * 
 * CARNEGIE MELLON ALLOWS FREE USE OF THIS SOFTWARE IN ITS "AS IS"
 * CONDITION.  CARNEGIE MELLON DISCLAIMS ANY LIABILITY OF ANY KIND FOR
 * ANY DAMAGES WHATSOEVER RESULTING FROM THE USE OF THIS SOFTWARE.
 * 
 * Carnegie Mellon requests users of this software to return to
 * 
 *  Software Distribution Coordinator  or  Software.Distribution@CS.CMU.EDU
 *  School of Computer Science
 *  Carnegie Mellon University
 *  Pittsburgh PA 15213-3890
 * 
 * any improvements or extensions that they make and grant Carnegie Mellon
 * the rights to redistribute these changes.
 */
/*
 * TTD Code for mapping between system and ttd threads
 * OS DEPENDENT 
 *
 * HISTORY:
 * 29-May-96  Charles Garrett (garrett) at the University of Washington
 *	Removed explicit ALPHA_SPIN from include filename.
 *
 * 29-Apr-96  Stefan Savage (savage) at the University of Washington
 *	Short term hack, ttd_thread_to_spintask() always returns
 *	the current_pmap (won't work when we ask to debug a different
 *	thread in a different address space)
 *
 * 22-Feb-96  Charles Garrett (garrett) at the University of Washington
 *	Changed ALPHA_OSF to ALPHA_SPIN.
 *
 * ttd_thread.c,v
 * Revision 1.1  1995/03/10  21:56:06  bershad
 * Created.
 *
 */

#include <sal/salhook.h>

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/kernel.h>
#include <sys/types.h>

#include <sal/ttd/ttd_types.h>
#include <sal/ttd/ttd_debug.h>
#include <sal/ttd/ttd_thread.h>
#include <machine/pmap.h>

int ttd_thread_debug = 0;

extern pmap_t current_pmap;
task_t
ttd_thread_to_spintask(ttd_thread t)
{
	unsigned long m3task;
	return (task_t)current_pmap;
}

boolean_t
ttd_thread_valid_thread_id(ttd_thread t)
{
	return salhook_valid_thread(t);
}

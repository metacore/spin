/* 
 * Mach Operating System
 * Copyright (c) 1993, 1992 Carnegie Mellon University
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
/***********************************************************
Copyright 1992 by Digital Equipment Corporation, Maynard, Massachusetts,

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, provided 
that the above copyright notice appear in all copies and that both that 
copyright notice and this permission notice appear in supporting 
documentation, and that the name of Digital not be used in advertising 
or publicity pertaining to distribution of the software without specific, 
written prior permission.  Digital makes no representations about the 
suitability of this software for any purpose.  It is provided "as is"
without express or implied warranty.

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

/*
 * Machine specific TTD definitions for the i386mach3 configuration.
 *
 * HISTORY
 * 14-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Changed a SIGSYS to a SIGTRAP for the port to Linux (Linux has no 
 *      SIGSYS).
 *
 * $Log: ttd_i386-mach3.h,v $
 * Revision 1.2  1996/08/15 04:33:58  fgray
 * x86 merge.
 *
 * Revision 1.1  1995/07/06  01:08:04  bershad
 * New File
 *
 * Revision 1.1  1995/07/06  00:25:26  bershad
 * *** empty log message ***
 *
 * Revision 2.1.2.1  93/08/06  23:39:18  mrt
 * 	First checkin.
 * 	[93/08/06  12:29:18  grm]
 * 
 */

/*
 * Author: Gerald Malan <grm@cs.cmu.edu>
 *	   School of Computer Science
 *	   Carnegie Mellon University
 *	   Pittsburgh, PA, USA
 *
 * This work is based on previous versions by:
 *
 * Author: David Redell
 *         Michael Sclafani
 *	   DEC SRC
 *         Palo Alto CA.
 */

#ifndef _TTD_target__included_
#define _TTD_target__included_

#define TTD_TARGET_MACHINE_TYPE TTD_AT386

#define TTD_TRAP_MAPPING { \
    SIGINT, SIGTRAP, SIGBUS, SIGTRAP, SIGTRAP, SIGTRAP, \
    SIGTRAP, SIGTRAP, SIGTRAP, SIGBUS, SIGSEGV, \
    SIGBUS, SIGBUS, SIGFPE, SIGFPE, SIGTRAP, SIGILL, \
    SIGFPE, SIGFPE, SIGFPE, SIGFPE, SIGFPE, SIGFPE }

#define TTD_FETCH_STATE_TO_REGISTERS(state)	\
	bcopy(state, &registers[0], NUM_REGS * sizeof(int));

#define TTD_STORE_REGISTERS_TO_STATE(state)	\
	bcopy(&registers[0], state, NUM_REGS * sizeof(int));

#define TTD_MK_INVALID_FRAME_CHAIN(address, nextframe) \
	((address < nextframe+8) || (nextframe < 0xc0000000))

extern boolean_t ttd_mk_debug;

/*
 * The information below comes from the kttd_machdep.h file from the
 * mk/kernel/i386 directory:
 */

/*
 * Register state for gdb
 */
struct i386_gdb_register_state {
	int	eax;
	int	ecx;
	int	edx;
	int	ebx;
	int	esp;	/* 4 */
	int	ebp;	/* 5 */
	int	esi;
	int	edi;
	int	eip;	/* 8 */
	int	efl;	/* 9 */
	int	cs;
	int	ss;
	int	ds;
	int	es;
	int	fs;
	int	gs;
};

typedef struct i386_gdb_register_state ttd_machine_state;

typedef unsigned long ttd_saved_inst;

#endif	/* _TTD_target__included_ */

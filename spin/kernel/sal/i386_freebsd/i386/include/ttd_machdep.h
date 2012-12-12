/* 
 * Mach Operating System
 * Copyright (c) 1992 Carnegie Mellon University
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
 * The i386 ttd machine dependent state
 *
 * HISTORY:
 * $Log: ttd_machdep.h,v $
 * Revision 1.2  1997/06/03 16:35:33  becker
 * Replace sal with kernel/salnet/clock/machinecpu/errno/select
 *
 * Revision 1.1.1.1  1996/08/15 03:22:45  fgray
 * x86 merge
 *
 * Revision 2.2  93/05/10  23:23:38  rvb
 * 	Checkin for MK80 branch.
 * 	[93/05/10  15:11:30  grm]
 * 
 * Revision 2.1.1.2  93/04/20  11:44:35  grm
 * 	Moved some defines here from ttd_stub.h.
 * 
 * 
 * Revision 2.1.1.1  93/03/25  11:32:50  grm
 * 	Moved here from ../mach/i386.
 * 
 * 
 */

#ifndef	_KTTD_MACHDEP_H_
#define	_KTTD_MACHDEP_H_

#include <vm/vm.h>
#include <vm/vm_map.h>

#define MAX_TTD_ACTIVE	2
#define MIN_TTD_ACTIVE	0

typedef unsigned long uint32;
typedef int natural_t;
typedef vm_offset_t vm_address_t;
typedef int integer_t;
typedef long task_t;
typedef long thread_t;

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

#endif /* _KTTD_MACHDEP_H_ */

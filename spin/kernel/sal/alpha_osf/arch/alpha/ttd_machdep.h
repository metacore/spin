/*
 *
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
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
 * The alpha ttd machine dependent state
 *
 * HISTORY:
 * 07-Aug-95  Brian Bershad (bershad) at the University of Washington
 *	Added copyright.
 *
 * 30-Jan-95  Brian Bershad (bershad) at the University of Washington
 *	Created for alpha (adapted from MIPS)
 *
 * ttd_machdep.h,v
 * Revision 1.1  1995/03/15  17:48:08  bershad
 * firstcheckin
 *
 * Revision 1.1  1995/03/10  21:55:35  bershad
 * Created.
 *
 * Revision 2.1.2.1  93/06/08  13:28:10  grm
 * 	Checkin for MK80 branch.
 * 
 * Revision 2.1.1.1  93/04/20  11:39:09  grm
 * 	Initial version.
 * 	[93/04/20            grm]
 * :	ttd_machdep.h,v $
 * 
 */

#ifndef	_TTD_MACHDEP_H_
#define	_TTD_MACHDEP_H_

#include <mach/mach_types.h>
#include <mach/boolean.h>
#include <mach/vm_param.h>
#include <vm/vm_map.h>
#include <kern/task.h>

#define MAX_TTD_ACTIVE	5
#define MIN_TTD_ACTIVE	0

struct alpha_gdb_register_state {
	natural_t	v0;
	natural_t	t0;
	natural_t	t1;
	natural_t	t2;
	natural_t	t3;
	natural_t	t4;
	natural_t	t5;
	natural_t	t6;
	natural_t	t7;
	natural_t	s0;	
	natural_t	s1;
	natural_t	s2;
	natural_t	s3;
	natural_t	s4;
	natural_t	s5;
	natural_t	fp;
	natural_t	a0;	
	natural_t	a1;
	natural_t	a2;
	natural_t	a3;
	natural_t	a4;
	natural_t	a5;
	natural_t	t8;
	natural_t	t9;
	natural_t	t10;
	natural_t	t11;
	natural_t	ra;
	natural_t	t12;
	natural_t	at;
	natural_t	gp;
	natural_t	sp;
	natural_t	zero;
	natural_t	floatregs[32];
	natural_t	pc;
	natural_t	vfp;
	vm_offset_t	bad_address;
	vm_offset_t	cause; /* trap cause */	
};


typedef struct alpha_gdb_register_state ttd_machine_state;

typedef uint32 ttd_saved_inst;

#define	ttd_halt_processors()
#define	ttd_restart_processors()

#ifndef SPIN
extern boolean_t ttd_insert_breakpoint(task_t, vm_offset_t, ttd_saved_inst*);
extern boolean_t ttd_remove_breakpoint(task_t, vm_offset_t, ttd_saved_inst);
#endif

extern vm_size_t getreg_val();
extern vm_address_t addrof_alpha_reg();
#ifndef SPIN
extern int ttd_read_bytes(natural_t*, int*, natural_t*, task_t);
extern int ttd_write_bytes(natural_t*, int*, natural_t*, task_t);
#endif

#endif	/* _TTD_MACHDEP_H_ */

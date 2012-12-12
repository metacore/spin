/*
 *
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 *
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
 * TTD externs.
 *
 * HISTORY
 * 07-Aug-95  Brian Bershad (bershad) at the University of Washington
 *	Added copyright.
 *
 * ttd_stub.h,v
 * Revision 1.1  1995/03/10  21:56:10  bershad
 * Created.
 *
 * Revision 2.2  93/05/10  23:25:08  rvb
 * 	Checkin for MK80 branch.
 * 	[93/05/10  15:10:15  grm]
 * 
 * Revision 2.1.2.2  93/04/20  11:01:18  grm
 * 	Changed types for use with version 2.4 of the protocol.
 * 	[93/04/20            grm]
 * 
 * Revision 2.1.2.1  93/03/03  14:35:28  grm
 * 	Changed interface.  Version works.
 * 	[93/03/03            grm]
 * 
 * Revision 2.1.1.6  93/01/28  15:19:49  grm
 * 	Last checkin before locore rewrite.
 * 
 * Revision 2.1.1.5  92/10/23  21:23:49  grm
 * 	Added single stepping boolean.
 * 	[92/10/23            grm]
 * 
 * Revision 2.1.1.4  92/10/08  14:31:56  grm
 * 	Added ttd_debug.
 * 	[92/10/08            grm]
 * 
 * Revision 2.1.1.3  92/10/01  15:36:27  grm
 * 	KTTD Restructuring checkpoint.
 * 	[92/10/01            grm]
 * 
 * Revision 2.1.1.2  92/09/25  15:15:11  grm
 * 	checkpointing...
 * 	[92/09/25            grm]
 * 
 * Revision 2.1.1.1  92/09/09  14:45:23  grm
 * 	Initial checkin.
 * 
 */

#ifndef	_TTD_STUB_H_
#define	_TTD_STUB_H_

#include <sal/ttd/ttd_comm.h>
#include <sal/ttd/ttd_types.h>

extern integer_t ttd_active;

extern boolean_t ttd_enabled;
extern boolean_t ttd_debug;
extern boolean_t ttd_bkpt_debug;
extern boolean_t ttd_init_debug;
extern boolean_t ttd_single_stepping;
extern ttd_status_t	ttd_run_status;



#define	BYTE_ALIGNMENT		8
#define MAX_TTD_MSG_SIZE	2048


/*
 * Prototypes:
 */
extern void ttd_init(void);
extern void ttd_task_trap(int type, int code, boolean_t user_space);

#endif /* _TTD_STUB_H_ */

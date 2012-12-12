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
 * Types for Mach TTD.
 *
 * HISTORY:
   $Log: ttd_types.h,v $
   Revision 1.8  1996/09/25 01:47:25  mef
   *** empty log message ***

 * Revision 1.1  1996/08/15  23:05:45  fgray
 * x86 merge
 *
 * Revision 1.6  1996/03/18  03:33:28  becker
 * for spin-19
 *
 * Revision 1.5  1996/03/05  16:26:56  becker
 * s17-becker-cam
 *
 * Revision 1.4.54.1  1996/03/04  18:03:34  becker
 * rename textaddr loadaddr
 *
 * Revision 1.4  1996/01/28  02:57:35  savage
 * s13-becker-jan25 spin-13
 *
 * Revision 1.3.168.1  1996/01/26  00:44:31  becker
 * merged sweep code into s13
 *
 * Revision 1.3.150.1  1996/01/26  00:03:02  becker
 * Added GET_NEXT_DOMAIN rpc for domain sweep command
 *
 * ttd_types.h,v
 * Revision 1.1  1995/03/10  21:56:10  bershad
 * Created.
 *
 * Revision 2.2  93/05/10  23:25:11  rvb
 * 	Checkin for MK80 branch.
 * 	[93/05/10  15:10:37  grm]
 * 
 * Revision 2.1.2.2  93/04/20  10:55:00  grm
 * 	Changed the types so that they are useable by a more universal
 * 	set of machines.  Added some helpful comments.
 * 	[93/04/20            grm]
 * 
 * Revision 2.1.2.1  93/03/03  14:43:43  grm
 * 	Changed types and removed obsolete definitions.  Version that works.
 * 	[93/03/03            grm]
 * 
 * Revision 2.1.1.3  93/01/28  15:20:15  grm
 * 	Added ttd_loop_status.
 * 
 * Revision 2.1.1.2  92/10/08  14:33:09  grm
 * 	Small changes.
 * 	[92/10/08            grm]
 * 
 * Revision 2.1.1.1  92/09/25  15:13:58  grm
 * 	Initial checkin.
 * 	[92/09/25            grm]
 * 
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

#ifndef	_TTD_TYPES_H_
#define	_TTD_TYPES_H_

/****************************************/
/*  Mach TTD Protocol Type Definitions  */
/****************************************/

#include <machine/ttd_machdep.h>

/*
 * Someday there will be a better way to define this?  16bit chars?
 */
typedef unsigned char ttd_byte;

/*
 * These three types must be specified as integers since
 * they are part of the machine independent protocol.
 */
enum { KERNEL_TTD = 1, USER_TTD = 2 };
typedef uint32 ttd_server;

/*
 * Machines on which the teledebug protocol is available.
 * (Or will be in the future :-))
 */
enum { TTD_AT386 = 1, TTD_MIPS, TTD_ALPHA, TTD_LUNA88K };
typedef uint32 ttd_machine_type;

/*
 * In the current implementation, the key protection is
 * not used.  A master key (non zero key) is used to override any
 * previous teledebug sessions.
 */
enum { NULL_KEY, MASTER_KEY };
typedef uint32 ttd_key;

/*
 * Disconnect mode.  If disconnect_halt, then we shutdown
 * the target before disconnecting
 */
enum { DISCONNECT_LIVE, DISCONNECT_HALT };
typedef uint32 ttd_disconnect_mode;


enum { THREAD_STATE_KERNEL, THREAD_STATE_USER };
typedef uint32 ttd_machine_state_flavor;

/*
 * This boolean MUST be machine indep.
 */
typedef natural_t ttd_boolean;

/*
 * This type defines the status of the kernel teledebugger.
 *
 * RUNNING: The kernel is running, and is not being debugged.
 *
 * STEPPING: The kernel is in the middle of executing a single step.
 *
 * ONE_STOP: kttd was entered via a network interrupt and
 *           can only service this single packet.
 *
 * FULL_STOP: the kernel has been halted and is in a state
 *            that debugging can occur.  Either an sync stop_target
 *            or breakpoint/single step occurred.
 *
 */
typedef enum { RUNNING, STEPPING, ONE_STOP, FULL_STOP } ttd_status_t;

/*
 * This type specifies whether or not the current teledebug
 * operation should send a reply.
 */
typedef enum { NO_REPLY, SEND_REPLY } ttd_response_t;

/*
 * This will have to change. 64bits?! XXX
 */
typedef vm_address_t ttd_address;

#define TTD_KERNEL_MID 1

typedef uint32 ttd_id;
typedef ttd_id ttd_target;


typedef uint32 ttd_seq;
typedef natural_t ttd_count;	/* for alignment sake */

/*
 * THE Maximum size of a teledebug data block:
 */
#define TTD_MAX_BLOCK_SIZE 1400

typedef unsigned char ttd_data_block[TTD_MAX_BLOCK_SIZE];

/*
 * Flavor of breakpoint.
 */
typedef uint32 ttd_flavor;

/*
 * 
 */
typedef uint32 ttd_thread;
#define TTD_NO_THREAD 0xffffffff

typedef uint32 ttd_task;
#define TTD_NO_TASK 0xffffffff

#define TASK_NULL ((task_t) 0)

typedef struct {
	natural_t	thread_address;	/* kernel's thread address */
	natural_t	task_address;	/* kernel's task address */
	ttd_task	task_id;
	char		thread_state_msg[8];/* any string */
} ttd_thread_info;

typedef struct {
	ttd_count	length;
	char		chars[200];
} ttd_string;

typedef struct {
	natural_t		trapno;
	ttd_boolean	        user_mode;	/* last trap in user mode */
} ttd_trap_info;

typedef struct {
	ttd_boolean	is_stopped;
	ttd_boolean	is_targeted;
	ttd_thread	trapped_thread;
	ttd_string	debug_reason;
} ttd_task_info;

typedef struct {
	ttd_string	name;
	natural_t	loadaddr;	/* domain's start address */
} ttd_domain_info;

typedef ttd_task_info ttd_target_info;

#endif	/* _TTD_TYPES_H_ */

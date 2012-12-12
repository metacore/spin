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
 * Types and structures for TTD remote debugger.
 * This file is identical to the kernel file:  spin/sal/ttd/ttd_types.h
 *
 * HISTORY:
 * $Log: ttd_i386-mach3_types.h,v $
 * Revision 1.1  1996/08/15 18:40:36  fgray
 * x86 merge.
 *
 * Revision 1.5  1996/03/18  03:55:14  becker
 * fixed sweep for lots of domains and long domain names
 *
 * Revision 1.4  1996/03/18  03:34:54  becker
 * for spin-19
 *
 * Revision 1.3  1996/02/28  18:06:11  becker
 * gdb fixes merged
 *
 * Revision 1.2.36.1  1996/02/28  01:35:58  becker
 * gdb general cleanup
 *
 * Revision 1.2  1996/01/28  02:58:18  savage
 * s13-becker-jan25 spin-13
 *
 * Revision 1.1.132.1  1996/01/26  00:45:33  becker
 * merged sweep code into s13
 *
 * Revision 1.1.122.1  1996/01/25  23:38:54  becker
 * Added GET_NEXT_DOMAIN rpc for domain sweep command
 *
 * Revision 1.1  1995/07/06  01:08:05  bershad
 * New File
 *
 * Revision 1.1  1995/07/06  00:25:28  bershad
 * *** empty log message ***
 *
 * Revision 2.1.2.1  93/08/06  23:39:29  mrt
 * 	First checkin.  This version of the TTD protocol works with the 
 * 	i386 Mach TTD kernel stub in MK82.
 * 	[93/08/06  12:31:41  grm]
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
 *	   DEC Systems Research Center
 *         Palo Alto CA.
 */

/****************************************/
/*  Mach TTD Protocol Type Definitions  */
/****************************************/

#ifndef	_TTD_TYPES_H_
#define	_TTD_TYPES_H_

typedef enum {FALSE = 0, TRUE} boolean_t;

#include "gdb-cache.h"
#include <sys/types.h>

#include <defs.h>

typedef unsigned long natural_t;
typedef unsigned int uint32;

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
 * Disconnect mode. 
	 default disconnect_cont, continue target and disconnect.
	 If disconnect_nocont, then we just disconnect.
	 If disconnect_halt, then we shutdown the target before disconnecting
 */
enum { DISCONNECT_CONT, DISCONNECT_NOCONT, DISCONNECT_HALT };
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
typedef vm_offset_t ttd_address;

#define TTD_KERNEL_MID 1
#define TTD_UX_MID 3

typedef uint32 ttd_id;
typedef ttd_id ttd_target;


typedef uint32 ttd_seq;
typedef natural_t ttd_count;

/*
 * The Maximum size of a teledebug data block:
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
	natural_t	trapno;
	ttd_boolean	user_mode;	/* user mode trap */
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

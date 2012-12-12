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
 * Definitions for TTD remote debugger.
 *
 * HISTORY
 * 14-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Cosmetic changes for port to Linux.
 *
 * 07-Apr-96  Brian Bershad (bershad) at the University of Washington
 *	added predicate to test if sweeping.
 *
 * 21-Feb-96  Brian Bershad (bershad) at the University of Washington
 *	Added ttd_is_connected and ttd_get_all_domain_info
 *
 * Revision 1.1  1995/07/06  01:08:02  bershad
 * New File
 *
 * Revision 1.1  1995/07/06  00:25:24  bershad
 * *** empty log message ***
 *
 * Revision 2.1.2.1  93/08/06  23:39:14  mrt
 * 	First checkin.
 * 	[93/08/06  12:28:09  grm]
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

#define TTD_SuccessRC                  (0)
#define TTD_InvalidOperationRC        (-1)
#define TTD_InvalidTargetRC           (-2)
#define TTD_InvalidArgumentRC         (-3)
#define TTD_ServerNotAvailableRC      (-4)
#define TTD_TargetNotAvailableRC      (-5)
#define TTD_MemoryReferenceFailedRC   (-6)
#define TTD_TooManyBreakpointsRC      (-7)
#define TTD_OperationNotApplicableRC  (-8)
#define TTD_TargetInLimboRC           (-9)
#define TTD_TargetStoppedRC          (-10)
#define TTD_TargetNotStoppedRC       (-11)
#define TTD_SynchErrorRC             (-12)
#define TTD_TargetTimedOutRC         (-13)
#define TTD_ThreadInKernelCallRC     (-14)
#define TTD_BadProtocolRC            (-15)
#define TTD_BadErrorCodeRC           (-16)
#define TTD_NoReplyRC                (-17)
#define TTD_UnixErrorRC              (-18)
#define TTD_UnknownHostnameRC        (-19)

extern char *TTD_resultMsg[];

typedef struct ttd_conn *ttd_conn;

extern void ttd_set_internal_kthread(natural_t value);
extern void ttd_set_internal_ktask(natural_t value);
extern void ttd_set_internal_thread(ttd_thread value);
extern void ttd_get_user_register_addresses(CORE_ADDR *regaddrs);

#define READ_USER_REGISTER_ADDRESSES(regaddrs) \
	 ttd_get_user_register_addresses(regaddrs);



extern int ttd_is_connected();		/* TRUE if already connected */

extern int ttd_get_all_domain_info();	/* return # of domains fetched */

extern int ttd_in_get_all_domain_info();	/* return true if sweeping */


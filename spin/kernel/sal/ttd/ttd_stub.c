/*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
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
 * TTD Stub code for the kernel.  Misc, LowfaceTTD code.
 *
 * HISTORY
 #
 * 25-Jul-97  Tsutomu Owa (owa) at the University of Washington
 *	Fixed mbuf leak in ttd_task_trap().
 *
 * 09-May-96  David Becker (becker) at the University of Washington
 *	Added whatpkt() for debugging.  Removed 'already have input'
 #	message.  added comments.
 *

 s12-becker  ttd-input now checks ttd_enabled so connects at boot work ok

 * 07-Aug-95  Brian Bershad (bershad) at the University of Washington
 *	Added a few more debugging statements.
 *
 * 14-Jul-95  Emin Gun Sirer (egs) at the University of Washington
 *	Modified to work with spin trap frames and strands.
 *
 * ttd_stub.c,v
 * Revision 1.2  1995/03/21  01:24:42  pardy
 * merge for release-6
 *
 * Revision 1.1.8.1  1995/03/14  22:03:58  bershad
 * TTD update.
 *
 * Revision 1.1  1995/03/10  21:56:06  bershad
 * Created.
 *
 * Revision 2.2  93/05/10  23:25:04  rvb
 * 	Turned ttd debugging output off by default.
 * 	[93/05/10  15:15:11  grm]
 * 
 * 	Checkin for MK80 branch.
 * 	[93/05/10  15:09:57  grm]
 * 
 * Revision 2.1.2.2  93/04/20  10:59:12  grm
 * 	Changed the types for protocol version 2.4.  Changed the logic
 * 	for dropping packets.
 * 	[93/04/20            grm]
 * 
 * Revision 2.1.2.1  93/03/03  14:35:18  grm
 * 	Second version of code.  It works.
 * 	[93/03/03            grm]
 * 
 * Revision 2.1.1.9  93/01/28  15:19:13  grm
 * 	Added ttd_loop_status.  Last checkin before locore rewrite.
 * 
 * Revision 2.1.1.8  93/01/22  15:53:23  grm
 * 	Added request length checks.
 * 
 * Revision 2.1.1.7  93/01/21  13:05:26  grm
 * 	Changed to Ansi C prototypes.  Modified code for single stepping.
 * 
 * Revision 2.1.1.6  92/10/23  21:23:09  grm
 * 	First pass at single stepping.  Left over code from debugging.
 * 	[92/10/23            grm]
 * 
 * Revision 2.1.1.5  92/10/08  14:30:40  grm
 * 	Small changes.  Checkin before ttd-ether rewrite.
 * 	[92/10/08            grm]
 * 
 * Revision 2.1.1.4  92/10/01  15:37:20  grm
 * 	KTTD restructuring checkpoint.
 * 	[92/10/01            grm]
 * 
 * Revision 2.1.1.3  92/09/30  13:31:16  grm
 * 	Added sync and async routines.  Filled in ttd_trap.
 * 	[92/09/30            grm]
 * 
 * Revision 2.1.1.2  92/09/25  15:12:12  grm
 * 	checkpointing...
 * 	[92/09/25            grm]
 * 
 * Revision 2.1.1.1  92/09/09  14:44:34  grm
 * 	Initial Checkin.
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

#include <sal/salnet.h>

#include <sys/types.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/kernel.h>
#include <sys/reboot.h>
#include <sys/mbuf.h>

#include <sal/ttd/ttd_types.h>
#include <sal/ttd/ttd_msg.h>
#include <sal/ttd/ttd_stub.h>

#include <netinet/in.h>

#include <sal/ttd/ttd_server.h>
#include <sal/ttd/ttd_interface.h>

#include <machine/cpu.h>

extern void ttd_intr();

/*
 *	Status and Debugging flags
 */
boolean_t 	ttd_enabled = FALSE;	/* are we catching traps with ttd? */
integer_t 	ttd_active = MIN_TTD_ACTIVE;	/* are we in ttd NOW? */
boolean_t	ttd_debug = FALSE; /* are we debugging ttd? */

boolean_t 	ttd_init_debug = FALSE; /* != 0 waits for host at bootstrap */
	/* use boot -fl T  so alpha_init.c:alpha_bootstrap() will set it */

/*
 * Pointer to the current request, and its length.  It must be a
 * global since it must be set in the async entry point which may
 * indirectly jump to the ttd_task_trap routine (as in the i386at
 * version).
 */
static struct mbuf 	*ttd_current_mbuf=0;
static salnet_route 	ttd_current_route;


ttd_status_t	ttd_run_status;

/*
 * Initialize the TTD code.
 * 
 * Allocate static buffer, etc.
 *
 */
void ttd_init(void)
{
	ttd_enabled = TRUE;

	/*
	 * Init the ttd_server.
	 */
	ttd_server_initialize();
	
	printf("TTD enabled, protocol version: %d.%d.\n",
	       TTD_VERSION/10, TTD_VERSION%10);

#ifdef __FreeBSD__
	if (boothowto & RB_KDB) {
#else
        if (ttd_init_debug) {
#endif
	    extern boolean_t ttd_stopping_on_boot;
	    /*
	     * Debugging init procedure, break here.
	     */
	    printf("TTD waiting...");
	    ttd_stopping_on_boot = TRUE;
	    ttd_break();
	    printf(" connected.\n");
	    
	}
}

/*
 * ttd_task_trap:
 *
 *  This routine is called by the ttd_trap routine in the
 * ttd_interface.c file.  Both asyncronous and syncronous entries call
 * this routine.
 *
 */
void ttd_task_trap(int type, int code, boolean_t user_space)
{
	boolean_t ttd_is_poller;
	salnet_route route;
	salnet_err err=0;
	int msecmax= 10000;
	struct mbuf *m;
	int getfrom_salnet_udprecv=0;


	if (ttd_debug)  {
		printf("ttd_task_trap entered. type = %d, code = %x, from %s space %s %s\n",
		       type, code, (user_space ? "USER" : "KERNEL"),
		       ((ttd_run_status == FULL_STOP) ? "fullstop" : "notfullstop"),
		       (ttd_single_stepping ? "singlestep" :""));
	}
    
	   
	if (ttd_run_status == FULL_STOP) {
	       
	        /*
		 * We're at a full stop.  So:
		 * 
		 * -  Halt the other processors
		 * -  Take us out of single stepping mode
		 *
		 */
		ttd_halt_processors();

		/*
		 * Update the kernel state. ie. we're stopped now...
		 */
		ttd_stop_target();

		if (ttd_debug)
			printf("ttd_task_trap: stopped kernel target.\n");
	
		/*
		 * Turn off single stepping if this is a
		 * single step trap/breakpoint.
		 */
		if (ttd_single_stepping) {
			if (!ttd_clear_single_step())
				printf("ttd_trap: Couldn't clear single stepping!!\n");
		}

        }

        if (ttd_single_stepping) {
		if (!ttd_clear_single_step())
			printf("ttd_trap: Couldn't clear single stepping!!\n");
        }

	/*
	 * Update kernel target info (ie. why we're stopped, etc.)
	 */
	ttd_type_to_ttdtrap(type);

	/*
	 * Turn off the transport chips, should be conditional on arch.
	 */

	/* since ttd_handle_async calls us via BPT trap, it is possi
	   that some other service is in net_poll mode.  We should not
	   call net_poll_exit() if this is the case
	*/
	salnet_begin();
	salnet_getlocalroute(&route);
	route.localudpport = 0x8765;

	if(ttd_current_mbuf) {
		m = ttd_current_mbuf;
		ttd_current_mbuf=0;
		route.remotether = ttd_current_route.remotether; /* ptr dup */
		route.remotip = ttd_current_route.remotip;
		route.remotudpport = ttd_current_route.remotudpport;
		}
	else	{/* wait for server to probe if we just took an async bpt. */
		route.remotip = INADDR_ANY;
		route.remotudpport = INADDR_ANY;
		while(salnet_udprecv(&route, 10000, &m)==SALNET_TIMEOUT)
			;
		getfrom_salnet_udprecv = 1;
		}

	if (!m || err) {
		if (err!=SALNET_TIMEOUT) salnet_perror("ttd_task_trap",err);
		printf("ttd_task_trap: no contact with gdb. resuming.\n");
		salnet_end();
		return;
		}

	/*
	 * The packet "command" loop, where we respond to remote
	 * debugging commands.
	 *
	 * Service request(s) now...
	 */

	if (ttd_debug)
		printf("ttd_task_trap: servicing requests\n");

	ttd_service_request(&route,m);
	/* Have to free mbufs that came from salnet_udprecv() */
	if (getfrom_salnet_udprecv) {
		m_freem(m);
	}
	while(ttd_run_status == FULL_STOP) {
		err = salnet_udprecv(&route, 10000, &m);
		if (err==SALNET_TIMEOUT) continue;

		if (err) {
			salnet_perror("ttd cmd loop",err);
			break;
			}

		ttd_service_request(&route, m);
		m_freem(m);
		}

	/*
	 * Turn off the transport chips, should be conditional by arch.
	 */
	salnet_end();
}



#ifdef __FreeBSD__
#include <sys/queue.h>
#endif

#include <net/if.h>
#include <netinet/in.h>
#include <netinet/if_ether.h>
#include <netinet/in_systm.h>
#include <netinet/ip.h>
#include <net/route.h>
#include <net/if_types.h>
#include <netinet/in_pcb.h>
#include <netinet/ip_var.h>
#include <netinet/udp.h>
#include <netinet/udp_var.h>

salnet_route ttd_route = {
	0x8765,		/*unsigned short	localudpport;*/
	0,		/*unsigned short	remotudpport;*/
	IPPROTO_UDP,	/*unsigned char	ip_protocol;*/
	INADDR_ANY,	/*ipaddr_t localip;*/
	INADDR_ANY,	/*ipaddr_t remotip;*/
	ETHERTYPE_IP,	/*unsigned short	ether_type;*/
	0,	/*etheraddr_t localether;*/
	0,	/*etheraddr_t remotether;*/
	0,	/*struct ifnet *ifp;*/
	0,	/*void (*ifintr)(int);*/
	0,	/*struct ifqueue *ifq;*/
	};

int
ttd_ether_packet(struct mbuf *m) 
{
	struct ether_header *ehp;
	struct ip *ip;
	struct udphdr *uh;
	extern ttd_status_t	ttd_run_status;

	if (ttd_run_status || !salnet_udp_check(&ttd_route, m))
		return 0;


	ehp = mtod(m,struct ether_header*);
	m_adj(m, sizeof(struct ether_header));
	ip = mtod(m, struct ip *);
	m_adj(m, sizeof(struct ip) /* XXX does not account for options */);
        uh = mtod(m, struct udphdr *);
	m_adj(m, sizeof(struct udphdr));

	
	salnet_getlocalroute(&ttd_current_route);
	ttd_current_route.remotether = ehp->ether_shost; /* pointer dup */
	ttd_current_route.remotip = ip->ip_src.s_addr;
	ttd_current_route.remotudpport = ntohs(uh->uh_sport);

	if (ttd_run_status == RUNNING) {
		ttd_current_mbuf = m;
		ttd_run_status = ONE_STOP;
		if (ttd_debug) printf("ttd_ether_packet: call breakpoint\n");
		ttd_intr();  /* enter ttd through breakpoint trap */
		}
	else	printf("ttd_ether_packet: got packet while stopped\n");

	m_freem(m);
	return 1;
}

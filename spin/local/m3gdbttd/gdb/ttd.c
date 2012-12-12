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
 * Stubs for TTD RPC
 *
 * HISTORY
 * 14-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Cosmetic changes for port to Linux.
 *
 *	   Pittsburgh, PA, USA
 *
 * This work is based on previous versions by:
 *
 * Author: David Redell
 *         Michael Sclafani
 *	   DEC SRC
 *         Palo Alto CA.
 */

#include <stdio.h>
#include <strings.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#include <sys/file.h>
#include <errno.h>
#ifndef linux
#include <sgtty.h>
#endif
#include <errno.h>
#include <signal.h>
#include <sys/wait.h>
#include <sys/time.h>

#include <defs.h>

#include "ttd.h"
#include "ttd_msg.h"

extern int errno;

#define DEFAULTPORT	0x8765
int ttd_retry_count   = 4;      /* default number of retries */
int ttd_retry_timeout = 300000; /* length of each retry */

struct ttd_conn {
	ttd_server server;
	int socket;
	struct sockaddr_in addr;
	ttd_seq seq;
};

char *TTD_resultMsg[] = {
	"TTD operation succeeded",
	"invalid TTD operation",
	"invalid TTD target (process may have died)",
	"invalid TTD argument",
	"wrong TTD server (nub may have crashed)",
	"TTD target not available (another client has control)",
	"TTD memory reference failed",
	"TTD: too many breakpoints",
	"TTD operation not applicable",
	"TTD target is in limbo (vfork)",
	"TTD target is stopped",
	"TTD target is not stopped",
	"TTD synchronization error",
	"TTD target timed out (dumping core or held by stopped/crashed Taos)",
	"TTD: thread can't be modified during kernel call",
	"unknown TTD protocol version?!?",
	"unknown TTD error code?!?",
	"no reply from TTD server",
	"Unix error result encountered during TTD operation",
	"unknown target hostname"
	};

extern boolean_t ttd_debug_enabled;
int ttd_debug_2 = TRUE;

int ttd_bind(char *hostname, int port, ttd_server server_type,
	     ttd_conn *conn)
{
	ttd_conn ttd;
	struct hostent *hp;
	struct servent *sp;
	
	ttd = (ttd_conn) malloc( sizeof( *ttd ) );
	
	if (hostname == NULL)
		hostname = "localhost";
	
	hp = gethostbyname( hostname );
	if (hp == 0)
		return TTD_UnknownHostnameRC;
	
	ttd->addr.sin_family = hp->h_addrtype;
	bcopy( (char *) hp->h_addr, (char *) &ttd->addr.sin_addr, hp->h_length );
	endhostent();
	
	if (port != 0)
		ttd->addr.sin_port = htons( port );
	else {
		sp = getservbyname( "ttd", "udp" );
		if (sp != 0)
			ttd->addr.sin_port = sp->s_port;
		else
			ttd->addr.sin_port = htons( DEFAULTPORT );
	}
	endservent();
	
	ttd->socket = socket( AF_INET, SOCK_DGRAM, 0 );
	if (ttd->socket < 0)
		return TTD_UnixErrorRC;
	
	if (connect( ttd->socket, (struct sockaddr *) &(ttd->addr),
		    sizeof( struct sockaddr_in ) ) < 0)
		return TTD_UnixErrorRC;
	
	ttd->server = server_type;
	ttd->seq = time( (long *) 0 ) * 2;
	
	*conn = ttd;
	return TTD_SuccessRC;
}

int ttd_free(ttd_conn t)
{
	int result;
	
	/* TODO: Check that the target has been disconnected.  */
	result = close( t->socket );
	if (result < 0)
		return TTD_UnixErrorRC;
	t->socket = -1;
	bzero( &t->addr, sizeof( t->addr ) );
	free( (char *) t );
	return TTD_SuccessRC;
}

int return_code(ttd_code_t code)
{
	switch (code) {
	    case Okay:                   return TTD_SuccessRC;
	    case InvalidOperation:       return TTD_InvalidOperationRC;
	    case InvalidTarget:          return TTD_InvalidTargetRC;
	    case InvalidArgument:        return TTD_InvalidArgumentRC;
	    case ServerNotAvailable:     return TTD_ServerNotAvailableRC;
	    case TargetNotAvailable:     return TTD_TargetNotAvailableRC;
	    case MemoryReferenceFailed:  return TTD_MemoryReferenceFailedRC;
	    case TooManyBreakpoints:     return TTD_TooManyBreakpointsRC;
	    case OperationNotApplicable: return TTD_OperationNotApplicableRC;
	    case TargetStopped:          return TTD_TargetStoppedRC;
	    case TargetNotStopped:       return TTD_TargetNotStoppedRC;
	    case SynchError:             return TTD_SynchErrorRC;
	    case TargetInLimbo:          return TTD_TargetInLimboRC;
	    case TargetTimedOut:         return TTD_TargetTimedOutRC;
	    case ThreadInKernelCall:     return TTD_ThreadInKernelCallRC;
	    default:                     return TTD_BadErrorCodeRC;
	}
}

/*
 * Used by our code.
 */
int	use_clean_out_code = TRUE;

static void clean_out_socket(ttd_conn connection)
{
	int counter = 0;
	fd_set readfds;
	struct timeval timeout;
	int nfound;
	char buf[1024];

	timeout.tv_sec = 0;
	timeout.tv_usec = 1000;

	/*
	 * Loop until we've read all of the messages on the
	 * socket.
	 */
	for (;;) {
		FD_ZERO(&readfds);
		FD_SET(connection->socket, &readfds);

		nfound = select(connection->socket+1,
				&readfds,
				(int *) NULL,
				(int *) NULL,
				&timeout);

		if (nfound == 0)
			break;

		if (nfound < 0) {
			printf("Clean_out_socket: error on select.\n");
			return;
		}

		counter++;

		nfound = recv(connection->socket, (char *) buf, sizeof(buf), 0);

		if (nfound < 0) {
			printf("Clean_out_socket: error on recv");
			return;
		}
	}
	
	if (ttd_debug_enabled && counter)
		printf("Clean_out_socket, count = %d\n", counter);

}

static int ttd_rpc(ttd_conn		connection,
		   enum ttd_operations	opcode,
		   ttd_target		target,
		   ttd_request_t	request,
		   ttd_reply_t		reply,
		   boolean_t		take_interrupts)
{
	ttd_operation op;
	int i, n, result;
	int readfds;
	int seq;
	struct timeval timeout;
	
	op = (ttd_operation) opcode;
	seq = (op < (ttd_operation) DISCONNECT_FROM_TARGET) ? 0 : ++(connection->seq);
	n = sizeof(*request);

	/*
	 * Convert to network byte order.	
	 */
	request->server = htonl(connection->server);
	request->seq = htonl(seq);
	request->operation = htonl(op);
	request->target = htonl(target);
	
	timeout.tv_sec = 10;
	timeout.tv_usec = ttd_retry_timeout;

	/*
	 * Throw away any previous messages.
	 */
	if (use_clean_out_code) {
		clean_out_socket(connection);
	}

	for (i = 0; i < ttd_retry_count; i++) {

		result = send(connection->socket, (char *) request, n, 0);
		if (result < 0) {
			printf("ttd_rpc:  error on send\n");
			return TTD_UnixErrorRC;
		}
		if (result != n) {
			(void) fprintf(stderr, "?ttd: partial send\n");
			return TTD_UnixErrorRC;
		}
		
		readfds = 1 << connection->socket;
		result = select(connection->socket+1, &readfds, (int *) NULL,
				(int *) NULL, &timeout);

		if (result < 0) {
			if (errno != EINTR) {
				printf("ttd_rpc: error on select\n");
				return TTD_UnixErrorRC;
			} else if (take_interrupts) {
				printf("ttd_rpc: operation interrupted\n");
				return TTD_TargetTimedOutRC;
			}
				
		}
		if (result > 0) {
			result = recv(connection->socket, (char *) reply,
				      sizeof( *reply ), 0);
			if (result < 0) {
				if (errno != EINTR) {
					printf("ttd_rpc: recv, eintr\n");
					return TTD_UnixErrorRC;
				} else if (take_interrupts) {
					printf("ttd_rpc: operation interrupted\n");
					return TTD_TargetTimedOutRC;
				}
					
			}

			/*
			 * Convert back to host byte order.	
			 */
			reply->seq = ntohl(reply->seq);
			reply->target = ntohl(reply->target);
			reply->operation = ntohl(reply->operation);
		
			if ((reply->server == request->server) 
			    && (reply->operation == op)
			    && ((op < (ttd_operation) DISCONNECT_FROM_TARGET) 
				|| ((reply->seq == seq) 
				    && (reply->target == target)))) {

				reply->server = ntohl(reply->server);

				return return_code( reply->result.code );
			}
		}
		if (result == 0) {
		    printf("TTD timeout\n");
		}
	}
	return TTD_NoReplyRC;
}

int ttd_probe_server(ttd_conn connection, ttd_machine_type *machine)
{
	struct ttd_request request;
	struct ttd_reply reply;
	int result;

	if (ttd_debug_enabled)
		printf("ttd_probe_server\n");
	
	result = ttd_rpc(connection, PROBE_SERVER, 0L, &request, &reply, TRUE);
	if (result < TTD_SuccessRC)
		return result;

	reply.u.probe_server.version = ntohl(reply.u.probe_server.version);
	reply.u.probe_server.machine_type = ntohl(reply.u.probe_server.machine_type);
	
	switch (reply.u.probe_server.version) {
	    case 1:
	    case 2:
		(void) fprintf( stderr, "?ttd: protocol version 1 unsupported\n" );
		return TTD_BadProtocolRC;
		
	    case TTD_VERSION:
		    *machine = reply.u.probe_server.machine_type;
		return result;
		
	    default:
		(void) fprintf(stderr, "?ttd: unknown protocol version %d %d\n",
			       reply.u.probe_server.version,
			       reply.u.probe_server.machine_type);
		return TTD_BadProtocolRC;
	}
    return 0;
}

int ttd_get_target_info(ttd_conn	connection,
			ttd_id		mid,
			ttd_target_info	*info)
{
	struct ttd_request request;
	struct ttd_reply reply;
	int result;

	if (ttd_debug_enabled)
		printf("ttd_get_target_info (mid = %d)\n", mid);

	request.u.get_target_info.target = mid;
	result = ttd_rpc(connection, GET_TARGET_INFO,
			 0L, &request, &reply, TRUE);
	if (result == TTD_SuccessRC) {
		if (info != NULL)
			*info = reply.u.get_target_info.target_info;
	}
	return result;
}

int ttd_connect_to_target(ttd_conn connection,
			  ttd_id mid,
			  ttd_key key,
			  ttd_target *target,
			  ttd_target_info *info)
{
	struct ttd_request request;
	struct ttd_reply reply;
	int result;
	
	if (ttd_debug_enabled)
		printf("ttd_connect_to_target (mid = %d) (key = %d)\n", mid, key);

	request.u.connect_to_target.target = mid;
	request.u.connect_to_target.key = key;
	if (ttd_debug_enabled)  {
		printf("ttd_connect_to_target(2) (mid = %d) (key = %d)\n",
		       request.u.connect_to_target.target,
		       request.u.connect_to_target.key);
	}
		       
	result = ttd_rpc(connection, CONNECT_TO_TARGET,
			 0L, &request, &reply, TRUE);
	if (result == TTD_SuccessRC) {
		*target = reply.u.connect_to_target.target;
		if (info != NULL)
			*info = reply.u.connect_to_target.target_info;
	}
	return result;
}

int ttd_disconnect_from_target(ttd_conn connection,
			       ttd_target target,
			       ttd_disconnect_mode mode)
{
	struct ttd_request request;
	struct ttd_reply reply;
	
	if (ttd_debug_enabled)
		printf("ttd_disconnect_from_target: %d\n", mode);

	if (mode == DISCONNECT_HALT) {
		request.u.disconnect_from_target.mode = 1;
	        printf("Halting target\n");
		}
	else	request.u.disconnect_from_target.mode = 0;
	
	return ttd_rpc(connection, DISCONNECT_FROM_TARGET,
		       target, &request, &reply, TRUE);
}

int ttd_read_from_target(ttd_conn connection,
			 ttd_target target,
			 ttd_address from,
			 void *buf,
			 int start,
			 int length)
{
	struct ttd_request request;
	struct ttd_reply reply;
	int result;
	int count;
	
	if (ttd_debug_enabled)
		printf("ttd_read_from_target (from = 0x%lx) (length = 0x%x)\n",
		       from, length);

	request.u.read_from_target.start = from;
	request.u.read_from_target.count = length;
	result = ttd_rpc(connection, READ_FROM_TARGET,
			 target, &request, &reply, TRUE);
	if (result < TTD_SuccessRC && result != TTD_MemoryReferenceFailedRC) {
		/* couldn't get anything */
		return result;
	}
	count = reply.u.read_from_target.count;
	if (count == 0)  {
		/* couldn't get anything */
		return TTD_MemoryReferenceFailedRC;
	}
	if (count > length) {
		(void) fprintf(stderr, "?ttd: too many bytes read (0x%x/0x%x)\n",
			       length, count);
		count = length;
	}else if (count < length) {
		(void) fprintf(stderr, "?ttd: too few bytes read (0x%x/0x%x)\n",
			       length, count);
	}

	if (count < 0) {
		fprintf(stderr, "?ttd: count < 0!!!\n");
		count = 0;
	}

	if (ttd_debug_enabled)  {
		printf("0x%lx\n", *(unsigned long*)&reply.u.read_from_target.data[0]);
	}
	bcopy( (char *) &reply.u.read_from_target.data[0], (char *) buf + start,
	      count );
	return count;
}

int ttd_write_into_target(ttd_conn connection,
			  ttd_target target,
			  ttd_address into,
			  void *buf,
			  int start,
			  int length)
{
	struct ttd_request request;
	struct ttd_reply reply;

	if (ttd_debug_enabled)
		printf("ttd_write_into_target (into = 0x%x) (length = 0x%x)\n",
		       into, length);
	
	if (length > TTD_MAX_BLOCK_SIZE)
		return TTD_InvalidArgumentRC;
	request.u.write_into_target.start = into;
	request.u.write_into_target.count = length;
	bcopy( (char *) buf + start, (char *) &request.u.write_into_target.data[0],
	      length );
	return ttd_rpc(connection, WRITE_INTO_TARGET,
		       target, &request, &reply, TRUE);
}

int ttd_get_next_thread(ttd_conn connection,
			ttd_target target,
			ttd_thread thread,
			ttd_thread *next)
{
	struct ttd_request request;
	struct ttd_reply reply;
	int result;
	
	if (ttd_debug_enabled)
		printf("ttd_get_next_thread (thread = 0x%x\n", thread);

	request.u.get_next_thread.thread = thread;
	result = ttd_rpc(connection, GET_NEXT_THREAD,
			 target, &request, &reply, TRUE);
	if (result == TTD_SuccessRC)
		*next = reply.u.get_next_thread.next;
	return result;
}

int ttd_get_next_domain(ttd_conn connection,
			ttd_target target,
			ttd_domain_info *info)
{
	struct ttd_request request;
	struct ttd_reply reply;
	int result;
	
	if (ttd_debug_enabled)
		printf("ttd_get_next_domain \n");

	result = ttd_rpc(connection, GET_NEXT_DOMAIN,
			 target, &request, &reply, TRUE);
	if (result == TTD_SuccessRC)
		*info = reply.u.get_domain_info.domain;
	return result;
}

int ttd_get_thread_info(ttd_conn connection,
			ttd_target target,
			ttd_thread thread,
			ttd_thread_info *info,
			ttd_trap_info *trap_info,
			ttd_machine_state_flavor flavor,
			ttd_machine_state *state)
{
	struct ttd_request request;
	struct ttd_reply reply;
	int result;
	
	if (ttd_debug_enabled)
		printf("ttd_get_thread_info (thread = 0x%x)\n", thread);

	request.u.get_thread_info.thread = thread;
	request.u.get_thread_info.flavor = flavor;
	result = ttd_rpc(connection, GET_THREAD_INFO,
			 target, &request, &reply, TRUE);
	if (result == TTD_SuccessRC) {
		if (info != NULL)
			*info = reply.u.get_thread_info.thread_info;
		if (trap_info != NULL)
			*trap_info = reply.u.get_thread_info.trap_info;
		if (state != NULL)
			*state = reply.u.get_thread_info.machine_state;
	}
	ttd_set_internal_thread(thread);
	ttd_set_internal_kthread(
		    reply.u.get_thread_info.thread_info.thread_address);
	ttd_set_internal_ktask(
		    reply.u.get_thread_info.thread_info.task_address);
	
	return result;
}

int ttd_set_thread_info(ttd_conn connection,
			ttd_target target,
			ttd_thread thread,
			ttd_trap_info trap_info,
			ttd_machine_state state)
{
	struct ttd_request request;
	struct ttd_reply reply;
	
	if (ttd_debug_enabled)
		printf("ttd_set_thread_info (thread = 0x%x)\n", thread);

	request.u.set_thread_info.thread	= thread;
	request.u.set_thread_info.trap_info	= trap_info;
	request.u.set_thread_info.machine_state = state;
	
	return ttd_rpc(connection, SET_THREAD_INFO,
		       target, &request, &reply, TRUE);
}

int ttd_stop_target(ttd_conn connection, ttd_target target)
{
	struct ttd_request request;
	struct ttd_reply reply;
	
	if (ttd_debug_enabled)
		printf("ttd_stop_target\n");

	return ttd_rpc(connection, STOP_TARGET,
		       target, &request, &reply, TRUE);
}

int ttd_probe_target(ttd_conn connection, ttd_target target, ttd_target_info *info)
{
	struct ttd_request request;
	struct ttd_reply reply;
	int result;
	
	if (ttd_debug_enabled)
		printf("ttd_probe_target\n");

	result = ttd_rpc(connection, PROBE_TARGET,
			 target, &request, &reply, TRUE);
	if (result == TTD_SuccessRC)
		*info = reply.u.probe_target.target_info;
	return result;
}

int ttd_restart_target(ttd_conn connection, ttd_target target)
{
	struct ttd_request request;
	struct ttd_reply reply;
	
	if (ttd_debug_enabled)
		printf("ttd_restart_target\n");

	return ttd_rpc(connection, RESTART_TARGET,
		       target, &request, &reply, FALSE);
}

int ttd_set_breakpoint(ttd_conn connection,
		       ttd_target target,
		       ttd_thread thread,
		       ttd_address at,
		       ttd_flavor flavor,
		       ttd_saved_inst *saved_inst)
{
	struct ttd_request request;
	struct ttd_reply reply;
	int result;
	
	if (ttd_debug_enabled)
		printf("ttd_set_breakpoint (address = 0x%lx)\n", at);

	request.u.set_breakpoint_in_target.address = at;
	request.u.set_breakpoint_in_target.flavor = flavor;
	request.u.set_breakpoint_in_target.thread = thread;
	
	result = ttd_rpc(connection, SET_BREAKPOINT_IN_TARGET,
			 target, &request, &reply, TRUE);
	if ((result < TTD_SuccessRC) || (saved_inst == NULL))
		return result;
	
	*saved_inst = reply.u.set_breakpoint_in_target.saved_inst;
	return result;
}

int ttd_clear_breakpoint(ttd_conn connection,
			 ttd_target target,
			 ttd_thread thread,
			 ttd_address at)
{
	struct ttd_request request;
	struct ttd_reply reply;
	
	if (ttd_debug_enabled)
		printf("ttd_clear_breakpoint (address = 0x%x)\n", at);

	request.u.clear_breakpoint_in_target.address = at;
	request.u.clear_breakpoint_in_target.thread = thread;
	return ttd_rpc(connection, CLEAR_BREAKPOINT_IN_TARGET,
		       target, &request, &reply, TRUE);
}

int ttd_get_next_breakpoint(ttd_conn connection,
			    ttd_target target,
			    ttd_address prev,
			    ttd_address *addr,
			    ttd_flavor *flavor,
			    ttd_saved_inst *saved_inst)
{
	struct ttd_request request;
	struct ttd_reply reply;
	int result;
	
	if (ttd_debug_enabled)
		printf("ttd_get_next_breakpoint\n");

	request.u.get_next_breakpoint_in_target.address = prev;
	request.u.get_next_breakpoint_in_target.thread = 0;
	request.u.get_next_breakpoint_in_target.all_breaks = TRUE;
	result = ttd_rpc(connection, GET_NEXT_BREAKPOINT_IN_TARGET,
			 target, &request, &reply, TRUE);
	if (result == TTD_SuccessRC) {
		if (addr != NULL)
			*addr = reply.u.get_next_breakpoint_in_target.address;
		if (flavor != NULL)
			*flavor = reply.u.get_next_breakpoint_in_target.flavor;
		if (saved_inst != NULL)
			*saved_inst = reply.u.get_next_breakpoint_in_target.saved_inst;
	}
	return result;
}

int ttd_single_step_thread(ttd_conn connection,
			   ttd_target target,
			   ttd_thread thread)
{
	struct ttd_request request;
	struct ttd_reply reply;
	
	if (ttd_debug_enabled)
		printf("ttd_single_step_thread\n");

	request.u.single_step_thread.thread = thread;
	return ttd_rpc(connection, SINGLE_STEP_THREAD,
		       target, &request, &reply, FALSE);
}

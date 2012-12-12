/*
 *
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
 * TTD Communications parsing code for the kernel ttd server.
 *
 * HISTORY:
 * 25-Jul-97  Tsutomu Owa (owa) at the University of Washington
 *	Fixed possible mbuf leak.
 *
 * 20-Feb-97  becker at the University of Washington
 *	Changed mbuf ops to make valid mbufs for freebsd ether drivers
 *
 * 29-May-96  Charles Garrett (garrett) at the University of Washington
 *	Removed explicit ALPHA_SPIN from include filename.
 *
 * Revision 1.9  1996/02/27  05:38:10  garrett
 * new ALPHA_SPIN template
 *
 * Revision 1.8.22.1  1996/02/23  21:23:43  garrett
 * compiler fixes, create ALPHA_SPIN template
 *
 * Revision 1.8.16.1  1996/02/14  01:36:26  garrett
 * Changed ALPHA_OSF to ALPHA_SPIN.
 *
 s14-becker
 	check if requests for mbufs are succesful and print warnings if
	they fail.  tidied up some comments and debug msgs.

 * Revision 1.8  1996/01/28  02:57:34  savage
 * s13-becker-jan25 spin-13
 *
 * Revision 1.7.126.1  1996/01/26  00:44:29  becker
 * merged sweep code into s13
 *
 * Revision 1.7.108.1  1996/01/26  00:03:01  becker
 * Added GET_NEXT_DOMAIN rpc for domain sweep command
 *
 * 03-Aug-95  Stefan Savage (savage) at the University of Washington
 *	Some crazy debugger fix in get_state_action... ask brian
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



/*
 * TTD Communications parsing code for the kernel ttd server.
 *
 * HISTORY
 *
 * 06-Aug-95  Brian Bershad (bershad) at the University of Washington
 *	Clean up for robustness.  Fix clear_break_action to get spintask
 *	of current the target thread, not a random stack value.  Removed
 *	pre-spin whist msgs.
 *
 */

#include <sal/salhook.h>
#include <sal/salnet.h>

#include <sys/types.h>
#include <sys/param.h>
#include <sys/kernel.h>
#include <sys/mbuf.h>

#include <sal/ttd/ttd_comm.h>
#include <sal/ttd/ttd_types.h>
#include <sal/ttd/ttd_msg.h>
#include <sal/ttd/kernel_ttd.h>
#include <sal/ttd/ttd_stub.h>
#include <sal/ttd/ttd_server.h>
#include <sal/ttd/ttd_thread.h>
#include <sal/ttd/ttd_debug.h>
#include <sal/ttd/ttd_interface.h>


struct ttd_reply	prev_reply;         /* saved reply for idempotency */
natural_t		prev_reply_length;
ttd_operation		prev_operation;

static boolean_t	ttd_server_initialized;
boolean_t		ttd_watch=FALSE; /* high level debugging */

#define END_ADDRESS(x) ((ttd_address)((natural_t)&(x) + sizeof(x)))

#define TTD_KMSG(kmsg)	((kmsg == NULL) ? ttd_request_msg : kmsg)

#define target_is_kernel(target) (target == TTD_KERNEL_MID)

#define target_stopped() kernel_target.is_stopped

#define valid_count(count) (count <= TTD_MAX_BLOCK_SIZE)

#define VERBOSE 1

/*
 * Aligned TTD Request.
 */
struct ttd_request aligned_request;

/*
 * Kernel target state:
 */
boolean_t		ttd_single_stepping = FALSE;
ttd_seq			ttd_current_seq;

ttd_breakpoint		ttd_breaktable[TTD_MAXBPT];
ttd_target_info		kernel_target;

ttd_machine_type	ttd_target_machine_type;



/*
 * Used by the ttd_stub code. 
 */
void ttd_stop_target(void)
{
	kernel_target.is_stopped = TRUE;
}

/*
 * Used by the machine dependent code for single stepping.
 * If task is null, then we check for the address specified in ANY task.
 * This lets us call in with the NULL task to talk about the kernel task.
 */
boolean_t break_set(task_t task,
		    ttd_address addr,
		    ttd_saved_inst *inst)
{
	ttd_breakno b;
	
	for (b = 0; b < TTD_MAXBPT; b++) {
		if ((!ttd_breaktable[b].free) &&
		    (ttd_breaktable[b].address == addr)) {
#ifdef notdef			
			thread_t t;
			if (task) {
				t = ttd_thread_to_kthread(ttd_breaktable[b].thread);
				if (t && t->task != task)
					continue;
			}
#endif			
			*inst = ttd_breaktable[b].saved_inst;
			return TRUE; 
		}
	}
	return FALSE;
}

/*
 * Misc. routines for this module:
 */
static boolean_t find_break(ttd_address addr,
		     ttd_thread thread,
		     ttd_breakno *bn)
{
	ttd_breakno b;
	
	for (b = 0; b < TTD_MAXBPT; b++) {
		if ((!ttd_breaktable[b].free) &&
		    (ttd_breaktable[b].address == addr)
#ifdef notdef		    
		    &&
		    (ttd_breaktable[b].thread == thread)
#endif
		    ) {
			*bn = b;
			return TRUE; 
		}
	}
	return FALSE;
}

static boolean_t find_free_breakentry(ttd_breakno *breakno)
{
	ttd_breakno	b;

	for(b = 0; b < TTD_MAXBPT; b++) {
		if (ttd_breaktable[b].free) {
			*breakno = b;
			return TRUE;
		}
	}
	return FALSE;
}


static void init_break_table(void)
{
	ttd_breakno bn;
	
	for (bn = 0; bn < TTD_MAXBPT; bn++)
		ttd_breaktable[bn].free = TRUE;
}

static void init_kernel_target(void)
{
	kernel_target.is_targeted = FALSE;
	kernel_target.is_stopped = FALSE;
	kernel_target.trapped_thread = 0;

	ttd_current_seq = 0x7fffffff;

	init_break_table();

	ttd_single_stepping = FALSE;
}

/*
 * Check to see if this message is a duplicate.  Updates
 * the current sequence number as a side effect.
 */
static boolean_t duplicate(ttd_seq s)
{
	boolean_t dup;
	
	dup = (ttd_current_seq == s);
	if (ttd_debug && (s < ttd_current_seq))
		printf("TTD:duplicate packet.\n");
	ttd_current_seq = s;
				
	return dup;
}

/*
 * Note: since the kernel is the only ttd target, the target is
 * implicit.
 */
static void get_kernel_target_info(ttd_target_info *ti)
{
	ti->is_stopped = kernel_target.is_stopped;
	ti->is_targeted = kernel_target.is_targeted;
	ti->trapped_thread = salhook_current_thread();
	if (ttd_debug)  {
		printf("get_kernel_target_info: %d\n", ti->trapped_thread);
	}
	ti->debug_reason.length = 0;
#if	NOT_NOW
	strcpy("Hi!", ti->debug_reason.chars);
#endif	/* NOT_NOW */
}

/*******************/
/* Action Routines */
/*******************/

/*
 * Read and write to kernel VM.
 * Take care of niggly alignment problems
 */
static ttd_code_t read_write_action(vm_prot_t	access,
				    ttd_address	mem_ptr,
				    ttd_address	buffer,
				    ttd_count	*length)
{
	vm_size_t	size;
	int res;
	void (*rw)() = 0;
	natural_t aligned_buf[ TTD_MAX_BLOCK_SIZE / sizeof(natural_t)];
	natural_t *ab;

	if (buffer & (BYTE_ALIGNMENT-1)) {
		if(ttd_debug) {
			printf("ttd read_write aligning buffer\n");
		}
		ab = aligned_buf;
		if (access & VM_PROT_WRITE) {
			bcopy((char *) buffer, ab, *length);
		}
	} else {
		ab = (natural_t*)buffer;
	}

	if (access & VM_PROT_WRITE)  {
		res = ttd_write_bytes((natural_t *) mem_ptr, 
				      (volatile int*) length, 
				      ab, 
				      TASK_NULL);
	} else {
		res = ttd_read_bytes((natural_t *) mem_ptr, 
				     (volatile int*) length, 
				     ab, 
				     TASK_NULL);
	}
	if (res)  {
		if ((ab == aligned_buf) && (access & VM_PROT_READ)) {
			bcopy(ab, (char *) buffer, *length);
		}
		return Okay;
	}
	else
		return MemoryReferenceFailed;
}	



static void get_state_action(ttd_thread		thread,
			     ttd_thread_info	*thread_info,
			     ttd_trap_info	*trap_info,
			     ttd_machine_state_flavor machine_state_flavor,
			     ttd_machine_state	*machine_state)
{
    char buf[128];
    char *bp = buf;
    boolean_t get_boundary_regs;
    
	thread_info->thread_address = 0;
	thread_info->task_address = 0;
        salhook_thread_name(thread,bp,sizeof(thread_info->thread_state_msg));

        if (ttd_debug) 
	    printf("GSA(1) for %lx %s\n", thread, bp);
	bcopy(bp, thread_info->thread_state_msg,
	       sizeof(thread_info->thread_state_msg));

	thread_info->task_id = 10;

	/*
	 * Trap info
	 */

        if (ttd_debug) 
	    printf("GSA(2) for %s\n", bp);    
        trap_info->user_mode = salhook_is_user_thread(thread);

	/*
	 * Register info
	 */
        if (ttd_debug) 
	    printf("GSA(3) for %s\n", bp);        
        if (machine_state_flavor == THREAD_STATE_USER && !trap_info->user_mode) {
		/* only fetch boundary registers if we want user registers yet
		 * we halted in kernel mode.
		 */
		get_boundary_regs = TRUE;
	} else {
		/* fetch true machine state registers if stopped in user mode,
		 * or stopped in kernel mode and we say "we want the user registers
		 * that are stored on the trap frame.
		 */
		get_boundary_regs = FALSE;
	}
        salhook_get_state(thread, get_boundary_regs, machine_state);
        if (ttd_debug) 
	    printf("GSA(4)\n", bp);            
}

static void set_state_action(ttd_thread		thread,
			     ttd_thread_info	*thread_info,
			     ttd_trap_info	*trap_info,
			     ttd_machine_state	*machine_state)
{
	/*
	 * Don't do the thread_info, or trap_info now either. XXX Fix it?
	 */
	salhook_set_state(thread,machine_state);
}

static void insert_break(ttd_breakno	bn,
			 ttd_address	addr,
			 ttd_thread	thread,
			 ttd_flavor	flavor,
			 ttd_saved_inst	saved_inst)
{
	ttd_breaktable[bn].free	= FALSE;
	ttd_breaktable[bn].address	= addr;
	ttd_breaktable[bn].thread	= thread;	/* NOT USED */
	ttd_breaktable[bn].flavor	= flavor;
	ttd_breaktable[bn].saved_inst	= saved_inst;
}

static ttd_code_t set_break_action(ttd_address	addr,
				   ttd_thread	thread,
				   ttd_flavor	flavor,
				   ttd_saved_inst	*saved_inst)
{
	ttd_breakno	bn;
	boolean_t	found = FALSE;
	thread_t	t;
	task_t		task;
	
	/*
	 * Check to see if a breakpoint is already set there for a
	 * different thread.
	 */
	for(bn = 0; bn < TTD_MAXBPT; bn++) {
		if (!ttd_breaktable[bn].free &&
		    (ttd_breaktable[bn].address == addr)) {
			/* CHECK THREAD */
			*saved_inst = ttd_breaktable[bn].saved_inst;
			found = TRUE;
			break;
		}
	}

	/*
	 * Get a free table entry.
	 */
	if (!find_free_breakentry(&bn))
		return TooManyBreakpoints;
	
	/*
	 * Insert into table if already have saved_inst.
	 */
	if (found) {
		insert_break(bn, addr, thread, flavor, *saved_inst);
		return Okay;
	}

	/*
	 * Insert the breakpoint into physical memory and flush the
	 * cache.
	 */
	task = ttd_thread_to_spintask(thread);
	if (ttd_bkpt_debug) printf("sba:ttd_thread_to_spintask returns 0x%lx\n",
				    task);
	
	
	if (!ttd_insert_breakpoint(task, addr, saved_inst))
		return MemoryReferenceFailed;

	/*
	 * Insert breakpoint into table.
	 */

	insert_break(bn, addr, thread, flavor, *saved_inst);

	if (ttd_bkpt_debug) {
		printf("Inserted breakpoint into table at bn = %d\n",bn);
	}

	return Okay;
}

static ttd_code_t clear_break_action(ttd_address addr, ttd_thread thread)
{
	ttd_breakno	b;
	ttd_breakno	bn = 0;
	ttd_count	count = 0;
	ttd_saved_inst	saved_inst;
	boolean_t	found = FALSE;	
	thread_t	t;
	task_t		task;

	if (ttd_bkpt_debug)
		printf("cba:called 0x%lx\n", addr);

	/*
	 * Cycle through breaktable.  If more than one breakpoint
	 * at addr, we know not to replace memory with saved_inst.
	 */
	for(b = 0; b < TTD_MAXBPT; b++) {
		if (!ttd_breaktable[b].free &&
		    (ttd_breaktable[b].address == addr)) {
#ifdef notdef			
			if (ttd_breaktable[b].thread == thread)
#endif				
			{
				found = TRUE;
				bn = b;
			}
			if (++count > 1) {
				break;
			}
		}
	}


	/*
	 * Didn't find breakpoint in table, return
	 */
	if (!found) {
		if (ttd_bkpt_debug) {
			printf("Couldn't find breakpoint in table.\n");
		}
		return InvalidArgument;
	}

	/*
	 * Only one breakpoint at that address, fault in memory
	 * and clear the breakpoint.
	 */
	if (count == 1) {
		if (ttd_bkpt_debug)
			printf("cba:calling ttd_thread_to_spintask\n");	       
		task = ttd_thread_to_spintask(thread);  /* was t, why fail?*/
		if (ttd_bkpt_debug)
			printf("cba:ttd_thread_to_spintask returns 0x%lx\n",
				    task);		
		saved_inst = ttd_breaktable[bn].saved_inst;

		if (ttd_bkpt_debug)
			printf("cba:calling ttd_remove_breakpoint\n");
		
		if (!ttd_remove_breakpoint(task,
					    addr,
					    saved_inst))
			return MemoryReferenceFailed;
	}

	if (ttd_bkpt_debug) {
		printf("Cleared breakpoint from table at bn = %d\n",bn);
	}
	
	/*
	 * Free entry in breaktable.
	 */
	ttd_breaktable[bn].free = TRUE;

	return Okay;
}

static ttd_code_t get_next_break_action(boolean_t	all_breaks,
					ttd_address	*addr,
					ttd_thread	*thread,
					ttd_flavor	*flavor,
					ttd_saved_inst	*saved_inst)
{
	ttd_breakno bn;
	ttd_breakno start;

	/*
	 * If addr is zero, then start from beginning of list.
	 * Otherwise, start from the breakno after addr's breakno.
	 */
	if (*addr == 0) {
		start = 0;
	}else{
		/*
		 * Find the first matching breakpoint entry.
		 */
		for(bn = 0; bn < TTD_MAXBPT; bn++) {
			if (!ttd_breaktable[bn].free &&
			    (ttd_breaktable[bn].address == *addr))
#ifdef notdef
			    &&
			    (ttd_breaktable[bn].thread == *thread)
#endif				
				break;
		}

		/*
		 * If ran off the end, it's an invalid argument.
		 * Even all_breaks will have a valid addr and thread.
		 */
		if (!all_breaks && (bn > TTD_MAXBPT))
			return InvalidArgument;

		if (bn < TTD_MAXBPT - 1) {
			start = bn + 1;
		}else{
			/*
			 * Ran off the end.  No more breakpoints
			 * in the table.  Return zeros in vars.
			 */
			*addr = 0;
			*flavor = 0;
			bzero(saved_inst, sizeof(ttd_saved_inst));
			return Okay;
		}
	}

	for(bn = start; bn < TTD_MAXBPT; bn ++) {
		if (!ttd_breaktable[bn].free &&
		    (all_breaks ||
		     (ttd_breaktable[bn].thread == *thread))) {
			*addr		= ttd_breaktable[bn].address;
			*thread		= ttd_breaktable[bn].thread;
			*flavor		= ttd_breaktable[bn].flavor;
			*saved_inst	= ttd_breaktable[bn].saved_inst;
			return Okay;
		}
	}
	*addr = 0;
	*flavor = 0;
	bzero(saved_inst, sizeof(ttd_saved_inst));

	return Okay;
}

/*
 * The Operations:
 */

/*
 * probe_server:
 *
 *  Probe server returns the version number of the TTD implementation
 * and the machine type that it is executing on.
 *
 */
static void probe_server(ttd_request_t	request,
			 ttd_reply_t	reply,
			 ttd_address	*reply_end)
{
	if (ttd_debug) {
		printf("TTD:Probe Server, version = %d, machine_type = %d\n",
		       TTD_VERSION, ttd_target_machine_type);
	}

	reply->u.probe_server.version = htonl(TTD_VERSION);
	reply->u.probe_server.machine_type = htonl(ttd_target_machine_type);
	*reply_end = END_ADDRESS (reply->u.probe_server);
}

/*
 * get_target_info:
 *
 *  Get target info returns the target_info struct for the given target.
 * Since this is the ttd implementation, the only valid target is the
 * kernel target (ie. it only returns the target info for the kernel.
 *
 */
static void get_target_info(ttd_request_t	request,
			    ttd_reply_t	 	reply,
			    ttd_address		*reply_end)
{
	ttd_target	target;
	ttd_target_info	target_info;

	target = request->u.get_target_info.target;
	if (!target_is_kernel(target)) {
		reply->result.code = InvalidTarget;

		if (ttd_debug) {
			printf("TTD:get_target_info, invalid target %d\n", target);
		}

		return;
	}

	get_kernel_target_info(&target_info);

	reply->u.get_target_info.target_info = target_info;
	*reply_end = END_ADDRESS (reply->u.get_target_info);
	
	if (ttd_debug) {
		printf("TTD:get_target_info, target %d is %s, %s, th = 0x%lx\n",
		       target,
		       target_info.is_stopped ? "stopped" : "running",
		       target_info.is_targeted ? "targeted" : "untargeted",
		       target_info.trapped_thread);
	}
}

/*
 * connect_to_target:
 *
 *  Connect to target connects to the target.  The only valid target
 * in the ttd implemetation is the kernel, all other targets are
 * invalid.  If the kernel is already targeted, it can only succeed
 * if the key value that is passed in the request message is valid.
 *
 * Note:  In this early implementation of ttd there is no security
 *        built into the ttd protocol (ie. a value of MASTER_KEY
 *        allows anyone to override the current connection).
 *
 */
static void connect_to_target(ttd_request_t	request,
			      ttd_reply_t	reply,
			      ttd_address	*reply_end)
{
	ttd_target	target;
	ttd_key		key;
	ttd_target_info	target_info;

	target = request->u.connect_to_target.target;
	key = request->u.connect_to_target.key;

	if (ttd_debug)  {
		printf("connect_to_target: 0x%x 0x%x\n", target, key);
	}
       

	if (!target_is_kernel(target)) {
		reply->result.code = InvalidTarget;
		if (ttd_debug)
			printf("TTD:connect_to_target, invalid target %d\n", target);
		return;
	} else if (kernel_target.is_targeted && (key != MASTER_KEY)) {
		reply->result.code = TargetNotAvailable;
		if (ttd_debug)
			printf("TTD:connect_to_target, target %d unavailable (key=%d master =%d)\n", target, key, MASTER_KEY);
		return;
	}

	kernel_target.is_targeted = TRUE;
	ttd_current_seq = 0;

	get_kernel_target_info(&target_info);

	reply->u.connect_to_target.target = TTD_KERNEL_MID;
	reply->u.connect_to_target.target_info = target_info;
	reply->result.argno = 2;
	*reply_end = END_ADDRESS (reply->u.connect_to_target);

	if (ttd_debug)
		printf("TTD:connect_to_target, connected ttd to target %d\n",target);
}

/*
 * disconnect_from_target:
 *
 *  Disconnect from target disconnects from the specified target.
 * The kernel target is the only valid target in the ttd implemen-
 * tation.
 *
 * Note:  This routine does not implicitly restart the target.
 *
 */
static void disconnect_from_target(ttd_request_t request,
				   ttd_reply_t 	 reply,
				   ttd_address	 *reply_end)
{
	
	/*
	 * The target checking is done in the ttd_service_request
	 * routine.  We know the target is the kernel if we get this
	 * far.
	 */
	kernel_target.is_targeted = FALSE;

	if (ttd_debug)
		printf("TTD:disconnect_from_target, disconnected from ttd target\n");
	if (request->u.disconnect_from_target.mode == DISCONNECT_HALT)  {
		printf("Halting cpu\n");
		standalone_halt();
	}
	reply->result.code = Okay;
}


/*
 * read_from_target:
 *
 *  Read from target reads count number of bytes from the target's
 * address space (the kernel in this implementation) and places them
 * in the reply message's buffer.
 *
 */
static void read_from_target(ttd_request_t	request,
			     ttd_reply_t	reply,
			     ttd_address	*reply_end)
{
	ttd_address	start;
	ttd_count		count;
	ttd_address	buffer;

	/*
	 * Target must be stopped in order to do this operation.
	 */
	if (!target_stopped()) {
		reply->result.code = TargetNotStopped;
		if (ttd_debug)
			printf("TTD:read_from_target, target not stopped.\n");
		return;
	}

	start = (ttd_address) request->u.read_from_target.start;
	count = request->u.read_from_target.count;
	buffer = (ttd_address) &reply->u.read_from_target.data[0];

	if (!valid_count(count)) {
		reply->result.code = InvalidArgument;
		if (ttd_debug)
			printf("TTD:read_from_target, invalid count 0x%x!!\n",
			       count);
		return;
	}

#if	VERBOSE
	if (ttd_debug) {
	    printf("ttd_debug addr = %x, value = %d\n", &ttd_debug, ttd_debug);
	    printf("TTD:read_from_target, start= 0x%x count=0x%x,",
		   start, count);
	}
#endif	/* VERBOSE */

	reply->result.code = read_write_action(VM_PROT_READ, start, buffer,
					       &count);

	if (reply->result.code == Okay ||
	    (reply->result.code == MemoryReferenceFailed )) {
		reply->u.read_from_target.count = count;
		*reply_end = (ttd_address)
			&reply->u.read_from_target.data[count];
		reply->result.argno = 2;
	}
}

/*
 * write_info_target:
 *
 *  Write into target writes count bytes into the target's address
 * space (the kernel's address space) at the address addr from the
 * request message's buffer.
 *
 */
static void write_into_target(ttd_request_t	request,
			      ttd_reply_t	reply,
			      ttd_address	*reply_end)
{
	ttd_address	start;
	ttd_count	count;
	ttd_address	buffer;

	/*
	 * Target must be stopped in order to do this operation.
	 */
	if (!target_stopped()) {
		reply->result.code = TargetNotStopped;
		if (ttd_debug)
			printf("TTD:write_into_target, target not stopped\n");
		return;
	}

	start = (ttd_address) request->u.write_into_target.start;
	count = request->u.write_into_target.count;
	buffer = (ttd_address) &request->u.write_into_target.data[0];

	if (!valid_count(count)) {
		reply->result.code = InvalidArgument;
		if (ttd_debug)
			printf("TTD:write_into_target, invalid count 0x%x\n",
			       count);
		return;
	}

	reply->result.code = read_write_action(VM_PROT_WRITE, start, buffer,
					       &count);

	if (ttd_debug)
		printf("TTD:write_into_target, start=0x%x count=0x%x, result = %s\n",
		       start, count, (reply->result.code == Okay) ? "OK" : "ERR");
}

/*
 * get_next_domain:
 *
 *  Get next domain returns the next domain in the SPIN system.
 *  Return a name length of zero after the last domain.
 *
 */
static void get_next_domain(ttd_request_t	request,
			    ttd_reply_t		reply,
			    ttd_address		*reply_end)
{
	ttd_domain_info	info;
	static int domain_cnt=0;

	/*
	 * Target must be stopped in order to do this operation.
	 */
	if (!target_stopped()) {
		reply->result.code = TargetNotStopped;
		if (ttd_debug)
			printf("TTD:get_next_domain, target not stopped\n");
		return;
	}

	/* These values tell gdb it just got the last domain */
	info.name.length = 0;
	info.loadaddr = 0x0;

	if (salhook_next_domain( domain_cnt, info.name.chars,
		sizeof(info.name.chars), &info.loadaddr)){
		domain_cnt++;
		info.name.length = strlen(info.name.chars)+1;
		}
	else {
		/* reset to first domain for next sweep */
		domain_cnt=0;
		}


	reply->u.get_domain_info.domain = info;
	reply->result.argno = 1;
	*reply_end = END_ADDRESS (reply->u.get_domain_info);

	if (ttd_debug)
		printf("TTD:get_next_domain, name %d:%s loadaddr= 0x%lx\n",
		       reply->u.get_domain_info.domain.name.length,
		       reply->u.get_domain_info.domain.name.chars,
		       reply->u.get_domain_info.domain.loadaddr);
}

/*
 * get_next_thread:
 *
 *  Get next thread returns the next thread in the taks's (target's)
 * thread list starting at the thread in the request message.  If the
 * request thread's value is TTD_NO_THREAD, the first thread in the task's
 * thread list is returned, otherwise we return the next thread.
 *
 */
static void get_next_thread(ttd_request_t	request,
			    ttd_reply_t		reply,
			    ttd_address		*reply_end)
{
	ttd_thread	thread;

	/*
	 * Target must be stopped in order to do this operation.
	 */
	if (!target_stopped()) {
		reply->result.code = TargetNotStopped;
		if (ttd_debug)
			printf("TTD:get_next_thread, target not stopped\n");
		return;
	}

	thread = request->u.get_next_thread.thread;

	salhook_next_thread(&thread);

	reply->u.get_next_thread.next = thread;
	reply->result.argno = 1;
	*reply_end = END_ADDRESS (reply->u.get_next_thread);

	if (ttd_debug)
		printf("TTD:get_next_thread, orig= 0x%x, thread=0x%x\n",
		       request->u.get_next_thread.thread, thread);
}

/*
 * get_thread_info:
 *
 *  Get thread info returns information about the thread specified
 * in the request message.  There are three parts to the thread info:
 * 
 *	thread_info:	the thread's state (ie. what's returned by
 *			thread_getstatus).
 *
 *	trap_info:	information on which trap caused the thread
 *			to trap (if it is stopped).
 *
 *	machine_state:	the thread's register state.
 *
 * Note:  In this implementation, we only support the machine state.
 *
 */
static void get_thread_info(ttd_request_t	request,
			    ttd_reply_t		reply,
			    ttd_address		*reply_end)
{
	ttd_thread	thread;
	ttd_thread_info	thread_info;
	ttd_trap_info	trap_info;
	ttd_machine_state machine_state;
	ttd_machine_state_flavor flavor;
	/*
	 * Target must be stopped in order to do this operation.
	 */
	if (!target_stopped()) {
		reply->result.code = TargetNotStopped;
		if (ttd_debug)	{
			printf("TTD:get_thread_info, target not stopped.\n");
		}
		return;
	}

	thread = request->u.get_thread_info.thread;
	flavor = request->u.get_thread_info.flavor;
	
	if (!ttd_thread_valid_thread_id(thread)) {
		reply->result.code = InvalidArgument;
		if (ttd_debug)  {
			printf("TTD:get_thread_info, invalid thread = %d.\n",
			       thread);
		}
		return;
	}

	if (ttd_debug) printf("calling get_state_action\n");
	
	get_state_action(thread, &thread_info, &trap_info, flavor, &machine_state);

	if (ttd_debug)
		printf("TTD:get_thread_info, thread= %d, ...\n", thread);
		
	reply->u.get_thread_info.thread_info = thread_info;
	reply->u.get_thread_info.trap_info = trap_info;
	reply->u.get_thread_info.machine_state = machine_state;
	reply->result.argno = 3;
	*reply_end = END_ADDRESS (reply->u.get_thread_info);

}

/*
 * set_thread_info:
 *
 *  Set thread info sets the specified thread's three states to
 * parameters in the request message.  These thread states are the
 * ones outlined above in the get_thread_info call.
 *
 * Note:  Only the thread's machine_state is set in this implemen-
 *        tation.
 *
 */
static void set_thread_info(ttd_request_t	request,
			    ttd_reply_t		reply,
			    ttd_address		*reply_end)
{
	ttd_thread	thread;
	ttd_thread_info	*thread_info;
	ttd_trap_info	*trap_info;
	ttd_machine_state *machine_state;

	/*
	 * Target must be stopped in order to do this operation.
	 */
	if (!target_stopped()) {
		reply->result.code = TargetNotStopped;
		if (ttd_debug)
			printf("TTD:set_thread_info, target not stopped.\n");
		return;
	}

	thread = request->u.set_thread_info.thread;
#if	FUTURE
	thread_info = &(request->u.set_thread_info.thread_info);
	trap_info = &(request->u.set_thread_info.trap_info);
#endif	/* FUTURE */
	
	machine_state = &(request->u.set_thread_info.machine_state);

	if (!ttd_thread_valid_thread_id(thread)) {
		reply->result.code = InvalidArgument;
		if (ttd_debug)
			printf("TTD:set_thread_info, invalid thread %d.\n",
			       thread);
		return;
	}

	set_state_action(thread,
		         thread_info,
		         trap_info,
		         machine_state);

	if (ttd_debug)  {
		printf("TTD:set_thread_info, thread= %d, 0x%lx, ...\n",
		       thread, ttd_thread_to_spintask(thread));
	}
}

/*
 * stop_target:
 *
 *  Stop target stops the target specified in the request message.
 * In order to issue this command, the client must have successfull
 * issued an attach_to_target request previous to this request.
 *
 * Note:  this command was not directly supported by the original
 *        NubTTD implementation.  It is supported by ttd since
 *        the ttd client can communicate with the ttd server
 *        asynchronously.
 *
 * Note:  This implementation stops all threads in a task.  Future
 *        versions will work on a per-thread basis.
 *
 */
static void stop_target(ttd_request_t	request,
			ttd_reply_t	reply,
			ttd_address	*reply_end)
{
	/*
	 * Return error message if already stopped.
	 */
	if (target_stopped()) {
		reply->result.code = TargetStopped;
		if (ttd_debug)
			printf("TTD:stop_target, target ALREADY stopped.\n");
		return;
	}

	/*
	 * All we need to do to stop the kernel is call
	 * ttd_break.  This will cause this "thread" to
	 * enter the ttd_handle_sync() routine which will:
	 *
	 * 1.  Halt all the processors.
	 * 
	 * 2.  Stop the kernel (kernel_target.is_stopped = TRUE).
	 *
	 */

	if (ttd_debug)
		printf("TTD:stop_target, stopping kernel.\n");

	ttd_halt_processors();

	kernel_target.is_stopped = TRUE;

	ttd_run_status = FULL_STOP;
}

/*
 * probe_target:
 *
 *  Probe target returns the target info of the current target.  This
 * is used extensively by the asynhronous client.  In general the client
 * will poll the ttd target until it is stopped, and only then issue
 * requests.
 *
 */
static void probe_target(ttd_request_t	request,
			 ttd_reply_t	reply,
			 ttd_address	*reply_end)
{
	ttd_target_info	target_info;

	get_kernel_target_info(&target_info);
	reply->u.probe_target.target_info = target_info;
	reply->result.argno = 1;
	*reply_end = END_ADDRESS (reply->u.probe_target);

	if (ttd_debug)
		printf("TTD:probe_target, Kernel target is %s, %s, th = 0x%x\n",
		       target_info.is_stopped ? "stopped" : "running",
		       target_info.is_targeted ? "targeted" : "untargeted",
		       target_info.trapped_thread);
}

/*
 * restart_target:
 *
 *  Restart target resume's the ttd target's (the kernel's) execution.
 *
 * Note:  The current implementation restarts all a tasks threads at once.
 *        Future versions will work on a per thread basis.
 *
 */
static ttd_response_t restart_target(ttd_request_t	request,
				     ttd_reply_t	reply,
				     ttd_address	*reply_end)
{
	ttd_thread	thread;

	/*
	 * If target already running return error code.
	 */
	if (!target_stopped()) {
		reply->result.code = TargetNotStopped;
		if (ttd_debug)
			printf("TTD:restart_target, target not stopped.\n");
		return SEND_REPLY;
	}

	if (1 && ttd_debug)
		printf("TTD:restart_target, restarting target....\n");

	/*
	 * We don't need to save a message for duplicate replies since
	 * restart doesn't send a reply.  We have a check when we check
	 * for duplicates that determines if it's a duplicate restart packet.
	 * If it is, then we just ignore it.
	 *
	 * The same holds for duplicate single_step packets.
	 */

	/* This will make it restart. */
	ttd_run_status = ONE_STOP;

	kernel_target.is_stopped = FALSE;

	/*
	 * Second way didn't send reply now, but third way will.
	 * We'll cache the reply and send it whenever we receive
	 * a duplicate!  Duhhhhhh....
	 */
	return SEND_REPLY;
}

/*
 * set_breakpoint_in_target
 *
 *  Set breakpoint in target sets a breakpoint for a specified thread in
 * in the current task's (the kernel task) at a specified address with
 * a specified flavor.  If the thread is NULL, it applies to all threads
 * in the target's task (all kernel threads).
 *
 */
static void set_breakpoint_in_target(ttd_request_t	request,
				     ttd_reply_t	reply,
				     ttd_address	*reply_end)
{
	ttd_address	addr;
	ttd_thread	thread;
	ttd_flavor	flavor;
	ttd_breakno	bn;
	ttd_saved_inst	saved_inst;

	/*
	 * Target must be stopped in order to do this operation.
	 */
	if (!target_stopped()) {
		reply->result.code = TargetNotStopped;
		if (ttd_debug)
			printf("TTD:set_breakpoint, target not stopped.\n");
		return;
	}

	addr = (ttd_address) request->u.set_breakpoint_in_target.address;
	thread = (ttd_thread) request->u.set_breakpoint_in_target.thread;
	flavor = request->u.set_breakpoint_in_target.flavor;

	if (thread && !ttd_thread_valid_thread_id(thread)) {
		reply->result.code = InvalidArgument;
		if (ttd_debug) {
			printf("TTD:set_breakpoint, Invalid thread.\n");
		}
		return;
	}
		
	if (ttd_bkpt_debug) {
		printf("TTD:set_breakpoint, addr= 0x%x, thread= 0x%x, flavor= %d, ",
		       addr, thread, flavor);
	}

	if (find_break(addr, thread, &bn)) {
		/* Breakpoint already set */
		ttd_breaktable[bn].flavor = flavor;
		saved_inst = ttd_breaktable[bn].saved_inst;
	}else{
		reply->result.code = set_break_action(addr, thread,
						      flavor, &saved_inst);
	}

	if (!reply->result.code == Okay) {
		if (ttd_debug)
			printf("ERR\n");
		return;
	}

	if (ttd_bkpt_debug)
		printf("OK\n");

	reply->u.set_breakpoint_in_target.saved_inst = saved_inst;
	reply->result.argno = 1;
	*reply_end = END_ADDRESS(reply->u.set_breakpoint_in_target);
}

/*
 * clear_breakpoint_in_target:
 *
 *  Clear breakpoint in target removes the breakpoint from target's
 * breakpoint table.  Like the set breakpoint request above, it takes
 * an address and a thread.  If the thread is NULL, it only clears
 * the breakpoints that apply to all threads.
 *
 */
static void clear_breakpoint_in_target(ttd_request_t	request,
				       ttd_reply_t	reply,
				       ttd_address	*reply_end)
{
	ttd_address	addr;
	ttd_thread	thread;

	/*
	 * Target must be stopped in order to do this operation.
	 */
	if (!target_stopped()) {
		reply->result.code = TargetNotStopped;
		if (ttd_debug)
		       printf("TTD:clear_break, target not stopped.\n");
		return;
	}

	addr = (ttd_address)request->u.clear_breakpoint_in_target.address;
	thread = (ttd_thread)request->u.clear_breakpoint_in_target.thread;

	if (ttd_bkpt_debug)
		printf("TTD:clear_breakpoint, addr= 0x%lx, thread= 0x%x\n",
		       addr, thread);

	/*
	 * Doesn't have to be a legal thread.  Might want to clear a
	 * breakpoint for a thread that has been destroyed.  The
	 * clear break action procedure will ignore threads that don't
	 * have breakpoints in the table.
	 */
	reply->result.code = clear_break_action(addr, thread);
}

/*
 * get_next_breakpoint_in_target:
 *
 *  Get next breakpoint in target returns the next breakpoint in the
 * target's (the kernel's) breakpoint list.  This request returns the
 * next breakpoint with respect to the breakpoint passed in the request.
 * If the request breakpoint is NULL, it starts at the beginning of the
 * breakpoint list.  If all_breaks is TRUE it returns the next breakpoint
 * without regard to the breakpoint's associated thread.  If all_breaks
 * is FALSE it only returns the next breakpoint that is associated with
 * the thread specified in the request message.
 *
 * Note:  This request can be issued on a running target.
 *
 */
static void get_next_breakpoint_in_target(ttd_request_t	request,
					  ttd_reply_t	reply,
					  ttd_address	*reply_end)
{
	ttd_address	addr;
	ttd_thread	thread;
	ttd_flavor	flavor;
	ttd_saved_inst	saved_inst;
	boolean_t	all_breaks;
		
	addr = (ttd_address)request->u.get_next_breakpoint_in_target.address;
	thread = (ttd_thread)request->u.get_next_breakpoint_in_target.thread;
	all_breaks = request->u.get_next_breakpoint_in_target.all_breaks;

	if (ttd_bkpt_debug)
		printf("TTD:get_break, address= 0x%x, thread= 0x%x, allbreaks= 0x%x\n",
		       addr, thread, all_breaks);

	reply->result.code = get_next_break_action(all_breaks, &addr, &thread,
						   &flavor, &saved_inst);

	if (reply->result.code != Okay)
		return;

	reply->u.get_next_breakpoint_in_target.address = (ttd_address) addr;
	reply->u.get_next_breakpoint_in_target.flavor = flavor;
	reply->u.get_next_breakpoint_in_target.saved_inst = saved_inst;
	*reply_end = END_ADDRESS (reply->u.get_next_breakpoint_in_target);
}

/*
 * ttd_single_step:
 *
 *  TTD Single Step sets the machine independent single stepping
 * values to single stepping state.  It then calls the machine 
 * dependent code to turn the machine into single stepping mode.
 *
 */
boolean_t ttd_single_step()
{
	if (ttd_single_stepping) {
		printf("ttd_single_step:  Already Single stepping!!!\n");
		return FALSE;
	}

	ttd_single_stepping = TRUE;

	/*
	 * In the current implementation, we can only set and
	 * clear single step in the kernel task.
	 */
	return(ttd_set_machine_single_step(NULL));
}

/*
 * ttd_clear_single_step:
 *
 *  TTD Clear Single Step clear the machine independent single stepping
 * mechanism, and takes the machine out of single stepping mode.
 *
 */
boolean_t ttd_clear_single_step()
{
	if (!ttd_single_stepping) {
		printf("ttd_clear_single_step: Already out of single stepping!!\n");
	}

	ttd_single_stepping = FALSE;

	/*
	 * In the current implementation, we can only set and
	 * clear single step in the kernel task.
	 */
	return (ttd_clear_machine_single_step(NULL));
}

boolean_t ttd_in_single_step(void)
{
	return (ttd_single_stepping);
}

/*
 * single_step_thread:
 *
 *  Single step thread starts a thread specified in the request message
 * executing in single step mode.
 *
 * Note:  In this implementation you can only single step the current
 *        thread.
 *
 */
static ttd_response_t single_step_thread(ttd_request_t	request,
					 ttd_reply_t	reply,
					 ttd_address	*reply_end)
{
	ttd_thread	thread;

	/*
	 * Target must be stopped in order to do this operation.
	 */
	if (!target_stopped()) {
		reply->result.code = TargetNotStopped;
		if (ttd_debug)
			printf("TTD:single_step, target not stopped.\n");
		return SEND_REPLY;
	}

#if	DO_LATER
	thread = request->u.single_step_thread.thread;

	if (!ttd_thread_valid_thread_id(thread)) {
		reply->result.code = InvalidArgument;
		return SEND_REPLY;
	}
#endif	/* DO_LATER */

	/*
	 * Note:  in this implementation a thread is assumed to
	 *        be stopped if it's target (task) is not running,
	 *        make sure the check that the thread is stopped
	 *        when the implementation is changed to allow 
	 *        thread stoppages.
	 */

	/*
	 * What should happen here:
	 *
	 * 1. swap in the specified thread.
	 *
	 */

	if (!ttd_single_step()) {
		reply->result.code = SingleSteppingError;
		if (ttd_debug)
			printf("TTD:single_step, single stepping ERROR!\n");
		return SEND_REPLY;
	}

	if (ttd_debug)
		printf("TTD:single_step, single step.\n");


	/* This will restart the kernel */
	ttd_run_status = ONE_STOP;

	kernel_target.is_stopped = FALSE;

	/* See comments in restart_target. */
	return SEND_REPLY;
}

char *opnames[] = {"invalid",
	"PROBE_SERVER",
	"invalid",
	"invalid",
	"GET_TARGET_INFO",
	"CONNECT_TO_TARGET",
	"DISCONNECT_FROM_TARGET",
	"READ_FROM_TARGET",
	"WRITE_INTO_TARGET",
	"GET_NEXT_THREAD",
	"GET_THREAD_INFO",
	"SET_THREAD_INFO",
	"STOP_TARGET",
	"PROBE_TARGET",
	"RESTART_TARGET",
	"SET_BREAKPOINT_IN_TARGET",
	"CLEAR_BREAKPOINT_IN_TARGET",
	"GET_NEXT_BREAKPOINT_IN_TARGET",
	"SINGLE_STEP_THREAD",
	"GET_NEXT_DOMAIN",
	};

/*
 * ttd_decode_request:
 *
 *  Returns TRUE if a reply should be sent, FALSE if no reply
 * should be sent.
 *
 */
static ttd_response_t ttd_decode_request(ttd_request_t	request,
					 ttd_reply_t	reply,
					 natural_t *	reply_length)
{
	ttd_address	reply_end;
	boolean_t	targeted_op;
	ttd_response_t	sr;

	if (ttd_debug)  {
		printf("ttd_decode_request: 0x%lx 0x%lx\n",
		       request, reply);
	}
	/*
	 * Convert from network to host byte ordering.
	 */
	request->server = htonl(request->server);
	request->seq = htonl(request->seq);
	request->target = htonl(request->target);
	request->operation = htonl(request->operation);

	/* XXXXX */
	targeted_op = request->operation >= DISCONNECT_FROM_TARGET;

#if 0
printf("ttd_decode_request: seq = 0x%x target = 0x%x operation = 0x%x\n",
       request->seq, request->target, request->operation);
#endif
	if (ttd_watch) {
		printf("ttd request: seq %04x op %2x ",
		       request->seq & 0xffff,
		       request->target,
		       request->operation);
		printf( opnames[request->operation]);
		printf("\n");
		}

	if (ttd_debug)  {
		printf("ttd_decode_request: req->server = 0x%x seq = 0x%x target = 0x%x operation = 0x%x\n",
		       request->server,
		       request->seq,
		       request->target,
		       request->operation);
	}

	/*
	 * Setup the reply's return values that are common
	 * to all operations:
	 */
	reply->server = htonl(KERNEL_TTD);
	reply->target = htonl(TTD_KERNEL_MID);
	reply->result.code = Okay;
	reply->result.argno = 0;
	reply->operation = htonl(request->operation);

	reply_end = END_ADDRESS(reply->operation);

	/*
	 * Set this now for those requests that return before the end.	
	 */
	*reply_length = (vm_address_t)reply_end - (vm_address_t)reply;
	
	/*
	 * For untargeted operations, set the sequence number of the
	 * reply to the seq # of the last valid targeted operation.
	 * This is used by the client end to determine restart and single
	 * stepping reception.
	 */
	reply->seq = htonl(ttd_current_seq);

	if (request->server != KERNEL_TTD) {
		reply->result.code = ServerNotAvailable;

		if (ttd_debug)
			printf("TTD Server not available.\n");
		
		return SEND_REPLY;
	}

	/*
	 * Make sure that there aren't any idempotent operations
	 * that aren't targeted operations, since the duplicate
	 * sequence checking takes place in this if-clause!
	 */
	if (targeted_op) {
		if (!target_is_kernel(request->target)) {
			reply->result.code = InvalidTarget;

			if (ttd_debug)
				printf("Invalid TTD target.\n");

			return SEND_REPLY;
		}

		if (!kernel_target.is_targeted) {
			reply->result.code = TargetNotAvailable;
			
			if (ttd_debug)
				printf("Kernel not targeted.\n");

			return SEND_REPLY;
		}

		if (duplicate(request->seq)) {
#if	SECOND_ATTEMPT
			/*
			 * The problem is, what happens when we get a duplicate
			 * restart or single step message?  There wasn't a
			 * reply for it, so we can't resend it.  So we just 
			 * act like we're ignoring it (like we did the first
			 * message), and "know" that we've already restarted
			 * or single stepped the kernel.
			 */
			if ((prev_operation == RESTART_TARGET) ||
			    (prev_operation == SINGLE_STEP_THREAD))
				return NO_REPLY;
#endif	/* SECOND_ATTEMPT */

			bcopy(&prev_reply, reply, prev_reply_length);

			if (ttd_debug)
#if	TTD_VERBOSE
				printf("Duplicate, resending last reply.\n");
#else
			        printf("<D>");
#endif	/* TTD_VERBOSE */
			
			return SEND_REPLY;
		}
	}
	
	switch(request->operation) {
	    case PROBE_SERVER:
		probe_server(request, reply, &reply_end);
		sr = SEND_REPLY;
		break;

	    case GET_TARGET_INFO:
		get_target_info(request, reply, &reply_end);
		sr = SEND_REPLY;
		break;

	    case CONNECT_TO_TARGET:
		connect_to_target(request, reply, &reply_end);
		sr = SEND_REPLY;
		break;

	    case DISCONNECT_FROM_TARGET:
		disconnect_from_target(request, reply, &reply_end);
		sr = SEND_REPLY;
		break;

	    case READ_FROM_TARGET:
		read_from_target(request, reply, &reply_end);
		sr = SEND_REPLY;
		break;

	    case WRITE_INTO_TARGET:
		write_into_target(request, reply, &reply_end);
		sr = SEND_REPLY;
		break;

	    case GET_NEXT_DOMAIN:
		get_next_domain(request, reply, &reply_end);
		sr = SEND_REPLY;
		break;

	    case GET_NEXT_THREAD:
		get_next_thread(request, reply, &reply_end);
		sr = SEND_REPLY;
		break;

	    case GET_THREAD_INFO:
		get_thread_info(request, reply, &reply_end);
		sr = SEND_REPLY;
		break;

	    case SET_THREAD_INFO:
		set_thread_info(request, reply, &reply_end);
		sr = SEND_REPLY;
		break;

	    case STOP_TARGET:
		stop_target(request, reply, &reply_end);
		sr = SEND_REPLY;
		break;

	    case PROBE_TARGET:
		probe_target(request, reply, &reply_end);
		sr = SEND_REPLY;
		break;

	    case RESTART_TARGET:
		sr = restart_target(request, reply, &reply_end);
		break;

	    case SET_BREAKPOINT_IN_TARGET:
		set_breakpoint_in_target(request, reply, &reply_end);
		sr = SEND_REPLY;
		break;

	    case CLEAR_BREAKPOINT_IN_TARGET:
		clear_breakpoint_in_target(request, reply, &reply_end);
		sr = SEND_REPLY;
		break;

	    case GET_NEXT_BREAKPOINT_IN_TARGET:
		get_next_breakpoint_in_target(request, reply, &reply_end);
		sr = SEND_REPLY;
		break;

	    case SINGLE_STEP_THREAD:
		sr = single_step_thread(request, reply, &reply_end);
		break;

	    default:
		sr = SEND_REPLY;
		reply->result.code = InvalidOperation;
		
		/*
		 * Don't return here, we might've already set up our
		 * seq # as the duplicate seq #, and we therefore need
		 * to make sure that the prev_reply_length field is set
		 * properly.
		 */
	}

	/*
	 * See above comment dealing with the reply->seq.
	 */
	if (targeted_op)
		reply->seq = htonl(request->seq);

	*reply_length = (natural_t)reply_end - (natural_t)reply;

	prev_operation = request->operation;

	return (sr);
}

void
ttd_service_request(salnet_route *route, struct mbuf *reqbuf)
{
	natural_t	ttd_reply_length;
	ttd_request_t	request;
	ttd_reply_t	reply;
	struct mbuf	*m = reqbuf;
	int		len=m_length(m);


	if (len != sizeof(struct ttd_request)) {
		if (ttd_debug)
			printf("INVALID TTD Request Size!! 0x%x != 0x%x\n",
			       len, sizeof(struct ttd_request));
		return ;
		}

	m_copydata(m, 0, len, &aligned_request);
	request = &aligned_request;


	MGET(m,M_DONTWAIT, MT_DATA);
	if (!m) {
		printf("ttd_service_request: no mbuf\n");
		return;
		}
	MCLGET(m,M_DONTWAIT);
	if ((m->m_flags & M_EXT) == 0)
		printf("ttd_service_request: no 2k mbufs left for reply.  Proceeding anyway\n");

	reply = mtod(m, ttd_reply_t);

	if (ttd_decode_request(request, reply, &ttd_reply_length)==SEND_REPLY){
		bcopy(reply, &prev_reply, ttd_reply_length);
		prev_reply_length = ttd_reply_length;

		if (ttd_debug)
			printf("ttd_service_request: reply->server = 0x%x seq = 0x%x target = 0x%x code %x argno %x operation = 0x%x\n",
			       reply->server,
			       reply->target,
			       reply->seq,
				reply->result.code,
				reply->result.argno,
			       reply->operation);
		if (ttd_watch)
			printf("ttd reply(%d):   seq %04x op %2x   code %x argno %x\n",
			       ttd_reply_length,
			       ntohl( reply->seq ) & 0xffff,
			       ntohl( reply->operation ),
				ntohs( reply->result.code ),
				ntohs( reply->result.argno ));


		m->m_len = ttd_reply_length;
		salnet_udpsend(route, m);
		}
	else {
		printf("ttd_service_request().  request is not SEND_REPLY\n");
		printf("   free mbuf\n");
		m_freem(m);
		}
}

void ttd_server_initialize(void)
{
	vm_address_t oreply;
	vm_size_t header_size;
	
	if (ttd_server_initialized)
		return;

	init_kernel_target();

	ttd_target_machine_type = get_ttd_machine_type();

	ttd_server_initialized = TRUE;
}

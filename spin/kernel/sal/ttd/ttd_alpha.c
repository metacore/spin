/*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * Mach Operating System Copyright (c) 1993 Carnegie Mellon University
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
 * the rights to redistribute these changes.  */
/*
 * The alpha machine specific Kernel TTD code.
 *
 * HISTORY:
 * 29-May-96  Charles Garrett (garrett) at the University of Washington
 *	Removed explicit ALPHA_SPIN from include filename.
 *
 * 29-Apr-96  Stefan Savage (savage) at the University of Washington
 *	Made a fix to ttd_addr_to_kernel addr.  Added a bunch of debugging 
 *	messages. Included Brian's fixes.  Unfortunately, my emacs
 *	went indentation happy, so this differs more than it should
 *	from the previous revision. 
 *
 * 28-Apr-96  Stefan Savage (savage) at the University of Washington
 *	User space fixes, and some minor cleanup.
 *
 * 22-Feb-96  Charles Garrett (garrett) at the University of Washington
 *	Changed ALPHA_OSF to ALPHA_SPIN.
 *
 * 05-Aug-95  Brian Bershad (bershad) at the University of Washington
 *	Revised heavily to eliminate access faults and to rely on OSF bcopy
 *	routines. (also added UW copyright)
 *
 * 15-Mar-95  Marc Fiuczynski (mef) at the University of Washington
 *	Using regular splhigh() and splx() macro's instead of procedure
 *	calls to kdbsplhigh() and kdbsplx().  The corresponding functions
 *	in alpha_cpu.s don't exists anymore, since they were identical to
 *	what the spl() macros invoked.
 *
 * ttd_interface.c,v
 * Revision 1.2  1995/03/21  01:16:30  pardy
 * merge for release-6
 *
 * Revision 1.1.4.1  1995/03/15  17:59:42  mef
 * Using regular splhigh() and splx() macro's instead of procedure
 * calls to kdbsplhigh() and kdbsplx().  The corresponding functions
 * in alpha_cpu.s don't exists anymore, since they were identical to
 * what the spl() macros invoked.
 *
 * Revision 1.1  1995/03/15  17:48:07  bershad
 * firstchecking
 *
 * Revision 1.1  1995/03/10  21:55:34  bershad
 * Created.
 *
 * Revision 2.1.2.1  93/06/08  13:27:37  grm
 * 	Checkin for MK80 branch.
 * 
 * Revision 2.1.1.1  93/04/20  11:38:33  grm
 * 	Initial version.
 * 	[93/04/20            grm]
 * 
 *
 */
#include <sal/ttd/ttd_types.h>
#include <sal/ttd/ttd_stub.h>
#include <machine/ttd_machdep.h>
#include <machine/trap.h>
#include <mach/machine/thread_status.h>
#include <mach/std_types.h>
#include <vm/vm_map.h>
#include <machine/thread.h>
#include <machine/pal.h>
#include <machine/psl.h>
#include <machine/spl.h>
#include <kern/thread.h>
#define	alphacache_Iflush() imb()
#define alpha_user_mode(ps) USERMODE(ps) 
#define  addrof_alpha_reg(regn, ss_ptr) (((char*)ss_ptr)+(u_long)(ttd_regs[(regn)]))

#include <kern/thread.h>
#include <kern/processor.h>
#include <mach/vm_prot.h>
#include <machine/machparam.h>		/* spl definitions */

extern pmap_t current_pmap;
#define ZERO (0)

#define TTD_VM_PROT	(VM_PROT_READ | VM_PROT_WRITE)

#define	ALPHA_KERNEL_BREAKPOINT	(ttd_saved_inst)(0x00000080)
					/* breakpoint instruction */

#ifndef SPIN
#define ttd_current_task()                                               \
    ((current_thread())? current_thread()->task: TASK_NULL)

task_t	ttd_task;
#else
/* SPIN */
#define ISA_KUSEG(addr) ((unsigned long) addr < 0xfffffc0000000000)

#define ttd_current_task()   current_pmap	/* AGH! R. */
#define ttd_current_thread()  salhook_current_thread()

#define T_ARITHMETIC	T_ARITH
#define T_UNALIGNED	T_ALIGN
#define T_BP		T_IFAULT_BPT
#endif

extern vm_map_t	kernel_map;
extern void gimmeabreak();
extern void _gimmeabreak();


/*
 * Hack that notifies us when we are entering ttd via net packet
 * instead of ^P or Do key.
 */
boolean_t ttd_async_entry = FALSE;

/*
 * Used as temporary breakpoints in the single stepping code.
 */
typedef struct {
	ttd_saved_inst	inst;
	vm_offset_t	pc;
} ttd_temp_bp;

ttd_temp_bp ttd_taken_bkpt;
ttd_temp_bp ttd_not_taken_bkpt;

boolean_t ttd_stopping_on_boot = FALSE;
boolean_t ttd_access_debug = FALSE;
boolean_t ttd_bkpt_debug = FALSE;
boolean_t ttd_write_debug = FALSE;
boolean_t ttd_read_debug = FALSE;


/*
 * PC Where the step started.
 */
vm_offset_t ttd_step_from_pc;

/*
 * Are we stepping over a pc that already has a bp in the
 * kernel bp table?
 */
boolean_t ttd_step_over_break;

/*
 * Use this for accessing the current register state during 
 * teledebugging.
 *
 * Note:  Sandro and David do things differntly in their ddb
 *        implementations, and this is one of places I mirrored
 *        that difference.  David makes a scratch copy of the regs,
 *        whereas Sandro just looks at the copy on the stack whenever
 *        they are needed.
 */
struct alpha_saved_state *ttd_esp;

/*
 * Enter teledebugger asyncronously, in this case call gimmeabreak.
 * This is different on some architectures (eg. i386), and its use
 * varies widely.
 */
void ttd_intr(void)
{
	ttd_async_entry = TRUE;
	asm("call_pal 128");
	asm(".globl ttd_intr_called");
	asm("ttd_intr_called:");
}

/*
 * Generate a breakpoint.
 */
void ttd_break(void)
{
	gimmeabreak();
}


/*
 * Return the ttd machine type.
 */
ttd_machine_type get_ttd_machine_type(void)
{
	return TTD_ALPHA;
}

natural_t* ttd_regs[] = {
/*v0*/  (natural_t*) &(((struct alpha_saved_state *)0)->r0),
/*t0*/  (natural_t*) &(((struct alpha_saved_state *)0)->r1),
/*t1*/  (natural_t*) &(((struct alpha_saved_state *)0)->r2),
/*t2*/  (natural_t*) &(((struct alpha_saved_state *)0)->r3),
/*t3*/  (natural_t*) &(((struct alpha_saved_state *)0)->r4),
/*t4*/  (natural_t*) &(((struct alpha_saved_state *)0)->r5),
/*t5*/  (natural_t*) &(((struct alpha_saved_state *)0)->r6),
/*t6*/  (natural_t*) &(((struct alpha_saved_state *)0)->r7),
/*t7*/	 (natural_t*)&(((struct alpha_saved_state *)0)->r8),
/*s0*/  (natural_t*) &(((struct alpha_saved_state *)0)->r9),
/*s1*/  (natural_t*) &(((struct alpha_saved_state *)0)->r10),
/*s2*/  (natural_t*) &(((struct alpha_saved_state *)0)->r11),
/*s3*/  (natural_t*) &(((struct alpha_saved_state *)0)->r12),
/*s4*/  (natural_t*) &(((struct alpha_saved_state *)0)->r13),
/*s5*/  (natural_t*) &(((struct alpha_saved_state *)0)->r14),
/*s6*/  (natural_t*) &(((struct alpha_saved_state *)0)->r15),	 
/*a0*/  (natural_t*) &(((struct alpha_saved_state *)0)->r16),
/*a1*/  (natural_t*) &(((struct alpha_saved_state *)0)->r17),
/*a2*/  (natural_t*) &(((struct alpha_saved_state *)0)->r18),
/*a3*/  (natural_t*) &(((struct alpha_saved_state *)0)->r19),
/*a4*/  (natural_t*) &(((struct alpha_saved_state *)0)->r20),
/*a5*/  (natural_t*) &(((struct alpha_saved_state *)0)->r21),
/*t8*/  (natural_t*) &(((struct alpha_saved_state *)0)->r22),
/*t9*/  (natural_t*) &(((struct alpha_saved_state *)0)->r23),
/*t10*/ (natural_t*) &(((struct alpha_saved_state *)0)->r24),
/*t11*/ (natural_t*) &(((struct alpha_saved_state *)0)->r25),
/*ra*/  (natural_t*) &(((struct alpha_saved_state *)0)->r26),	
/*t12*/ (natural_t*) &(((struct alpha_saved_state *)0)->r27),
/*at*/  (natural_t*) &(((struct alpha_saved_state *)0)->r28),
/*gp*/  (natural_t*) &(((struct alpha_saved_state *)0)->r29),
/*sp*/  (natural_t*) &(((struct alpha_saved_state *)0)->r30),
/*pc*/  (natural_t*) &(((struct alpha_saved_state *)0)->pc),
/*ps*/  (natural_t*) &(((struct alpha_saved_state *)0)->ps),
};

natural_t** ttd_eregs = ttd_regs + sizeof(ttd_regs)/sizeof(ttd_regs[0]);

void
ttd_recover_esp(struct alpha_gdb_register_state *ttd_state)
{
    natural_t *p;
    int i;

    /* get registers */
    p = &ttd_state->v0;

    /* First pick up the general registers*/
    for (i = 0; i < 32; i++)  {
	p[i] = (natural_t)getreg_val(i, ttd_esp);
    }
    /* Now pick up floating point registers */
    bzero(ttd_state->floatregs, sizeof(ttd_state->floatregs));

    /* Now collect the miscellaneous stuff */
    ttd_state->pc = ttd_esp->pc;
    ttd_state->bad_address = 0;
    ttd_state->cause = 0;
}

void
ttd_overwrite_esp(struct alpha_gdb_register_state *ttd_state)
{
    natural_t *p;
    int i;

    /* get registers */
    p = &ttd_state->v0;

    /* First pick up the general registers*/
    for (i = 0; i < 32; i++)  {
	*(natural_t *)addrof_alpha_reg(i, ttd_esp) = p[i];
    }
    /* Nowhere to put floaters */

    /* set the pc to the new location */
    ttd_esp->pc = ttd_state->pc;
}



/*
 * Manipulate memory
 */


/*
 * Support routines for manipulating and querying virtual address.
 *
 * ttd_vtophys: computes the physical address of a given virtual address in some task. 
 *
 * ttd_address_ok: boolean determination if a given virtual address is in a legitimate
 *		    portion of the kernel's virtual address space (which includes
 *		    all of the user's virtual address space)
 *
 * ttd_mem_access: boolean determination if the chunk of bytes starting at a given
 *		    address is legitimate.
 *
 * ttd_addr_to_kernel_addr:
 *		    convert a virtual address to the underlying kernel address
 *		    which yields the same referent.
 *
 * ttd_rword:	    read one word from a given pmap.
 * ttd_read_bytes: read a number of bytes from a given pmap.
 *
 * ttd_wword:	    write one word into a given pmap.
 * ttd_write_bytes:write a number of words into a given pmap.
 *
 * ttd_read_instruction:
 *		    read one instruction from a task's virtual address space.
 *
 * and others...
 */


/*
 *   ttd_vtophys - map addresses by hand, so that TLB is
 *   minimally affected.  But we do take faults, if necessary.
 */
boolean_t ttd_no_vm_fault = TRUE;

static int
ttd_vtophys(task, addr, access, paddr, no_ret_error)
        task_t          task;
	vm_offset_t	addr;
	vm_prot_t	access;
	vm_offset_t	*paddr;
	boolean_t	no_ret_error;
{
	pmap_t		pmap;
	vm_offset_t	pa;	/* physical address */
	int		ret;

	if (task == TASK_NULL) {
	   if (no_ret_error) {
	      printf("ttd_vtophys: bad task");
	   }
	   return(-1);
	}
	pmap = (pmap_t) task;
	if (ttd_access_debug) {
	   printf("ttd_vtophys: pmap = 0x%lx addr = 0x%lx\n", pmap, addr);
	}
retry:
	pa = pmap_extract(pmap, addr);
	if (pa == 0) {
	   if (ttd_access_debug) {
	      printf("ttd_vtophys: pmap_extract fails\n");
	   }	    
	   if (!no_ret_error) {
	      return(-1);
	   } 
	   if (ttd_access_debug) {
	      printf("ttd_vtophys: retrying\n");
	   }	    
	   goto retry;
	}
	*paddr = (vm_offset_t) PHYS_TO_KSEG(pa);
	if (ttd_access_debug) {
	   printf("ttd_vtophys: pmap = 0x%lx pa = 0x%lx, paddr = 0x%lx\n",
		  pmap, pa, *paddr);
	}
	return(0);
}



static
boolean_t
ttd_address_ok(vm_offset_t addr, vm_prot_t access)
{
	vm_offset_t pa;
	
	if (ttd_access_debug) {
	   printf("ttd_address_ok: address 0x%lx, access  0x%d\n",
		  addr, access);
	}
	if (addr == 0) {
	   if (ttd_access_debug) {
	      printf("ttd_address_ok: zero address\n");
	   }
	   return FALSE;
	} else if (ISA_KUSEG(addr)) {
	   if (ttd_access_debug) {
	      printf("ttd_address_ok: user address\n");
	   }
	   if (ttd_vtophys(ttd_current_task(), addr, access, &pa, FALSE) < 0) {
	      if (ttd_access_debug) {
		 printf("ttd_address_ok: ttd_vtophys failed\n");
	      }
	      return FALSE;
	   } else {
	      return TRUE;
	   }
        } else if (IS_SEG0_VA(addr)) {
	   return TRUE;		
	} else if (IS_SYS_VA(addr)) {
	   return TRUE;
	} else if (IS_KSEG_VA(addr)) {
	   return TRUE;
        } else if (IS_SEG1_VA(addr)) {
	   return TRUE;
	} else return FALSE;
}

boolean_t
ttd_mem_access(vm_offset_t addr,
		vm_size_t size,
		vm_prot_t access)
{
	/* Should fault in memory... if it's not present */
	return (ttd_address_ok(addr, access)
		&& ttd_address_ok(addr + size, access));
}


/*
 * Map an address into its corresponding address in the kernel's virtual
 * address space.
 */

static boolean_t
ttd_addr_to_kernel_addr(addr, task)
	vm_offset_t *addr;
	task_t task;
{

	if (ttd_access_debug) {
	   printf("ttd_addr_to_kernel_addr: addr = %lx, pmap = %lx\n",
		  *addr, task);
	}
	if (!ttd_address_ok(*addr, VM_PROT_READ|VM_PROT_WRITE)) {
	   if (ttd_access_debug) {
	      printf("ttd_addr_to_kernel_addr: ttd_address_ok failed\n");
	   }
	   return FALSE;
	}
	
	if (ISA_KUSEG(*addr)) {
	   if (ttd_access_debug) {
	      printf("ttd_addr_to_kernel_addr: 0x%lx is a user addr\n", *addr);
	   }
	   if (task == TASK_NULL && 
	       (task = ttd_current_task()) == TASK_NULL) {
	      printf("ttd_addr_to_kernel_addr: no task\n");
	   }

	   if (ttd_vtophys(task,*addr,VM_PROT_READ|VM_PROT_WRITE,addr,FALSE)<0)
	   {
	      if (ttd_access_debug) {
		 printf("ttd_addr_to_kernel_addr: ttd_vtophys failed\n");
	      }
	      return FALSE;
	   } else {
	      if (ttd_access_debug) {
		 printf("ttd_addr_to_kernel_addr: ttd_vtophys succeeded\n",
			*addr);
	      }
	   }
	}
	return TRUE;
}



int
ttd_write_bytes(addr, len, data, task)
	natural_t	*addr;
	int   		*len;
	natural_t	*data;
	task_t		task;
{
	if (ttd_addr_to_kernel_addr(&addr, task))  {
	   if (ttd_write_debug) {
	      printf("write_bytes: 0x%lx 0x%lx %d\n", addr, *data, *len);
	   }
	   kdebug_bcopy(data, addr, *len);
	   ttd_flush_cache((vm_offset_t)addr, *len);	/* XX */
	   return TRUE;
	}
	return FALSE;
}
	


int
ttd_read_bytes(addr, len, data, task)
	natural_t	*addr;
	int   		*len;
	natural_t	*data;
	task_t		task;
{
	if (ttd_addr_to_kernel_addr(&addr, task))  {
	   if (ttd_read_debug) {
	      printf("read_bytes: 0x%lx 0x%lx %d\n", addr, *data, *len);
	   }
	   kdebug_bcopy(addr, data, *len);
	   return TRUE;
	}
	return FALSE;
}

		
/*
 * BREAKPOINT MANAGEMENT CODE
 */
ttd_flush_cache(vm_offset_t offset, vm_size_t length)
{
	alphacache_Iflush();
}



boolean_t
ttd_read_instruction(task_t task,
		      vm_offset_t address,
		      ttd_saved_inst *inst)
{
	int len = sizeof(ttd_saved_inst);
	if (!ttd_read_bytes((natural_t*)address,&len,(natural_t*)inst,task)) {
	   if (ttd_access_debug) {
	      printf("ttd_overwrite_instruction: can't read from addr 0x%lx in task 0x%lx\n", address, task);
	   }
	   return FALSE;
	}
	if (len != sizeof(ttd_saved_inst)) {
	   printf("Whoops: ttd_read_instruction couldn't read whole instruction: %d out of %d\n", len, sizeof(ttd_saved_inst));
	}
	return TRUE;
}
	
		      

boolean_t
ttd_overwrite_instruction(task_t task,
			   vm_offset_t address,
			   ttd_saved_inst new_inst,
			   ttd_saved_inst *old_inst)
{
	int len = sizeof(ttd_saved_inst);
	
	if (old_inst && !ttd_read_instruction(task, address, old_inst)) {
	   printf("ttd_overwrite_instruction: can't read from addr 0x%lx",
		  address);
	   return FALSE;
	}	


	if (!ttd_write_bytes((natural_t*)address, &len,
			     (natural_t*)&new_inst, task)) {
	   if (ttd_access_debug) {
	      printf("ttd_overwrite_instruction: can't write to addr 0x%lx in task 0x%lx\n", address, task);
	   }
	   return FALSE;
	}
	return TRUE;
}

			   
			   
/*
 * Insert a breakpoint into memory.
 */
boolean_t ttd_insert_breakpoint(task_t task,
				 vm_offset_t address,
				 ttd_saved_inst *saved_inst)
{
	boolean_t res;

	if (ttd_bkpt_debug) printf("+BKPT@0x%lx\n", address);
	res = ttd_overwrite_instruction(task,
					address,
					ALPHA_KERNEL_BREAKPOINT,
					saved_inst);
	if (!res) {
	   if (ttd_access_debug) {
	      printf("ttd_insert_breakpoint failed\n");
	   }
	}
	return res;
}


/*
 * Remove breakpoint from memory.
 */
boolean_t ttd_remove_breakpoint(task_t task,
				 vm_address_t address,
				 ttd_saved_inst saved_inst)
{
	boolean_t res;
	ttd_saved_inst bpinst;

	if (ttd_bkpt_debug) printf("-BKPT@0x%lx\n", address);	
	res = ttd_overwrite_instruction(task,
					 address,
					 saved_inst,
					 &bpinst);
	if (!res) {
		printf("ttd_delete_breakpoint failed\n");
	}
	if (bpinst != ALPHA_KERNEL_BREAKPOINT) {
		printf("WHOA! Removed a breakpoint that was not a breakpoint: 0x%lx 0x%x\n", address, bpinst);
	}
	return res;
}


/*
 * Set single stepping mode.  Assumes that program counter is set
 * to the location where single stepping is to begin.  
 *
 * This is blatantly stolen from the alpha ddb code in db_run.c :-)
 *
 * XXX Fix this to work for arbitrary tasks.
 */

boolean_t
ttd_set_machine_single_step(task_t task)
{
	vm_offset_t pc = ttd_esp->pc;
	vm_offset_t brpc;
	ttd_saved_inst	inst;
	register boolean_t	unconditional;

	/* Make sure we can touch this... */
	if (!ttd_mem_access(pc, sizeof(ttd_saved_inst),
			     TTD_VM_PROT)) {
		printf("ttd_step: can't access mem during step 0x%lx\n", pc);
		return FALSE;
		/* NOTREACHED */
	}

	ttd_step_from_pc = pc;

	/*
	 * If there is a breakpoint already set at the pc, we've got
	 * to put the saved_inst back and step over it.  We have to
	 * do this, since we don't know if the client debugger clears
	 * all the breakpoints out of memory before sending a single
	 * stepping packet.
	 */
	if (break_set(task, pc, &inst)) {

		ttd_step_over_break = TRUE;
		if (!ttd_overwrite_instruction(task,
					       pc,
					       inst,
					       0)) {
			printf("ttd_machine_single_step: failed on overwrite to 0x%lx\n", pc);
			return FALSE;
		}

	}else{
		ttd_step_over_break = FALSE;
		if (!ttd_read_instruction(task, pc, &inst)) {
			printf("ttd_machine_single_step: failed on read_instruction from 0x%lx\n", pc);
			return FALSE;
		}
	}
#ifndef SPIN
	if (isa_branch(inst) || isa_call(inst)) {
		brpc = branch_taken(inst, pc, getreg_val, ttd_esp);
#else
	if (kdebug_isa_branch(inst)) {
		brpc = kdebug_branch_target(inst, pc);
		if (ttd_bkpt_debug)		
		        printf("kdebug_is_a_branch (0x%x) from 0x%lx to 0x%lx\n", inst, pc, brpc);
#endif

		if (brpc != pc) { /* self-branches are hopeless */
		    
			if (!ttd_mem_access(brpc, sizeof(ttd_saved_inst), TTD_VM_PROT)) {
				printf("ttd_step: NO access mem in taken bp to 0x%lx\n", brpc);
				return FALSE;
				/* NOTREACHED */
			}
			ttd_taken_bkpt.pc = brpc;
			if (!ttd_overwrite_instruction(task,
							brpc,
							ALPHA_KERNEL_BREAKPOINT,
							&ttd_taken_bkpt.inst)){
				printf("ttd_smss:overwrite failed to 0x%lx\n", brpc);
				printf("E\n");
				
				return FALSE;
			}
		}else{
			ttd_taken_bkpt.pc = 0;
			ttd_taken_bkpt.inst = (ttd_saved_inst)0;
		}
#ifdef notdef
			/*XX... we don't have bdelay on the alpha == BNB */
		/* Move past the delay slot. */
		pc = pc + sizeof(ttd_saved_inst);
#endif		
	}	

	/* check if this control flow instruction is an unconditional transfer */
#ifndef SPIN
	unconditional = inst_unconditional_flow_transfer(inst);
#else
	unconditional = kdebug_isa_uncond_branch(inst);
#endif
	

	/*
	 * Point pc at the next instruction.
	 */
	pc = pc + sizeof(ttd_saved_inst);

	/* 
	  We only set the sequential breakpoint if previous instruction was not
	  an unconditional change of flow of control. If the previous instruction
	  is an unconditional change of flow of control, setting a breakpoint in the
	  next sequential location may set a breakpoint in data or in another routine,
	  which could screw up either the program or the debugger. 
	  (Consider, for instance, that the next sequential instruction is the 
	  start of a routine needed by the debugger.)
	*/

	if (!unconditional) {

		if (!ttd_overwrite_instruction(task,
						pc,
						ALPHA_KERNEL_BREAKPOINT,
						&ttd_not_taken_bkpt.inst)){
			if (ttd_access_debug) {
				printf("ktt_mss: nottaken overwrite failed:0x%lx\n", pc);
			}
			return FALSE;
		}
		ttd_not_taken_bkpt.pc = pc;

	}else{
		if (ttd_bkpt_debug)
			printf("unconditional branch taken 0x%lx\n", pc-sizeof(ttd_saved_inst));
		ttd_not_taken_bkpt.inst = (ttd_saved_inst)0;
		ttd_not_taken_bkpt.pc = 0;
	}
	return TRUE;
}

/*
 * Clear single stepping mode.
 *
 */
boolean_t
ttd_clear_machine_single_step(task_t task)
{
	ttd_saved_inst oinst;

	
	
	if (ttd_esp->pc == ttd_step_from_pc) {
		printf("ttd_clear_step: We stepped in a breakpoint!\n");
	}
	
	/*
	 * Put breakpoint inst back if we were stepping over
	 * a kernel breakpoint.
	 */
	if (ttd_step_over_break) {
		if (!ttd_overwrite_instruction(task,
						ttd_step_from_pc,
						ALPHA_KERNEL_BREAKPOINT,
						0)) {
			if (ttd_access_debug) {
				printf("ttd_clear: overwrite failed 0x%\lx\n",
				       ttd_step_from_pc);
			}
			return FALSE;
		}
	}


	if (ttd_taken_bkpt.pc != 0) {
		if (ttd_bkpt_debug)
			printf("ttd_clear<1>clearing bkpt @ 0x%lx\n",
			       ttd_taken_bkpt.pc);
		if (!ttd_overwrite_instruction(task,
						ttd_taken_bkpt.pc,
						ttd_taken_bkpt.inst,
						&oinst)) {
			printf("ttd_clear: overwrite 2 failed 0x%lx\n",
			       ttd_taken_bkpt.pc);
			return FALSE;
		}
		if (oinst != ALPHA_KERNEL_BREAKPOINT) {
			printf("panic: ttd_clear: clearing non bkpt 0x%lx\n",
			       ttd_taken_bkpt.pc);
		}
		ttd_taken_bkpt.inst = (ttd_saved_inst)0;
		ttd_taken_bkpt.pc = 0;
	}

	
	if (ttd_not_taken_bkpt.pc != 0) {
		if (ttd_bkpt_debug)		
			printf("ttd_clear<2>clearing bkpt @ 0x%lx\n",
			       ttd_not_taken_bkpt.pc);		
		if (!ttd_overwrite_instruction(task, ttd_not_taken_bkpt.pc,
						ttd_not_taken_bkpt.inst,
						&oinst)) {
			printf("ttd_clear: overwrite 3 failed 0x%lx\n",
				       ttd_not_taken_bkpt.pc);
			return FALSE;
		}
		if (oinst != ALPHA_KERNEL_BREAKPOINT) {
			printf("panic: ttd_clear: clearing<2>non bkpt 0x%lx\n",
			       ttd_not_taken_bkpt.pc);
		}		
		ttd_not_taken_bkpt.inst = (ttd_saved_inst)0;
		ttd_not_taken_bkpt.pc = 0;
	}

	return TRUE;
}


/*
 * ttd_type_to_ttdtrap:
 *
 * Fills in the task and thread info structures with the reason
 * for entering the Teledebugger (bp, single step, pg flt, etc.)
 *
 */
void ttd_type_to_ttdtrap(int type)
{
	/* XXX Fill this in sometime for ALPHA */
}


/* for insn decoding in kdebug_misc.c */
#include <sys/kdebug.h>
extern KdebugInfo kdebug_info;

/* Labels at special permanent breakpoint traps. */
extern int ttd_gimmeabreak_called[], ttd_intr_called[];

int ttd_need_halt = 0;

/*
 * ttd_trap:
 *
 * Called from trap.c or locore.s.  The flag register is TRUE if we are
 * coming from trap.c [via a fatal error], or FALSE if we're coming from
 * kdb_breakpoint() [via a kernel breakpoint].  Only in this latter case
 * is the TLB info in *esp valid.  This procedure is the mirror of kdb_trap.
 */


boolean_t ttd_trap(
	struct alpha_saved_state *esp,
	u_long	code,			
	u_long	subcode)
{
	boolean_t res;
	spl_t		s;
	

	
	s = splextreme();

	if (ttd_need_halt)   {
		printf("Backdoor halt\n");
		halt_cpu();
	}

	if (ttd_debug)
		printf("Entering ttd_trap, ttd_active = %d\n", ttd_active);	
	/*
	 * Are we already in TTD?
	 */
	if (++ttd_active > MAX_TTD_ACTIVE) {
		vm_offset_t actual_pc = esp->pc - sizeof(ttd_saved_inst);
		extern ttd_target_info kernel_target;
		printf("ttd_trap: RE-ENTERED (code=0x%lx subcode=0x%lx from 0x%lx !!!, ttd_active=%d, ttd_run_status=%d ttd_target_is_stopped=%d, spl=%d\n", code, subcode,
		       actual_pc,
		       ttd_active, ttd_run_status, kernel_target.is_stopped, s);
		/*
		  XXX some fancy code here could let ttd through the bpt but
		  take the bpt otherwise.
		 */
		esp->pc += sizeof(ttd_saved_inst);
	}  else {
		res = ttd_trap2(esp, code, subcode);
	}

	if (--ttd_active < MIN_TTD_ACTIVE)
		printf("ttd_trap: ttd_active < 0\n");

	if (ttd_debug)
		printf("Leaving ttd_trap, ttd_active = %d\n", ttd_active);

	kdebug_mb();		/* XX */
	splx(s);

	return res;
}




boolean_t ttd_trap2(
	struct alpha_saved_state *esp,
	u_long	code,			
	u_long	subcode)
{

	boolean_t	async_entry;
	long		cause;
	long		type;
	vm_address_t	ps;
	thread_t	thread;
	task_t		task;


	/*
	 * We need to know whether or not we have come into ttd via a
	 * network interrupt.  We can tell if we came from gimmeabreak.
	 * We espume that the entry into ttd via ttd_intr() will ONLY
	 * use gimmeabreak.
	 * XXX We should trace back the stack frames several times; if any
	 * return addresses "near us" are in ttd_intr() then we can espume
	 * we've gotten here via network interrupt.
	 */

	if (ttd_stopping_on_boot) {
		printf("ttd stopping on boot\n");
		async_entry = FALSE;
		ttd_stopping_on_boot = FALSE;
	} else {	
		async_entry = (esp->pc == (vm_offset_t)ttd_intr_called);
	}
	if (ttd_debug) printf("esp %#lx  pc %#lx\n",esp,esp->pc);

	ttd_esp = esp;
	kdebug_info.exc_frame = esp; /* for insn decoding in kdebug_misc.c */

	type = code;

	/*
	 * Set up the teledebug globals depending on how we entered.
	 * They will already be set if we are entering via a ttd_intr().
	 */
	
	if (!async_entry) {
		/*
		 * Entered via breakpoint or panic, no ttd message to
		 * look at.
		 */
		ttd_run_status = FULL_STOP;

		/* Store the pointer to the saved state. */
	
	}else{
		/*
		 * Put a check here to see if we're coming from
		 * serial line or Do key.  If we are then return
		 * FALSE so that ddb will handle it.
		 *
		 * This check is really lame, we should do it better
		 * (see above comment), but I want to see if I can get
		 * this running in five days....
		 */
		
		if (!ttd_async_entry) {
			if (ttd_debug)
				printf("ttd_trap2 async but not ttd_async!? how did I get here?\n");
			return FALSE;
		}

		/*
		 * Entered via ttd_intr(), do a sanity check!
		 */
		if (ttd_run_status != ONE_STOP) {
			printf("ttd_trap: INSANITY!!!\n");

			/* XXX Should we panic here?! */
		}
	}

	ps = esp->ps;

#ifndef SPIN
	esp->hw_pcb.usp = mfpr_usp();

	if (flag) {
		kdbprinttrap(type, esp);
		/*
		 * Must backup the PC in a variety of cases
		 */
		if ((type == T_ARITHMETIC) ||
		   ((type >= T_UNALIGNED) && (type <= T_CHMU)))
#else
		if ((type == T_ARITH) ||(type == T_ALIGN) ||
		   (type == T_IFAULT))
#endif
			esp->pc -= sizeof(ttd_saved_inst);
#ifndef SPIN
		/*
		 * To get a sensible trace, needs to recover trapped SP
		 */
		if (alpha_user_mode(ps)) {
			esp->sp = esp->hw_pcb.usp;
		} else {
			esp->r30 += sizeof(struct trap_frame) + (ps >> 56);
		}
	} else {
		/*
		 * Must check for regular userland bpt/sstep.
		 * We do it by noticing that we knew nothing
		 * about this bpt, and came from user mode.
		 */
		vm_offset_t	addr = (vm_offset_t)esp->pc;

		if ((type == T_BP) && alpha_user_mode(ps)) {

			/*
			 * Still must check against kernelmode sstep
			 */
			thread = ttd_current_thread();			
			if (thread && thread->pcb->mms.msss) {
				printf("{User bpt/sstep @%X}", addr);
				esp->framep->saved_pc -= sizeof(ttd_saved_inst);
				esp->framep = &esp->saved_frame;
				trap( esp, 0, 0, T_BP);
				return TRUE;
			}
		}
	}
	
		
	ttd_task_trap(type, 0x0badc0de, alpha_user_mode(ps));
#endif
        if (ttd_bkpt_debug && type == 64) {
/*		ttd_debug++; */
		printf("Into ttd_task_trap from 0x%lx\n", esp->pc);
	}

	ttd_task_trap(type, 0x0badc0de, 0);



	ttd_run_status = RUNNING;

	/*
	 * If we're handling an asyncronous packet, we've got to
	 * step past the breakpoint that brought us into the ttd
	 * code or else we'll just be right back here in the next
	 * few instruction cycles... Whhheeeeeeeeeyyy :-)
	 */
	if (async_entry || (esp->pc == (natural_t)ttd_gimmeabreak_called))
		esp->pc += sizeof(ttd_saved_inst);

	ttd_async_entry = FALSE;

	return TRUE;
}





/*********************/
/* from cmu alpha/trap.c */


/*
 *      Object:
 *              getreg_val                      EXPORTED function
 *
 *      Return the value of a register in the exception frame
 *
 */
natural_t
getreg_val(regn, ss_ptr)
        register unsigned regn;
        struct alpha_saved_state *ss_ptr;
{
        if (regn >= 31)
                return 0;
        return *(natural_t*)(addrof_alpha_reg(regn,ss_ptr));
}

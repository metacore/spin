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
 * The i386 machine specific Kernel TTD code.
 *
 * HISTORY:
 * $Log: ttd_i386.c,v $
 * Revision 1.1  1997/06/03 16:35:49  becker
 * Replace sal with kernel/salnet/clock/machinecpu/errno/select
 *
 * Revision 1.1.1.1  1996/08/15 03:23:30  fgray
 * x86 merge
 *
 * Revision 2.2  93/05/10  23:23:34  rvb
 * 	Checkin for MK80 branch.
 * 	[93/05/10  15:11:12  grm]
 * 
 * Revision 2.1.1.2  93/04/20  11:44:01  grm
 * 	Changed for use with different asyncronous entry logic.
 * 
 * Revision 2.1.1.1  93/03/25  11:31:47  grm
 * 	Moved here from ../i386at.
 * 
 *
 */

#include <setjmp.h>
#include <sys/param.h>
#include <sys/systm.h>
#include <sys/proc.h>
#include <sys/user.h>
#include <sys/acct.h>
#include <sys/kernel.h>
#include <sys/syscall.h>
#include <sys/sysent.h>
#include <sys/types.h>

#include <vm/vm.h>
#include <vm/vm_map.h>
#include <vm/vm_kern.h>
#include <machine/pmap.h>
#include <machine/db_machdep.h>
#include <sal/ttd/ttd_types.h>
#include <sal/ttd/ttd_debug.h>
#include <sal/ttd/ttd_thread.h>
#include <sal/ttd/ttd_stub.h>
#include <sal/ttd/ttd_interface.h>
#include <machine/ttd_machdep.h>

#include <machine/cpu.h>
#include <machine/md_var.h>
#include <machine/psl.h>
#include <machine/reg.h>
#include <machine/trap.h>

boolean_t ttd_stopping_on_boot = FALSE;
boolean_t ttd_access_debug = FALSE;
boolean_t ttd_bkpt_debug = FALSE;
boolean_t ttd_write_debug = FALSE;
boolean_t ttd_read_debug = FALSE;

struct i386_saved_state *ttd_regs;

#define	I386_BREAKPOINT	0xcc

/* Labels at special permanent breakpoint traps. */
extern int  ttd_intr_called[];

/*
 * Enter teledebugger asyncronously, in this case call gimmeabreak.
 * This is different on some architectures (eg. i386), and its use
 * varies widely.
 */
void ttd_intr(void)
{
    if(!ttd_enabled)
	return;
    asm("int3");
    asm(".globl _ttd_intr_called");
    asm("_ttd_intr_called:");
}

/*
 *	Execute a break instruction that will invoke ttd
 */
void ttd_break(void)
{
	if (!ttd_enabled)
		return;
	asm("int3");
}

/*
 * Halt all processors on the 386at (not really applicable).
 */
void ttd_halt_processors(void)
{
	/* XXX Fix for Sequent!!! */
	/* Only one on AT386, so ignore for now... */
}

/*
 * Return the ttd machine type for the i386at
 */
ttd_machine_type get_ttd_machine_type(void)
{
	return TTD_AT386;
}

void ttd_recover_esp(struct i386_gdb_register_state *ttd_state)
{
    ttd_state->es = ttd_regs->tf_es;
    ttd_state->ds = ttd_regs->tf_ds;
    ttd_state->edi = ttd_regs->tf_edi;
    ttd_state->esi = ttd_regs->tf_esi;
    ttd_state->ebp = ttd_regs->tf_ebp;
    ttd_state->ebx = ttd_regs->tf_ebx;
    ttd_state->edx = ttd_regs->tf_edx;
    ttd_state->ecx = ttd_regs->tf_ecx;
    ttd_state->eax = ttd_regs->tf_eax;
    ttd_state->eip = ttd_regs->tf_eip;
    ttd_state->cs = ttd_regs->tf_cs;
    ttd_state->efl = ttd_regs->tf_eflags;
	
    if (ISPL(ttd_regs->tf_cs) == SEL_UPL) {
	ttd_state->esp = ttd_regs->tf_esp;
	ttd_state->ss = ttd_regs->tf_ss;
    } else {
    	/*
	 * Kernel mode - esp and ss not saved
	 */
	ttd_state->esp = (int)&ttd_regs->tf_esp; /* kernel stack pointer */
	ttd_state->ss  = ttd_regs->tf_ds;
    }
}

void ttd_overwrite_esp(struct i386_gdb_register_state *ttd_state)
{
    if (ttd_regs->tf_es != ttd_state->es) {
		if (ttd_debug)
			printf("es 0x%x:0x%x, ", ttd_regs->tf_es, ttd_state->es);
		ttd_regs->tf_es = ttd_state->es;
	}
	if (ttd_regs->tf_ds != ttd_state->ds) {
		if (ttd_debug)
			printf("ds 0x%x:0x%x, ", ttd_regs->tf_ds, ttd_state->ds);
		ttd_regs->tf_ds = ttd_state->ds;
	}
	if (ttd_regs->tf_edi != ttd_state->edi) {
		if (ttd_debug)
			printf("edi 0x%x:0x%x, ", ttd_regs->tf_edi, ttd_state->edi);
		ttd_regs->tf_edi = ttd_state->edi;
	}
	if (ttd_regs->tf_esi != ttd_state->esi) {
		if (ttd_debug)
			printf("esi 0x%x:0x%x, ", ttd_regs->tf_esi, ttd_state->esi);
		ttd_regs->tf_esi = ttd_state->esi;
	}
	if (ttd_regs->tf_ebp != ttd_state->ebp) {
		if (ttd_debug)
			printf("ebp 0x%x:0x%x, ", ttd_regs->tf_ebp, ttd_state->ebp);
		ttd_regs->tf_ebp = ttd_state->ebp;
	}
	if (ttd_regs->tf_ebx != ttd_state->ebx) {
		if (ttd_debug)
			printf("ebx 0x%x:0x%x, ", ttd_regs->tf_ebx, ttd_state->ebx);
		ttd_regs->tf_ebx = ttd_state->ebx;
	}
	if (ttd_regs->tf_edx != ttd_state->edx) {
		if (ttd_debug)
			printf("edx 0x%x:0x%x, ", ttd_regs->tf_edx, ttd_state->edx);
		ttd_regs->tf_edx = ttd_state->edx;
	}
	if (ttd_regs->tf_ecx != ttd_state->ecx) {
		if (ttd_debug) 
			printf("ecx 0x%x:0x%x, ", ttd_regs->tf_ecx, ttd_state->ecx);
		ttd_regs->tf_ecx = ttd_state->ecx;
	}
	if (ttd_regs->tf_eax != ttd_state->eax) {
		if (ttd_debug)
			printf("eax 0x%x:0x%x, ", ttd_regs->tf_eax, ttd_state->eax);
		ttd_regs->tf_eax = ttd_state->eax;
	}
	if (ttd_regs->tf_eip != ttd_state->eip) {
		if (ttd_debug)
			printf("eip 0x%x:0x%x, ", ttd_regs->tf_eip, ttd_state->eip);
		ttd_regs->tf_eip = ttd_state->eip;
	}
	if (ttd_regs->tf_cs != ttd_state->cs) {
		if (ttd_debug)
			printf("cs 0x%x:0x%x, ", ttd_regs->tf_cs, ttd_state->cs);
		ttd_regs->tf_cs = ttd_state->cs;
	}
	if (ttd_regs->tf_eflags != ttd_state->efl) {
		if (ttd_debug)
			printf("efl 0x%x:0x%x, ", ttd_regs->tf_eflags, ttd_state->efl);
		ttd_regs->tf_eflags = ttd_state->efl;
	}

    if (ISPL(ttd_regs->tf_cs) == SEL_UPL) {

	if (ttd_regs->tf_esp != ttd_state->esp) {
		if (ttd_debug)
			printf("esp 0x%x:0x%x, ", ttd_regs->tf_esp, ttd_state->esp);
		ttd_regs->tf_esp = ttd_state->esp;
	}
	if (ttd_regs->tf_ss != ttd_state->ss) {
		if (ttd_debug)
			printf("ss 0x%x:0x%x, ", ttd_regs->tf_ss, ttd_state->ss);
		ttd_regs->tf_ss = ttd_state->ss;
	}
    }
}

/*
 *	Enable a page for access, faulting it in if necessary
 */
boolean_t ttd_mem_access(vm_offset_t offset, vm_prot_t access)
{
	int	code;

	/*
	 *	VM_MIN_KERNEL_ADDRESS if the beginning of equiv
	 *	mapped kernel memory.  virtual_end is the end.
	 *	If it's in between it's always accessible
	 */
	if (offset >= VM_MIN_KERNEL_ADDRESS && offset < virtual_end)
		return TRUE;

#if 0
	if (offset >= virtual_end) {
		/*
		 *    	fault in the memory just to make sure we can access it
		 */
		if (ttd_debug)
			printf(">>>>>>>>>>Faulting in memory: 0x%x, 0x%x\n",
			       trunc_page(offset), access);
		code = vm_fault(kernel_map, trunc_page(offset), access, FALSE, 
				FALSE, (void (*)()) 0);
	}else{
		/*
		 * Check for user thread
		 */
#if	1
		if ((current_thread() != THREAD_NULL) && 
		    (current_thread()->task->map->pmap != kernel_pmap) &&
		    (current_thread()->task->map->pmap != PMAP_NULL)) {
			code = vm_fault(current_thread()->task->map,
					trunc_page(offset), access, FALSE,
					FALSE, (void (*)()) 0);
		}else{
			/*
			 * Invalid kernel address (below VM_MIN_KERNEL_ADDRESS)
			 */
			return FALSE;
		}
#else
		if (ttd_debug)
			printf("==========Would've tried to map in user area 0x%x\n",
			       trunc_page(offset));
		return FALSE;
#endif	/* 0 */
	}

	return (code == KERN_SUCCESS);
#endif
	return !code;
}

/*
 *	See if we modified the kernel text and if so flush the caches.
 *	This routine is never called with a range that crosses a page
 *	boundary.
 */
void ttd_flush_cache(vm_offset_t offset, vm_size_t length)
{
	/* 386 doesn't need this */
	return;
}

/*
 * Insert a breakpoint into memory.
 */
boolean_t ttd_insert_breakpoint(task_t task,
				 vm_address_t address,
				 ttd_saved_inst *saved_inst)
{
	/*
	 * Saved old memory data:
	 */
	*saved_inst = *(unsigned char *)address;

	/*
	 * Put in a Breakpoint:
	 */
	*(unsigned char *)address = I386_BREAKPOINT;

	return TRUE;
}

/*
 * Remove breakpoint from memory.
 */
boolean_t ttd_remove_breakpoint(task_t task,
                                 vm_address_t address,
				 ttd_saved_inst saved_inst)
{
	/*
	 * replace it:
	 */
	*(unsigned char *)address = (saved_inst & 0xff);

	return TRUE;
}

/*
 * Set single stepping mode.  Assumes that program counter is set
 * to the location where single stepping is to begin.  The 386 is
 * an easy single stepping machine, ie. built into the processor.
 */
boolean_t ttd_set_machine_single_step(task_t task)
{
	/* Turn on Single Stepping */
	ttd_regs->tf_eflags |= PSL_T;

	return TRUE;
}

/*
 * Clear single stepping mode.
 */
boolean_t ttd_clear_machine_single_step(task_t task)
{
	/* Turn off the trace flag */
	ttd_regs->tf_eflags &= ~(PSL_T);

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
	/* XXX Fill this in sometime for i386 */
}

static jmp_buf *ttd_nofault;
static jmp_buf ttd_jmpbuf;

/*
 * ttd_trap:
 *
 *  This routine is called from the trap or interrupt handler when a
 * breakpoint instruction is encountered or a single step operation
 * completes. The argument is a pointer to a machine dependent
 * saved_state structure that was built on the interrupt or kernel stack.
 *
 */
boolean_t ttd_trap(int	type, int code, struct i386_saved_state *regs)
{
	int s;

	if(!ttd_enabled)
	    return FALSE;

	if(ttd_nofault)
	    longjmp(*ttd_nofault, 1);

	if (ttd_debug)
		printf("ttd_TRAP, before splhigh()\n");

	s = splhigh();

	/*
	 * We are already in TTD!
	 */
	if (++ttd_active > MAX_TTD_ACTIVE) {
		printf("ttd_trap: RE-ENTERED!!!\n");

		/*
		 * This will, hopefully, drop us back in
		 * ddb.  Cross your fingers kids and be
		 * careful when you do a continue!
		 */
		ttd_enabled = FALSE;

		splx(s);
		return FALSE;
	}

	if (ttd_debug)
		printf("ttd_TRAP, after splhigh()\n");

	/*  Should switch to ttd's own stack here. */

	ttd_regs = regs;

#if 0
	if ((regs->tf_cs & 0x3) == 0) {
	    /*
	     * Kernel mode - esp and ss not saved
	     */
	    ttd_regs.esp = (int)&regs->esp;	/* kernel stack pointer */
	    ttd_regs.ss   = KERNEL_DS;
	}
#endif

	/*
	 * If this was not entered via an interrupt (type != -1)
	 * then we've entered via a bpt, single, etc. and must
	 * set the globals.
	 *
	 * Setup the ttd globals for entry....
	 */

	if (regs->tf_eip != (vm_offset_t) ttd_intr_called)
	    ttd_run_status = FULL_STOP;

	ttd_task_trap(type, code, (regs->tf_cs & 0x3) != 0);

	if (--ttd_active < MIN_TTD_ACTIVE)
		printf("ttd_trap: ttd_active < 0\n");

	if (ttd_debug) {
		printf("Leaving ttd_trap, ttd_active = %d\n", ttd_active);
	}

	ttd_run_status = RUNNING;

	(void) splx(s);

	/*
	 * Return true, that yes we handled the trap.
	 */
	return TRUE;
}

extern pmap_t current_pmap;

int
ttd_write_bytes(addr, len, data, task)
        natural_t       *addr;
        volatile int    *len;
        natural_t       *data;
        task_t          task;
{
    char       *src;
    char       *dest;
    volatile int size = *len;

    ttd_nofault = &ttd_jmpbuf;
    if(setjmp(*ttd_nofault)) {
	ttd_nofault = 0;
	return *len - size;
    }

    dest = (char *)addr;
    src = (char *)data;
    while (--size >= 0) {
	if(!pmap_extract(current_pmap, (vm_offset_t) dest))
	    return *len - size;
	*dest++ = *src++;
    }
    
    ttd_nofault = 0;

    return *len;
}

int
ttd_read_bytes(addr, len, data, task)
        natural_t       *addr;
        volatile int    *len;
        natural_t       *data;
        task_t          task;
{
    char       *src;
    char       *dest;
    volatile int size = *len;

    ttd_nofault = &ttd_jmpbuf;
    if(setjmp(*ttd_nofault)) {
	ttd_nofault = 0;
	return *len - size;
    }

    src = (char *)addr;
    dest = (char *)data;
    while (--size >= 0) {
	if(!pmap_extract(current_pmap, (vm_offset_t) src))
	    return *len - size;
	*dest++ = *src++;
    }

    ttd_nofault = 0;

    return *len;
}







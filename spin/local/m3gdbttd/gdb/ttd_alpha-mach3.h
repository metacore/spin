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
 * Machine specific TTD definitions for the alphamach3 configuration.
 *
 * HISTORY
 * $Log: ttd_alpha-mach3.h,v $
 * Revision 1.3  1996/07/02 23:49:11  bershad
 * Major revision to integrate code from network available m3gdb (also lots of bug
 * fixes)
 *
 * Revision 1.2  1996/02/21  18:57:59  bershad
 * *** empty log message ***
 *
 *
 * Modified by bershad 2/20/96
 * Changed DECR_PC to 0 instead of 4.
 *
 * Revision 1.1.160.1  1996/02/20  17:39:26  bershad
 * *** empty log message ***
 *
 * Revision 1.1  1995/07/06  01:08:03  bershad
 * New File
 *
 * Revision 1.1  1995/07/06  00:25:26  bershad
 * *** empty log message ***
 *
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
 *	   DEC SRC
 *         Palo Alto CA.
 */

#ifndef _TTD_target__included_
#define _TTD_target__included_

#define TTD_TARGET_MACHINE_TYPE TTD_ALPHA

#define TTD_TRAP_MAPPING { \
    SIGINT, SIGSYS, SIGBUS, SIGTRAP, SIGTRAP, SIGTRAP, \
    SIGTRAP, SIGTRAP, SIGTRAP, SIGBUS, SIGSEGV, \
    SIGBUS, SIGBUS, SIGFPE, SIGFPE, SIGTRAP, SIGILL, \
    SIGFPE, SIGFPE, SIGFPE, SIGFPE, SIGFPE, SIGFPE }

#define TTD_FETCH_STATE_TO_REGISTERS(state)	\
	bcopy(state, &registers[0], NUM_REGS * sizeof(unsigned long));

#define TTD_STORE_REGISTERS_TO_STATE(state)	\
	bcopy(&registers[0], state, NUM_REGS * sizeof(unsigned long));

#define TTD_MK_INVALID_FRAME_CHAIN(address, nextframe) \
	((address < nextframe+8) || (nextframe <0xfffffc0000000000))
	/* Frame is a KUSEG */



extern int ttd_mk_debug;

/*
 * The following information comes from the kttd_machdep.h file in
 * the mk/kernel/alpha directory.
 */

typedef unsigned long natural_t;

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

typedef unsigned int ttd_saved_inst;


/*
 * Structure definitions borrowed from kernel/alpha/*.h which are necessary
 * for interpreting the state at the time of an exception.
 * XX THESE SHOULD BE CHANGED IF THE KERNEL CHANGES.
 */

#define TTD_KERNEL_STACK_SIZE (2 * 8192)

#define IM_EXC	0x80000000		/* from mach/alpha/asm.h */

struct ttd_trap_frame {
	unsigned long		saved_r2;
	unsigned long		saved_r3;
	unsigned long		saved_r4;
	unsigned long		saved_r5;
	unsigned long		saved_r6;
	unsigned long		saved_r7;
	unsigned long		saved_pc;
	unsigned long		saved_ps;
};

typedef struct ttd_hw_pcb {
	vm_offset_t	ksp;
	vm_offset_t	esp;
	vm_offset_t	ssp;
	vm_offset_t	usp;
	vm_offset_t	ptbr;
	long		asn;
	long		ast_status;
	long		fpa_enabled;
	long		cycle_counter;
	long		process_unique;
	long		pal_scratch[6];
} *ttd_hw_pcb_t;


/*
 *	Kernel state.  Saved and restored across context-switches
 *	inside the kernel.  We can ignore caller-saved registers.
 *	Kept at the base of the thread's stack.
 */

struct ttd_alpha_kernel_state {
	vm_offset_t	s0;		/* callee-saved */
	vm_offset_t	s1;
	vm_offset_t	s2;
	vm_offset_t	s3;
	vm_offset_t	s4;
	vm_offset_t	s5;
	vm_offset_t	s6;
	vm_offset_t	sp;		/* stack  pointer */
	vm_offset_t	pc;		/* suspended program counter */
};

/*
 *	Machine state.  Includes all machine registers and other
 *	field used by machine-level code to provide in software
 *	things that architectures other than ALPHA might provide
 *	in hardware, e.g. single-stepping.  The FPA state is scheduled
 *	asyncronously and saved here also, on demand.  Part of the pcb.
 *	We allocate space for this state as needed.
 */

struct alpha_float_state;
struct alpha_sstep_state;


struct ttd_alpha_sstep_state {
	int	ss_count;	/* no. of breakpoints installed */
	struct	ttd_breakpoint {
		vm_offset_t	address;	/* where */
		ttd_saved_inst instruction;	/* original inst. */
	} ss_bp[2];		/* taken/nontaken sides of branch */
};

struct ttd_alpha_machine_state {
	struct alpha_float_state *mfs;	/* see mach/alpha/thread_status.h */
	struct alpha_sstep_state *msss;	/* single-stepping if present */
};

struct ttd_alpha_saved_state   {
	struct ttd_hw_pcb	hw_pcb;		/* with usp */
/* wline */
	struct ttd_trap_frame
			*framep;	/* t1-t6, pc, ps */
	vm_offset_t	gp;		/* global pointer */
	vm_offset_t	a0;		/* argument 0 */
	vm_offset_t	a1;		/* argument 1 */
/* wline */
	vm_offset_t	a2;		/* argument 2 */
	vm_offset_t	a3;		/* argument 3 */
	vm_offset_t	a4;		/* argument 4 */
	vm_offset_t	a5;		/* argument 5 */
/* wline */
	vm_offset_t	ra;		/* return address */
	vm_offset_t	v0;		/* return value 0 */
	vm_offset_t	t0;		/* caller saved 0 */
	vm_offset_t	t7;		/* caller saved 7 */
/* wline */
	vm_offset_t	t8;		/* caller saved 8 */
	vm_offset_t	t9;		/* caller saved 9 */
	vm_offset_t	t10;		/* caller saved 10 */
	vm_offset_t	t11;		/* caller saved 11 */
/* wline */
	vm_offset_t	t12;		/* caller saved 12 */
	vm_offset_t	s0;		/* callee saved 0 */
	vm_offset_t	s1;		/* callee saved 1 */
	vm_offset_t	s2;		/* callee saved 2 */
/* wline */
	vm_offset_t	s3;		/* callee saved 3 */
	vm_offset_t	s4;		/* callee saved 4 */
	vm_offset_t	s5;		/* callee saved 5 */
	vm_offset_t	s6;		/* callee saved 6 */
/* wline */
	vm_offset_t	at;		/* assembler temporary */
	vm_offset_t	sp;		/* stack pointer (if kernel) */
	vm_offset_t	bad_address;	/* bad virtual address */
	vm_offset_t	cause;		/* trap cause */

	struct ttd_trap_frame
			saved_frame;	/* t1-t6, pc, ps */
};

/*
 *	At the base of a kernel stack is an "exception link" record.
 *	It contains the C calling sequence's argument save area.
 *	It also contains a pointer to the exception frame (alpha_saved_state).
 *	If the exception happened in user mode, then the exception frame
 *	is in the thread's pcb.  If the exception happed in kernel mode,
 *	then the exception frame is further up the kernel stack.
 */
struct ttd_alpha_exception_link {
	struct ttd_alpha_saved_state *eframe;/* pointer to exception frame */
	struct ttd_trap_frame	tf;	/* HW saves regs here, and pc+ps */
};

/*
 *	Lives at the base of a kernel stack.
 *	The full arrangement is
 *	stack:	...
 *		struct alpha_exception_link
 *		struct alpha_kernel_state
 *		struct alpha_stack_base
 *	stack+KERNEL_STACK_SIZE:
 */
typedef struct ttd_pcb {
	struct ttd_alpha_saved_state	mss;	/* includes hw_pcb, first! */
	struct ttd_alpha_machine_state	mms;
	/* roundup, cuz HW wants it 128-byte aligned */
	char	pad[ 512 -
			(sizeof(struct ttd_alpha_saved_state) +
			 sizeof(struct ttd_alpha_machine_state)) ];
} *ttd_pcb_t;	/* exported */

struct ttd_alpha_stack_base {
	vm_offset_t	next;		/* next stack on free list */
	/*struct vm_page	**/
	vm_offset_t	page;		/* page structure for this stack */
	ttd_pcb_t		pcb;		/* pointer to our pcb */
					/* align, cuz trap_frame will */
	char		pad[64-sizeof(vm_offset_t)-sizeof(vm_offset_t)-sizeof(ttd_pcb_t)];
};

#define	USER_REGS(th)	((th)->pcb)

#define STACK_MSB(stack)	\
	((struct ttd_alpha_stack_base *)((stack) + TTD_KERNEL_STACK_SIZE) - 1)
#define STACK_MEL(stack)	\
	((struct ttd_alpha_exception_link *)STACK_MSB(stack) - 1)
#define STACK_MKS(stack)	\
	((struct ttd_alpha_kernel_state *)STACK_MEL(stack) - 1)


/*
 * Remote kernels report the real PC when we hit a break. No need to
 * compensate.
 */
#define DECR_PC_AFTER_BREAK   alpha_decr_pc_after_break() 

#endif	/* _TTD_target__included_ */






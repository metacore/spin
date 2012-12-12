/*
 *
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 29-May-96  Charles Garrett (garrett) at the University of Washington
 *	Added profile calls to beginning of stubs. Profiling of stubs is 
 *	performed only when the symbol CALL_GRAPH is defined at compile time.
 *
 * 01-Apr-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added support for patching procedure entry to do the dispatch.
 *
 * 10-Mar-95  Przemek Pardyak (pardy) at the University of Washington
 *	Added third level of optimization (unrolled dispatch loop).
 *
 * 26-Jan-95  Przemek Pardyak (pardy) at the University of Washington
 *	Added second level of optimization (stiching from code snippets).
 */

#define LONG	
	
#include <machine/regdef.h>
	
	.ugen	
	.extern	MI_MachineDispatcher 0
	.text	
	.align	4
	.file	2 "DispatcherStubs.s"
	.set noreorder

/******************************************************************************
 *	
 *				LEVEL 0
 *
 *****************************************************************************/

/*
 * trampoline_stub is a stub that saves callee saved registers and jumps 
 * to the right entry in the MachineDispatcher jtable
 */
	
/*		
 * TRAMPOLINE_STUB_FRAME_SIZE is the size of the stack frame allocated
 * by this stub.  This constant has to by manually synchronized with 
 * Dispatcher.m3
 */
#define TRAMPOLINE_STUB_FRAME_SIZE 88

	.globl	trampoline_stub
	.globl	trampoline_stub_end
	.globl  trampoline_gp_patch
	.globl  trampoline_long_patch
	.globl	trampoline_offset_patch
	.globl	trampoline_gp_patch
	.globl	trampoline_profile_gp_patch
	
	.ent	trampoline_stub
trampoline_stub:
trampoline_gp_patch:
	ldgp	gp, 0(t12)

#ifdef CALL_GRAPH
	.set	noat
	bis	ra, 1, ra
	lda	$at, _mcount
	jsr	$at, ($at), _mcount
	.set	at
trampoline_profile_gp_patch:	
	ldgp	gp,0(pv)
#endif CALL_GRAPH
			
	lda	sp, -TRAMPOLINE_STUB_FRAME_SIZE(sp)
	stq	ra, 0(sp)
	stq	a0, 16(sp)
	stq	a1, 24(sp)

 # save callee saved registers to be able to access them in handlers for
 # Strand.Stop, stub doesn't have to restore it because the code that
 # gets called by this stub will do it
	stq	s0, 32(sp)
	stq	s1, 40(sp)
	stq	s2, 48(sp)
	stq	s3, 56(sp)
	stq	s4, 64(sp)
	stq	s5, 72(sp)
	stq	s6, 80(sp)

	.mask	0x0403fe00, -TRAMPOLINE_STUB_FRAME_SIZE
	.frame	sp, TRAMPOLINE_STUB_FRAME_SIZE, ra, 0
	.prologue	1

 # get the pointer to the dispatcher jtable
	lda	t1, MI_MachineDispatcher

 # get the first argument (pointer to the alias descriptor)
trampoline_long_patch:	
	ldah	t2, 0x0(zero)
	lda	t2, 0x0(t2)
	sll	t2, 0x20, t2
	ldah	t2, 0x0(t2)
	lda	t2, 0x0(t2)	
 	bis	t2, t2, a0
	
 # get the second argument (the original stack pointer)
	lda	a1, TRAMPOLINE_STUB_FRAME_SIZE(sp)

 # and save it with the callee-save registers
	stq	a1, 8(sp)
	
 # jump to Dispatcher.Dispatch 
trampoline_offset_patch:	
	ldq	t12, 0(t1)
	jsr	ra, (t12), 0
	ldq	ra, 0(sp)
	lda	sp, TRAMPOLINE_STUB_FRAME_SIZE(sp)
	ret	zero, (ra), 1
trampoline_stub_end:	
	.end	trampoline_stub


/* #ifdef SIRPA*/

/* CLEAN */
		
/******************************************************************************
 *	
 *				LEVEL 1
 *
 *****************************************************************************/

/* word size in bytes */
#define WORD_SIZE 8

/* these constants have to be synchronized manually with Dispatcher.m3 */
#define MaxNumOfHandlersForDebug	255
#define MaxNumOfArgumentsForDebug	15

/* offset to the procedure that raises NoHandlerInvoked exception */
#define RaiseNoHandler_OFFSET		608

/*
 * arguments:	
 *    1st - 4th  => s0 - s3
 *    5th - 6th  => (72, 8)(ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+72)(sp)
 *    7th - 15th => (0 - 64)(PESSIMISTIC_STUB_FRAME_SIZE+sp)
 */
	
/* size of the structure HandlerDesc and offsets to this structure */
/* has to be manually synchronized with DispDesc.i3 */
#define SizeOfHandlerDesc	8 * WORD_SIZE
#define nImposedOffset		0 * WORD_SIZE
#define guardOffset		1 * WORD_SIZE
#define handlerOffset		2 * WORD_SIZE
#define cancelOffset		3 * WORD_SIZE
#define useGuardClosureOffset	4 * WORD_SIZE
#define useHandlerClosureOffset	5 * WORD_SIZE
#define guardClosureOffset	6 * WORD_SIZE
#define handlerClosureOffset	7 * WORD_SIZE

#define SizeOfDispatchDesc      7 * WORD_SIZE
#define defaultResultOffset     0 * WORD_SIZE
#define defaultHandlerOffset    1 * WORD_SIZE
#define defaultClosureOffset    2 * WORD_SIZE
#define useDefaultClosure       3 * WORD_SIZE
#define resultHandlerOffset     4 * WORD_SIZE
#define resultClosureOffset     5 * WORD_SIZE
#define useResultClosure        6 * WORD_SIZE

/* size of the stack frame created by the pessimistic_stub */
#define REGISTERS_SAVE_FRAME	96
#define GUARD_RESULTS_FRAME	(MaxNumOfHandlersForDebug + 1) * WORD_SIZE

/* max number of args, plus closure, minus 4 args always kept in regs */
#define ARGUMENTS_FRAME		(MaxNumOfArgumentsForDebug + 1 - 4) * WORD_SIZE

#define PESSIMISTIC_STUB_FRAME_SIZE	(REGISTERS_SAVE_FRAME + \
					 GUARD_RESULTS_FRAME + \
					 ARGUMENTS_FRAME)
	
	.globl	pessimistic_stub
	.globl	pessimistic_stub_end
	.globl  pessimistic_handlers_gl_long
	.globl  pessimistic_handlers_hl_long
	.globl	pessimistic_gp_patch
	.globl	pessimistic_profile_gp_patch
	.globl	pessimistic_guard_gp_patch
	.globl	pessimistic_handler_gp_patch
	.globl	pessimistic_pass_callee_saved_1
	.globl	pessimistic_pass_callee_saved_2
	.globl	pessimistic_inc_stack
	.globl	pessimistic_jmp_no_handler_1
	.globl	pessimistic_jmp_no_handler_2
	.globl	pessimistic_return
	
	.ent	pessimistic_stub
	.mask	0x04000000, -PESSIMISTIC_STUB_FRAME_SIZE
	.frame	sp, PESSIMISTIC_STUB_FRAME_SIZE, ra, 48
	.prologue	1

pessimistic_stub:
pessimistic_gp_patch:
	ldgp	gp, 0(t12)

#ifdef CALL_GRAPH
	.set	noat
	bis	ra, 1, ra
	lda	$at, _mcount
	jsr	$at, ($at), _mcount	# Call profile routine
	.set	at
pessimistic_profile_gp_patch:
	ldgp	gp,0(pv)
#endif CALL_GRAPH
	
	bis	sp, sp, t1
	lda	sp, -PESSIMISTIC_STUB_FRAME_SIZE(sp)

	stq	ra, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+0)(sp)
	stq	t1, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+8)(sp)
	stq	a4, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+16)(sp)
	stq	a5, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+24)(sp)
	stq	s0, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+32)(sp)
	stq	s1, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+40)(sp)
	stq	s2, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+48)(sp)
	stq	s3, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+56)(sp)
	stq	s4, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+64)(sp)
	stq	s5, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+72)(sp)
	stq	s6, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+80)(sp)

	# save arguments
	bis	a0, a0, s0
	bis	a1, a1, s1
	bis	a2, a2, s2
	bis	a3, a3, s3

pessimistic_guard_loop:
	# load pointer to handler descriptor list from the alias descriptor
pessimistic_handlers_gl_long:
	ldah	s6, 0x0(zero)
	lda	s6, 0x0(s6)
	sll	s6, 0x20, s6
	ldah	s6, 0x0(s6)
	lda	s6, 0x0(s6)	
	ldq	s6, 0(s6)

	# skip result handling procedure
	lda	s6, SizeOfDispatchDesc(s6)

	# is there any handler?
	ldq	t12, handlerOffset(s6)
	bne	t12, skip_1
pessimistic_jmp_no_handler_1:	
	bsr	pessimistic_no_handler
skip_1:

	# pointer to the list of flags for handlers to be called
	lda	s4, ARGUMENTS_FRAME(sp)
	
	# set the count of handlers to be invoked
	bis	zero, zero, s5

pessimistic_gl_loop_head:
	# check for NIL guard
	ldq	t12, guardOffset(s6)
	bne	t12, pessimistic_call_guard
	lda	v0, 1(zero)
	stq	v0, 0(s4)
	br	pessimistic_guard_true

pessimistic_call_guard:	
	ldq	t1, useGuardClosureOffset(s6)
	bne	t1, pessimistic_guard_closure
	
	# set the arguments in registers
	bis	s0, s0, a0
	bis	s1, s1, a1
	bis	s2, s2, a2
	bis	s3, s3, a3
	ldq	a4, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+16)(sp)
	ldq	a5, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+24)(sp)

	# set the arguments on stack
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+0)(sp)
	stq	t2, 0(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+8)(sp)
	stq	t2, 8(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+16)(sp)
	stq	t2, 16(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+24)(sp)
	stq	t2, 24(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+32)(sp)
	stq	t2, 32(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+40)(sp)
	stq	t2, 40(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+48)(sp)
	stq	t2, 48(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+56)(sp)
	stq	t2, 56(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+64)(sp)
	stq	t2, 64(sp)

	br	pessimistic_execute_guard
	
pessimistic_guard_closure:		
	# set the arguments in registers
	ldq	a0, handlerClosureOffset(s6)
	bis	s0, s0, a1
	bis	s1, s1, a2
	bis	s2, s2, a3
	bis	s3, s3, a4
	ldq	a5, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+16)(sp)

	# copy arguments 5th through 15th
	ldq	t2, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+24)(sp)
	stq	t2, 0(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+0)(sp)
	stq	t2, 8(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+8)(sp)
	stq	t2, 16(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+16)(sp)
	stq	t2, 24(sp)
 	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+24)(sp)
	stq	t2, 32(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+32)(sp)
	stq	t2, 40(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+40)(sp)
	stq	t2, 48(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+48)(sp)
	stq	t2, 56(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+56)(sp)
	stq	t2, 64(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+64)(sp)
	stq	t2, 72(sp)

pessimistic_execute_guard:
	# execute a guard
	jsr     ra, (t12), 0
pessimistic_guard_gp_patch:
	ldgp	gp, 0(ra)

	# save result
	stq	v0, 0(s4)
	
	# skip if false
	beq     v0, pessimistic_gl_loop_tail

pessimistic_guard_true:
	# increase the counter of handlers to be invoked
	lda	s5, 1(s5)

	# check if it cancels
	ldq	t12, cancelOffset(s6)
	bne	t12, pessimistic_handler_loop

pessimistic_gl_loop_tail:
	# check whether there is one more handler
	lda	s4, 8(s4)
	lda	s6, SizeOfHandlerDesc(s6)
	ldq	t12, handlerOffset(s6)
	beq	t12, pessimistic_handler_loop

	# repeat
	br	pessimistic_gl_loop_head
	
	# execute all handlers in the call list
pessimistic_handler_loop:
	# check whether there are any handlers called
	bne	s5, skip_2
pessimistic_jmp_no_handler_2:
	bsr	pessimistic_no_handler
skip_2:
	
	# reload pointers
pessimistic_handlers_hl_h:
pessimistic_handlers_hl_long:
	ldah	s6, 0x0(zero)
	lda	s6, 0x0(s6)
	sll	s6, 0x20, s6
	ldah	s6, 0x0(s6)
	lda	s6, 0x0(s6)	
	ldq	s6, 0(s6)

	# skip result handling procedure
	lda	s6, SizeOfDispatchDesc(s6)

	# reload pointer to the beginning of the call list
	lda	s4, ARGUMENTS_FRAME(sp)

	# set result to 0
	bis	zero, zero, s5

pessimistic_hl_loop_head:
	# find the next handler to call or the end of the list
	ldq	t12, handlerOffset(s6)
	beq	t12, pessimistic_return
	ldq	t1, 0(s4)
	bne	t1, pessimistic_call_handler
	lda	s4, 8(s4)
	lda	s6, SizeOfHandlerDesc(s6)
	br	pessimistic_hl_loop_head

pessimistic_call_handler:
	# check for closure
	ldq	t1, useHandlerClosureOffset(s6)
	bne	t1, pessimistic_handler_closure

	# set the arguments in registers
	bis	s0, s0, a0
	bis	s1, s1, a1
	bis	s2, s2, a2
	bis	s3, s3, a3
	ldq	a4, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+16)(sp)
	ldq	a5, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+24)(sp)

	# set the arguments on stack
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+0)(sp)
	stq	t2, 0(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+8)(sp)
	stq	t2, 8(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+16)(sp)
	stq	t2, 16(sp)
 	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+24)(sp)
	stq	t2, 24(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+32)(sp)
	stq	t2, 32(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+40)(sp)
	stq	t2, 40(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+48)(sp)
	stq	t2, 48(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+56)(sp)
	stq	t2, 56(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+64)(sp)
	stq	t2, 64(sp)

pessimistic_pass_callee_saved_1:	
	lda	t2, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+0)(sp)
	bis	t2, t2, t3

	br	pessimistic_execute_handler

pessimistic_handler_closure:
	# set the arguments in registers
	ldq	a0, handlerClosureOffset(s6)
	bis	s0, s0, a1
	bis	s1, s1, a2
	bis	s2, s2, a3
	bis	s3, s3, a4
	ldq	a5, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+16)(sp)

	# copy arguments 5th through 15th
	ldq	t2, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+24)(sp)
	stq	t2, 0(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+0)(sp)
	stq	t2, 8(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+8)(sp)
	stq	t2, 16(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+16)(sp)
	stq	t2, 24(sp)
 	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+24)(sp)
	stq	t2, 32(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+32)(sp)
	stq	t2, 40(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+40)(sp)
	stq	t2, 48(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+48)(sp)
	stq	t2, 56(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+56)(sp)
	stq	t2, 64(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+64)(sp)
	stq	t2, 72(sp)

pessimistic_pass_callee_saved_2:	
	lda	t2, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+0)(sp)
	bis	t2, t2, t3

pessimistic_execute_handler:	
	# execute a handler
	jsr     ra, (t12), 0
pessimistic_handler_gp_patch:
	ldgp	gp, 0(ra)

pessimistic_hl_loop_tail:
	# check if it cancels
	ldq	t12, cancelOffset(s6)
	bne	t12, pessimistic_return

	# repeat
	lda	s4, 8(s4)
	lda	s6, SizeOfHandlerDesc(s6)
	br	pessimistic_hl_loop_head
	
	# return
pessimistic_return:
	bis	s5, s5, v0 /* result */
	ldq	s0, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+32)(sp)
	ldq	s1, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+40)(sp)
	ldq	s2, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+48)(sp)
	ldq	s3, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+56)(sp)
	ldq	s4, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+64)(sp)
	ldq	s5, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+72)(sp)
	ldq	s6, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+80)(sp)
	ldq	ra, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+0)(sp)
	lda	sp, PESSIMISTIC_STUB_FRAME_SIZE(sp)
	ret	zero, (ra), 1

pessimistic_no_handler:	
	lda	t1,  MI_DispatcherPrivate
	ldq	t12, RaiseNoHandler_OFFSET(t1)
	jsr	ra, (t12), 0

	/* never reached */
pessimistic_stub_end:	
	.end	pessimistic_stub

/* #endif SIRPA */

#ifdef SIRPA

/* FIRST CUT */

/******************************************************************************
 *	
 *				LEVEL 1
 *
 *****************************************************************************/

/* word size in bytes */
#define WORD_SIZE 8

/* these constants have to be synchronized manually with Dispatcher.m3 */
#define MaxNumOfHandlersForDebug	255
#define MaxNumOfArgumentsForDebug	15

/* offset to the procedure that raises NoHandlerInvoked exception */
#define RaiseNoHandler_OFFSET		608

/*
 * arguments:	
 *    1st - 4th  => s0 - s3
 *    5th - 6th  => (72, 8)(ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+72)(sp)
 *    7th - 15th => (0 - 64)(PESSIMISTIC_STUB_FRAME_SIZE+sp)
 */
	
/* size of the stack frame created by the pessimistic_stub */
#define REGISTERS_SAVE_FRAME	96
#define GUARD_RESULTS_FRAME	(MaxNumOfHandlersForDebug + 1) * WORD_SIZE

/* max number of args, plus closure, minus 4 args always kept in regs */
#define ARGUMENTS_FRAME		(MaxNumOfArgumentsForDebug + 1 - 4) * WORD_SIZE

#define PESSIMISTIC_STUB_FRAME_SIZE	(REGISTERS_SAVE_FRAME + \
					 GUARD_RESULTS_FRAME + \
					 ARGUMENTS_FRAME + 16)
	
	.globl	pessimistic_stub
	.globl	pessimistic_stub_end
	.globl  pessimistic_handlers_gl_long
	.globl  pessimistic_handlers_hl_long
	.globl	pessimistic_gp_patch
	.globl	pessimistic_profile_gp_patch
	.globl	pessimistic_guard_gp_patch
	.globl	pessimistic_handler_gp_patch
	.globl	pessimistic_pass_callee_saved_1
	.globl	pessimistic_pass_callee_saved_2
	.globl	pessimistic_inc_stack
	.globl	pessimistic_jmp_no_handler_1
	.globl	pessimistic_jmp_no_handler_2
	.globl	pessimistic_return
	
	.extern add_xxx_counts
	
	.ent	pessimistic_stub /* FIRST CUT */
	.mask	0x04000000, -PESSIMISTIC_STUB_FRAME_SIZE
	.frame	sp, PESSIMISTIC_STUB_FRAME_SIZE, ra, 48
	.prologue	1

pessimistic_stub:
pessimistic_gp_patch:
	ldgp	gp, 0(t12)

#ifdef CALL_GRAPH
	.set	noat
	bis	ra, 1, ra
	lda	$at, _mcount
	jsr	$at, ($at), _mcount	# Call profile routine
	.set	at
pessimistic_profile_gp_patch:
	ldgp	gp,0(pv)
#endif CALL_GRAPH
	
	bis	sp, sp, t1
	lda	sp, -PESSIMISTIC_STUB_FRAME_SIZE(sp)

	stq	ra, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+0)(sp)

	# store the new counter in the scratch location on the stack
	stq	zero, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+96)(sp)
	rpcc	t0
	zapnot	t0, 15, t0
	stq	t0, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+88)(sp)

	stq	t1, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+8)(sp)
	stq	a4, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+16)(sp)
	stq	a5, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+24)(sp)
	stq	s0, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+32)(sp)
	stq	s1, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+40)(sp)
	stq	s2, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+48)(sp)
	stq	s3, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+56)(sp)
	stq	s4, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+64)(sp)
	stq	s5, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+72)(sp)
	stq	s6, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+80)(sp)

	# save arguments
	bis	a0, a0, s0
	bis	a1, a1, s1
	bis	a2, a2, s2
	bis	a3, a3, s3

pessimistic_guard_loop:
	# load pointer to handler descriptor list from the alias descriptor
pessimistic_handlers_gl_long:
	ldah	s6, 0x0(zero)
	lda	s6, 0x0(s6)
	sll	s6, 0x20, s6
	ldah	s6, 0x0(s6)
	lda	s6, 0x0(s6)	
	ldq	s6, 0(s6)

	# skip result handling procedure
	lda	s6, SizeOfDispatchDesc(s6)

	# is there any handler?
	ldq	t12, handlerOffset(s6)
	bne	t12, skip_1
pessimistic_jmp_no_handler_1:	
	bsr	pessimistic_no_handler
skip_1:

	# pointer to the list of flags for handlers to be called
	lda	s4, ARGUMENTS_FRAME(sp)
	
	# set the count of handlers to be invoked
	bis	zero, zero, s5

pessimistic_gl_loop_head:
	# check for NIL guard
	ldq	t12, guardOffset(s6)
	bne	t12, pessimistic_call_guard
	lda	v0, 1(zero)
	stq	v0, 0(s4)
	br	pessimistic_guard_true

pessimistic_call_guard:	
	ldq	t1, useGuardClosureOffset(s6)
	bne	t1, pessimistic_guard_closure
	
	# set the arguments in registers
	bis	s0, s0, a0
	bis	s1, s1, a1
	bis	s2, s2, a2
	bis	s3, s3, a3
	ldq	a4, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+16)(sp)
	ldq	a5, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+24)(sp)

	# set the arguments on stack
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+0)(sp)
	stq	t2, 0(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+8)(sp)
	stq	t2, 8(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+16)(sp)
	stq	t2, 16(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+24)(sp)
	stq	t2, 24(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+32)(sp)
	stq	t2, 32(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+40)(sp)
	stq	t2, 40(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+48)(sp)
	stq	t2, 48(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+56)(sp)
	stq	t2, 56(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+64)(sp)
	stq	t2, 64(sp)

	br	pessimistic_execute_guard
	
pessimistic_guard_closure:		
	# set the arguments in registers
	ldq	a0, handlerClosureOffset(s6)
	bis	s0, s0, a1
	bis	s1, s1, a2
	bis	s2, s2, a3
	bis	s3, s3, a4
	ldq	a5, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+16)(sp)

	# copy arguments 5th through 15th
	ldq	t2, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+24)(sp)
	stq	t2, 0(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+0)(sp)
	stq	t2, 8(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+8)(sp)
	stq	t2, 16(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+16)(sp)
	stq	t2, 24(sp)
 	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+24)(sp)
	stq	t2, 32(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+32)(sp)
	stq	t2, 40(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+40)(sp)
	stq	t2, 48(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+48)(sp)
	stq	t2, 56(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+56)(sp)
	stq	t2, 64(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+64)(sp)
	stq	t2, 72(sp)

pessimistic_execute_guard:
	# compute the difference since the last time the counter read
	# store it in the second scratch location on the stack
	rpcc	t0
	zapnot	t0, 15, t0
	ldq	t1, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+88)(sp)
	subq	t0, t1, t0
	ldq	t1, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+96)(sp)
	cmplt	t1, zero, t2
	beq	t2, xxx1
	bis	zero, 1, t2
	sll	t2, 32, t2
	subq	t2, t0, t0
xxx1:	addq	t0, t1, t0
	stq	t0, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+96)(sp)

	# execute a guard
	jsr     ra, (t12), 0
pessimistic_guard_gp_patch:
	ldgp	gp, 0(ra)

	# store the new counter in the scratch location on the stack
	rpcc	t0
	zapnot	t0, 15, t0
	stq	t0, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+88)(sp)

	# save result
	stq	v0, 0(s4)
	
	# skip if false
	beq     v0, pessimistic_gl_loop_tail

pessimistic_guard_true:
	# increase the counter of handlers to be invoked
	lda	s5, 1(s5)

	# check if it cancels
	ldq	t12, cancelOffset(s6)
	bne	t12, pessimistic_handler_loop

pessimistic_gl_loop_tail:
	# check whether there is one more handler
	lda	s4, 8(s4)
	lda	s6, SizeOfHandlerDesc(s6)
	ldq	t12, handlerOffset(s6)
	beq	t12, pessimistic_handler_loop

	# repeat
	br	pessimistic_gl_loop_head
	
	# execute all handlers in the call list
pessimistic_handler_loop:
	# check whether there are any handlers called
	bne	s5, skip_2
pessimistic_jmp_no_handler_2:
	bsr	pessimistic_no_handler
skip_2:
	
	# reload pointers
pessimistic_handlers_hl_h:
pessimistic_handlers_hl_long:
	ldah	s6, 0x0(zero)
	lda	s6, 0x0(s6)
	sll	s6, 0x20, s6
	ldah	s6, 0x0(s6)
	lda	s6, 0x0(s6)	
	ldq	s6, 0(s6)

	# skip result handling procedure
	lda	s6, SizeOfDispatchDesc(s6)

	# reload pointer to the beginning of the call list
	lda	s4, ARGUMENTS_FRAME(sp)

	# set result to 0
	bis	zero, zero, s5

pessimistic_hl_loop_head:
	# find the next handler to call or the end of the list
	ldq	t12, handlerOffset(s6)
	beq	t12, pessimistic_return
	ldq	t1, 0(s4)
	bne	t1, pessimistic_call_handler
	lda	s4, 8(s4)
	lda	s6, SizeOfHandlerDesc(s6)
	br	pessimistic_hl_loop_head

pessimistic_call_handler:
	# check for closure
	ldq	t1, useHandlerClosureOffset(s6)
	bne	t1, pessimistic_handler_closure

	# set the arguments in registers
	bis	s0, s0, a0
	bis	s1, s1, a1
	bis	s2, s2, a2
	bis	s3, s3, a3
	ldq	a4, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+16)(sp)
	ldq	a5, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+24)(sp)

	# set the arguments on stack
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+0)(sp)
	stq	t2, 0(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+8)(sp)
	stq	t2, 8(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+16)(sp)
	stq	t2, 16(sp)
 	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+24)(sp)
	stq	t2, 24(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+32)(sp)
	stq	t2, 32(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+40)(sp)
	stq	t2, 40(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+48)(sp)
	stq	t2, 48(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+56)(sp)
	stq	t2, 56(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+64)(sp)
	stq	t2, 64(sp)

pessimistic_pass_callee_saved_1:	
	lda	t2, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+0)(sp)
	bis	t2, t2, t3

	br	pessimistic_execute_handler

pessimistic_handler_closure:
	# set the arguments in registers
	ldq	a0, handlerClosureOffset(s6)
	bis	s0, s0, a1
	bis	s1, s1, a2
	bis	s2, s2, a3
	bis	s3, s3, a4
	ldq	a5, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+16)(sp)

	# copy arguments 5th through 15th
	ldq	t2, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+24)(sp)
	stq	t2, 0(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+0)(sp)
	stq	t2, 8(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+8)(sp)
	stq	t2, 16(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+16)(sp)
	stq	t2, 24(sp)
 	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+24)(sp)
	stq	t2, 32(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+32)(sp)
	stq	t2, 40(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+40)(sp)
	stq	t2, 48(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+48)(sp)
	stq	t2, 56(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+56)(sp)
	stq	t2, 64(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+64)(sp)
	stq	t2, 72(sp)

pessimistic_pass_callee_saved_2:	
	lda	t2, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+0)(sp)
	bis	t2, t2, t3

pessimistic_execute_handler:	
	# compute the difference since the last time the counter read
	# store it in the second scratch location on the stack
	rpcc	t0
	zapnot	t0, 15, t0
	ldq	t1, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+88)(sp)
	subq	t0, t1, t0
	ldq	t1, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+96)(sp)
	cmplt	t1, zero, t2
	beq	t2, xxx2
	bis	zero, 1, t2
	sll	t2, 32, t2
	subq	t2, t0, t0
xxx2:	addq	t0, t1, t0
	stq	t0, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+96)(sp)

	# execute a handler
	jsr     ra, (t12), 0
pessimistic_handler_gp_patch:
	ldgp	gp, 0(ra)

	# store the new counter in the scratch location on the stack
	rpcc	t0
	zapnot	t0, 15, t0
	stq	t0, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+88)(sp)

	bis	v0, v0, s5 /* result */

pessimistic_hl_loop_tail:
	# check if it cancels
	ldq	t12, cancelOffset(s6)
	bne	t12, pessimistic_return

	# repeat
	lda	s4, 8(s4)
	lda	s6, SizeOfHandlerDesc(s6)
	br	pessimistic_hl_loop_head
	
	# return
pessimistic_return:
	ldq	s0, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+32)(sp)
	ldq	s1, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+40)(sp)
	ldq	s2, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+48)(sp)
	ldq	s3, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+56)(sp)
	ldq	s4, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+64)(sp)
	ldq	s6, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+80)(sp)

	# compute the difference since the last time the counter read
	# store it in the second scratch location on the stack
	rpcc	t0
	zapnot	t0, 15, t0
	ldq	t1, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+88)(sp)
	subq	t0, t1, t0
	ldq	t1, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+96)(sp)
	cmplt	t1, zero, t2
	beq	t2, xxx3
	bis	zero, 1, t2
	sll	t2, 32, t2
	subq	t2, t0, t0
xxx3:	addq	t0, t1, a0
	lda	t12, add_xxx_counts
	jsr	ra, (t12), 0

	bis	s5, s5, v0 /* result */
	ldq	s5, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+72)(sp)
	ldq	ra, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+0)(sp)
	lda	sp, PESSIMISTIC_STUB_FRAME_SIZE(sp)
	ret	zero, (ra), 1

pessimistic_no_handler:	
	lda	t1,  MI_DispatcherPrivate
	ldq	t12, RaiseNoHandler_OFFSET(t1)
	jsr	ra, (t12), 0

	/* never reached */
pessimistic_stub_end:	
	.end	pessimistic_stub

#endif SIRPA

/* SECOND CUT */

#ifdef SIRPA
	
/* word size in bytes */
#define WORD_SIZE 8

/* these constants have to be synchronized manually with Dispatcher.m3 */
#define MaxNumOfHandlersForDebug	255
#define MaxNumOfArgumentsForDebug	15

/* offset to the procedure that raises NoHandlerInvoked exception */
#define RaiseNoHandler_OFFSET		608

/*
 * arguments:	
 *    1st - 4th  => s0 - s3
 *    5th - 6th  => (72, 8)(ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+72)(sp)
 *    7th - 15th => (0 - 64)(PESSIMISTIC_STUB_FRAME_SIZE+sp)
 */
	
/* size of the stack frame created by the pessimistic_stub */
#define REGISTERS_SAVE_FRAME	96
#define GUARD_RESULTS_FRAME	(MaxNumOfHandlersForDebug + 1) * WORD_SIZE

/* max number of args, plus closure, minus 4 args always kept in regs */
#define ARGUMENTS_FRAME		(MaxNumOfArgumentsForDebug + 1 - 4) * WORD_SIZE

#define TRACE_EVENTS
#ifdef TRACE_EVENTS
#define TRACE_CALL_AR 0
#define TRACE_STORE_AR 32
#define TOTAL_COUNTER     (PESSIMISTIC_STUB_FRAME_SIZE - 8)
#define SCRATCH_LOCATION  (PESSIMISTIC_STUB_FRAME_SIZE - 16)
#define STUB_POINTER      (PESSIMISTIC_STUB_FRAME_SIZE - 24)
#else
#define TRACE_CALL_AR  0
#define TRACE_STORE_AR 0
#endif

#define PESSIMISTIC_STUB_FRAME_SIZE	(REGISTERS_SAVE_FRAME + \
					 GUARD_RESULTS_FRAME + \
					 ARGUMENTS_FRAME + \
					 TRACE_CALL_AR + \
					 TRACE_STORE_AR)
	
	.extern add_xxx_counts
	
	.globl	pessimistic_stub
	.globl	pessimistic_stub_end
	.globl  pessimistic_handlers_gl_long
	.globl  pessimistic_handlers_hl_long
	.globl	pessimistic_gp_patch
	.globl	pessimistic_profile_gp_patch
	.globl	pessimistic_guard_gp_patch
	.globl	pessimistic_handler_gp_patch
	.globl	pessimistic_pass_callee_saved_1
	.globl	pessimistic_pass_callee_saved_2
	.globl	pessimistic_inc_stack
	.globl	pessimistic_jmp_no_handler_1
	.globl	pessimistic_jmp_no_handler_2
	.globl	pessimistic_return
	
	.ent	pessimistic_stub /* SECOND CUT */
	.mask	0x04000000, -PESSIMISTIC_STUB_FRAME_SIZE
	.frame	sp, PESSIMISTIC_STUB_FRAME_SIZE, ra, 48
	.prologue	1

pessimistic_stub:
pessimistic_gp_patch:
	ldgp	gp, 0(t12)

#ifdef CALL_GRAPH
	.set	noat
	bis	ra, 1, ra
	lda	$at, _mcount
	jsr	$at, ($at), _mcount	# Call profile routine
	.set	at
pessimistic_profile_gp_patch:
	ldgp	gp,0(pv)
#endif CALL_GRAPH
	
	bis	sp, sp, t1
	lda	sp, -PESSIMISTIC_STUB_FRAME_SIZE(sp)

	stq	ra, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+0+TRACE_CALL_AR)(sp)

#ifdef TRACE_EVENTS	
	# store the new counter in the scratch location on the stack
	stq	zero, (TOTAL_COUNTER)(sp)
	stq	t12,  (STUB_POINTER)(sp)
	rpcc	t0
	zapnot	t0, 15, t0
	stq	t0, (SCRATCH_LOCATION)(sp)
#endif TRACE_EVENTS

	stq	t1, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+8+TRACE_CALL_AR)(sp)
	stq	a4, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+16+TRACE_CALL_AR)(sp)
	stq	a5, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+24+TRACE_CALL_AR)(sp)
	stq	s0, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+32+TRACE_CALL_AR)(sp)
	stq	s1, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+40+TRACE_CALL_AR)(sp)
	stq	s2, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+48+TRACE_CALL_AR)(sp)
	stq	s3, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+56+TRACE_CALL_AR)(sp)
	stq	s4, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+64+TRACE_CALL_AR)(sp)
	stq	s5, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+72+TRACE_CALL_AR)(sp)
	stq	s6, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+80+TRACE_CALL_AR)(sp)

	# save arguments
	bis	a0, a0, s0
	bis	a1, a1, s1
	bis	a2, a2, s2
	bis	a3, a3, s3

pessimistic_guard_loop:
	# load pointer to handler descriptor list from the alias descriptor
pessimistic_handlers_gl_long:
	ldah	s6, 0x0(zero)
	lda	s6, 0x0(s6)
	sll	s6, 0x20, s6
	ldah	s6, 0x0(s6)
	lda	s6, 0x0(s6)	
	ldq	s6, 0(s6)

	# skip result handling procedure
	lda	s6, SizeOfDispatchDesc(s6)

	# is there any handler?
	ldq	t12, handlerOffset(s6)
	bne	t12, skip_1
pessimistic_jmp_no_handler_1:	
	bsr	pessimistic_no_handler
skip_1:

	# pointer to the list of flags for handlers to be called
	lda	s4, ARGUMENTS_FRAME(sp)
	
	# set the count of handlers to be invoked
	bis	zero, zero, s5

pessimistic_gl_loop_head:
	# check for NIL guard
	ldq	t12, guardOffset(s6)
	bne	t12, pessimistic_call_guard
	lda	v0, 1(zero)
	stq	v0, 0(s4)
	br	pessimistic_guard_true

pessimistic_call_guard:	
	ldq	t1, useGuardClosureOffset(s6)
	bne	t1, pessimistic_guard_closure
	
	# set the arguments in registers
	bis	s0, s0, a0
	bis	s1, s1, a1
	bis	s2, s2, a2
	bis	s3, s3, a3
	ldq	a4, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+16+TRACE_CALL_AR)(sp)
	ldq	a5, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+24+TRACE_CALL_AR)(sp)

	# set the arguments on stack
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+0)(sp)
	stq	t2, 0(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+8)(sp)
	stq	t2, 8(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+16)(sp)
	stq	t2, 16(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+24)(sp)
	stq	t2, 24(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+32)(sp)
	stq	t2, 32(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+40)(sp)
	stq	t2, 40(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+48)(sp)
	stq	t2, 48(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+56)(sp)
	stq	t2, 56(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+64)(sp)
	stq	t2, 64(sp)

	br	pessimistic_execute_guard
	
pessimistic_guard_closure:		
	# set the arguments in registers
	ldq	a0, handlerClosureOffset(s6)
	bis	s0, s0, a1
	bis	s1, s1, a2
	bis	s2, s2, a3
	bis	s3, s3, a4
	ldq	a5, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+16+TRACE_CALL_AR)(sp)

	# copy arguments 5th through 15th
	ldq	t2, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+24+TRACE_CALL_AR)(sp)
	stq	t2, 0(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+0)(sp)
	stq	t2, 8(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+8)(sp)
	stq	t2, 16(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+16)(sp)
	stq	t2, 24(sp)
 	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+24)(sp)
	stq	t2, 32(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+32)(sp)
	stq	t2, 40(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+40)(sp)
	stq	t2, 48(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+48)(sp)
	stq	t2, 56(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+56)(sp)
	stq	t2, 64(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+64)(sp)
	stq	t2, 72(sp)

pessimistic_execute_guard:
#ifdef TRACE_EVENTS	
	# compute the difference since the last time the counter read
	# store it in the second scratch location on the stack
	rpcc	t0
	zapnot	t0, 15, t0
	ldq	t1, (SCRATCH_LOCATION)(sp)
	subq	t0, t1, t0
	ldq	t1, (TOTAL_COUNTER)(sp)
	cmplt	t1, zero, t2
	beq	t2, xxx1
	bis	zero, 1, t2
	sll	t2, 32, t2
	subq	t2, t0, t0
xxx1:	addq	t0, t1, t0
	stq	t0, (TOTAL_COUNTER)(sp)
#endif TRACE_EVENTS	

	# execute a guard
	jsr     ra, (t12), 0
pessimistic_guard_gp_patch:
	ldgp	gp, 0(ra)

#ifdef TRACE_EVENTS	
	# store the new counter in the scratch location on the stack
	rpcc	t0
	zapnot	t0, 15, t0
	stq	t0, (SCRATCH_LOCATION)(sp)
#endif TRACE_EVENTS	

	# save result
	stq	v0, 0(s4)
	
	# skip if false
	beq     v0, pessimistic_gl_loop_tail

pessimistic_guard_true:
	# increase the counter of handlers to be invoked
	lda	s5, 1(s5)

	# check if it cancels
	ldq	t12, cancelOffset(s6)
	bne	t12, pessimistic_handler_loop

pessimistic_gl_loop_tail:
	# check whether there is one more handler
	lda	s4, 8(s4)
	lda	s6, SizeOfHandlerDesc(s6)
	ldq	t12, handlerOffset(s6)
	beq	t12, pessimistic_handler_loop

	# repeat
	br	pessimistic_gl_loop_head
	
	# execute all handlers in the call list
pessimistic_handler_loop:
	# check whether there are any handlers called
	bne	s5, skip_2
pessimistic_jmp_no_handler_2:
	bsr	pessimistic_no_handler
skip_2:
	
	# reload pointers
pessimistic_handlers_hl_h:
pessimistic_handlers_hl_long:
	ldah	s6, 0x0(zero)
	lda	s6, 0x0(s6)
	sll	s6, 0x20, s6
	ldah	s6, 0x0(s6)
	lda	s6, 0x0(s6)	
	ldq	s6, 0(s6)

	# skip result handling procedure
	lda	s6, SizeOfDispatchDesc(s6)

	# reload pointer to the beginning of the call list
	lda	s4, ARGUMENTS_FRAME(sp)

	# set result to 0
	bis	zero, zero, s5

pessimistic_hl_loop_head:
	# find the next handler to call or the end of the list
	ldq	t12, handlerOffset(s6)
	beq	t12, pessimistic_return
	ldq	t1, 0(s4)
	bne	t1, pessimistic_call_handler
	lda	s4, 8(s4)
	lda	s6, SizeOfHandlerDesc(s6)
	br	pessimistic_hl_loop_head

pessimistic_call_handler:
	# check for closure
	ldq	t1, useHandlerClosureOffset(s6)
	bne	t1, pessimistic_handler_closure

	# set the arguments in registers
	bis	s0, s0, a0
	bis	s1, s1, a1
	bis	s2, s2, a2
	bis	s3, s3, a3
	ldq	a4, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+16+TRACE_CALL_AR)(sp)
	ldq	a5, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+24+TRACE_CALL_AR)(sp)

	# set the arguments on stack
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+0)(sp)
	stq	t2, 0(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+8)(sp)
	stq	t2, 8(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+16)(sp)
	stq	t2, 16(sp)
 	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+24)(sp)
	stq	t2, 24(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+32)(sp)
	stq	t2, 32(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+40)(sp)
	stq	t2, 40(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+48)(sp)
	stq	t2, 48(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+56)(sp)
	stq	t2, 56(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+64)(sp)
	stq	t2, 64(sp)

pessimistic_pass_callee_saved_1:	
	lda	t2, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+0+TRACE_CALL_AR)(sp)
	bis	t2, t2, t3

	br	pessimistic_execute_handler

pessimistic_handler_closure:
	# set the arguments in registers
	ldq	a0, handlerClosureOffset(s6)
	bis	s0, s0, a1
	bis	s1, s1, a2
	bis	s2, s2, a3
	bis	s3, s3, a4
	ldq	a5, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+16+TRACE_CALL_AR)(sp)

	# copy arguments 5th through 15th
	ldq	t2, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+24+TRACE_CALL_AR)(sp)
	stq	t2, 0(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+0)(sp)
	stq	t2, 8(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+8)(sp)
	stq	t2, 16(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+16)(sp)
	stq	t2, 24(sp)
 	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+24)(sp)
	stq	t2, 32(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+32)(sp)
	stq	t2, 40(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+40)(sp)
	stq	t2, 48(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+48)(sp)
	stq	t2, 56(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+56)(sp)
	stq	t2, 64(sp)
	ldq	t2, (PESSIMISTIC_STUB_FRAME_SIZE+64)(sp)
	stq	t2, 72(sp)

pessimistic_pass_callee_saved_2:	
	lda	t2, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+0+TRACE_CALL_AR)(sp)
	bis	t2, t2, t3

pessimistic_execute_handler:	
#ifdef TRACE_EVENTS	
	# compute the difference since the last time the counter read
	# store it in the second scratch location on the stack
	rpcc	t0
	zapnot	t0, 15, t0
	ldq	t1, (SCRATCH_LOCATION)(sp)
	subq	t0, t1, t0
	ldq	t1, (TOTAL_COUNTER)(sp)
	cmplt	t1, zero, t2
	beq	t2, xxx2
	bis	zero, 1, t2
	sll	t2, 32, t2
	subq	t2, t0, t0
xxx2:	addq	t0, t1, t0
	stq	t0, (TOTAL_COUNTER)(sp)
#endif TRACE_EVENTS	

	# execute a handler
	jsr     ra, (t12), 0
pessimistic_handler_gp_patch:
	ldgp	gp, 0(ra)

	# store the new counter in the scratch location on the stack
#ifdef TRACE_EVENTS	
	rpcc	t0
	zapnot	t0, 15, t0
	stq	t0, (SCRATCH_LOCATION)(sp)
#endif TRACE_EVENTS	

	bis	v0, v0, s5 /* result */

pessimistic_hl_loop_tail:
	# check if it cancels
	ldq	t12, cancelOffset(s6)
	bne	t12, pessimistic_return

	# repeat
	lda	s4, 8(s4)
	lda	s6, SizeOfHandlerDesc(s6)
	br	pessimistic_hl_loop_head
	
	# return
pessimistic_return:
	bis	s5, s5, v0 /* result */
	ldq	s0, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+32+TRACE_CALL_AR)(sp)
	ldq	s1, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+40+TRACE_CALL_AR)(sp)
	ldq	s2, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+48+TRACE_CALL_AR)(sp)
	ldq	s3, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+56+TRACE_CALL_AR)(sp)
	ldq	s4, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+64+TRACE_CALL_AR)(sp)
	ldq	s5, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+72+TRACE_CALL_AR)(sp)
	ldq	s6, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+80+TRACE_CALL_AR)(sp)
	ldq	ra, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+0+TRACE_CALL_AR)(sp)
	lda	sp, PESSIMISTIC_STUB_FRAME_SIZE(sp)

	# compute the difference since the last time the counter read
	# call the dispatcher tracking code to atomically update the
	# trace state of an event
#ifdef TRACE_EVENTS
	# revert some state to make a correct call and return (see the below)
	bis	v0, v0, s5
	lda	sp, -PESSIMISTIC_STUB_FRAME_SIZE(sp)
	
	rpcc	t0
	zapnot	t0, 15, t0
	ldq	t1, (SCRATCH_LOCATION)(sp)
	subq	t0, t1, t0
	ldq	t1, (TOTAL_COUNTER)(sp)
	cmplt	t1, zero, t2
	beq	t2, xxx3
	bis	zero, 1, t2
	sll	t2, 32, t2
	subq	t2, t0, t0
xxx3:	addq	t0, t1, a0
	ldq	a1, (STUB_POINTER)(sp)
	
	# call the atomic update function
	lda	t12, add_xxx_counts
	jsr	ra, (t12), 0

	# we need to redo this code because that state was garbled
	# by the call to the counting routine, we do it twice so
	# that the last rpcc takes its execution into acccount
	bis	s5, s5, v0 /* result */
	ldq	s5, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+72+TRACE_CALL_AR)(sp)
	ldq	ra, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+0+TRACE_CALL_AR)(sp)
	lda	sp, PESSIMISTIC_STUB_FRAME_SIZE(sp)
#endif TRACE_EVENTS
	
	ret	zero, (ra), 1

pessimistic_no_handler:	
	lda	t1,  MI_DispatcherPrivate
	ldq	t12, RaiseNoHandler_OFFSET(t1)
	jsr	ra, (t12), 0

	/* never reached */
pessimistic_stub_end:	
	.end	pessimistic_stub

#endif

/* PESSIMISTIC_1 */
 	
/* size of the stack frame created by the pessimistic_stub */
#define REGISTERS_SAVE_FRAME_1	96
#define GUARD_RESULTS_FRAME_1	0

/* max number of args, plus closure, minus 4 args always kept in regs */
#define ARGUMENTS_FRAME_1	(MaxNumOfArgumentsForDebug + 1 - 4) * WORD_SIZE

#define PESSIMISTIC_1_STUB_FRAME_SIZE	(REGISTERS_SAVE_FRAME_1 + \
					 GUARD_RESULTS_FRAME_1 + \
					 ARGUMENTS_FRAME_1)
	
	.globl	pessimistic_1_stub
	.globl	pessimistic_1_stub_end
#ifdef SHORT
	.globl  pessimistic_1_handlers_gl_l
	.globl  pessimistic_1_handlers_gl_h
	.globl  pessimistic_1_handlers_hl_l
	.globl  pessimistic_1_handlers_hl_h
#endif SHORT
#ifdef LONG		
	.globl  pessimistic_1_handlers_gl_long
	.globl  pessimistic_1_handlers_hl_long
#endif LONG
	.globl	pessimistic_1_gp_patch
	.globl	pessimistic_1_profile_gp_patch
	.globl	pessimistic_1_guard_gp_patch
	.globl	pessimistic_1_handler_gp_patch
	.globl	pessimistic_1_pass_callee_saved_1
	.globl	pessimistic_1_copy_callee_saved_1	
	.globl	pessimistic_1_pass_callee_saved_2
	.globl	pessimistic_1_copy_callee_saved_2	
	.globl	pessimistic_1_passed_callee_saved
	.globl	pessimistic_1_inc_stack
	
	.ent	pessimistic_1_stub
	.mask	0x04000000, -PESSIMISTIC_1_STUB_FRAME_SIZE
	.frame	sp, PESSIMISTIC_1_STUB_FRAME_SIZE, ra, 48
	.prologue	1

pessimistic_1_stub:
pessimistic_1_gp_patch:
	ldgp	gp, 0(t12)

#ifdef CALL_GRAPH
	.set	noat
	bis	ra, 1, ra
	lda	$at, _mcount
	jsr	$at, ($at), _mcount	# Call profile routine
	.set	at
pessimistic_1_profile_gp_patch:
	ldgp	gp,0(pv)
#endif CALL_GRAPH
	
	bis	sp, sp, t1
	lda	sp, -PESSIMISTIC_1_STUB_FRAME_SIZE(sp)

	stq	ra, (ARGUMENTS_FRAME_1+GUARD_RESULTS_FRAME_1+0)(sp)
	stq	a4, (ARGUMENTS_FRAME_1+GUARD_RESULTS_FRAME_1+8)(sp)
	stq	a5, (ARGUMENTS_FRAME_1+GUARD_RESULTS_FRAME_1+16)(sp)

	# save callee-saved registers
	stq	s0, (ARGUMENTS_FRAME_1+GUARD_RESULTS_FRAME_1+24)(sp)
	stq	s1, (ARGUMENTS_FRAME_1+GUARD_RESULTS_FRAME_1+32)(sp)
	stq	s2, (ARGUMENTS_FRAME_1+GUARD_RESULTS_FRAME_1+40)(sp)
	stq	s3, (ARGUMENTS_FRAME_1+GUARD_RESULTS_FRAME_1+48)(sp)
	stq	s4, (ARGUMENTS_FRAME_1+GUARD_RESULTS_FRAME_1+56)(sp)
	stq	s5, (ARGUMENTS_FRAME_1+GUARD_RESULTS_FRAME_1+64)(sp)
	stq	s6, (ARGUMENTS_FRAME_1+GUARD_RESULTS_FRAME_1+72)(sp)
	stq	t1, (ARGUMENTS_FRAME_1+GUARD_RESULTS_FRAME_1+80)(sp)

	# save arguments
	bis	a0, a0, s0
	bis	a1, a1, s1
	bis	a2, a2, s2
	bis	a3, a3, s3

pessimistic_1_guard_loop:
	# load pointer to handler descriptor list from the alias descriptor
#ifdef SHORT
pessimistic_1_handlers_gl_h:
	ldah	s6, 0x0(gp)
pessimistic_1_handlers_gl_l:
	lda	s6, 0x0(s6)
#endif SHORT	
#ifdef LONG
pessimistic_1_handlers_gl_long:
	ldah	s6, 0x0(zero)
	lda	s6, 0x0(s6)
	sll	s6, 0x20, s6
	ldah	s6, 0x0(s6)
	lda	s6, 0x0(s6)	
#endif LONG
	ldq	s6, 0(s6)

	# skip result handling procedure
	lda	s6, SizeOfDispatchDesc(s6)

	# is there any handler?
	ldq	t12, handlerOffset(s6)
	bne	t12, skip_3
	bsr	pessimistic_1_no_handler
skip_3:

	# pointer to the list of flags for handlers to be called
	lda	s4, 1(zero)
	
	# set the count of handlers to be invoked
	bis	zero, zero, s5

pessimistic_1_gl_loop_head:
	# check for NIL guard
	ldq	t12, guardOffset(s6)
	bne	t12, pessimistic_1_call_guard
	br	pessimistic_1_guard_true

pessimistic_1_call_guard:	
	ldq	t1, useGuardClosureOffset(s6)
	bne	t1, pessimistic_1_guard_closure
	
	# set the arguments in registers
	bis	s0, s0, a0
	bis	s1, s1, a1
	bis	s2, s2, a2
	bis	s3, s3, a3
	ldq	a4, (ARGUMENTS_FRAME_1+GUARD_RESULTS_FRAME_1+8)(sp)
	ldq	a5, (ARGUMENTS_FRAME_1+GUARD_RESULTS_FRAME_1+16)(sp)

	# set the arguments on stack
	ldq	t2, (PESSIMISTIC_1_STUB_FRAME_SIZE+0)(sp)
	stq	t2, 0(sp)
	ldq	t2, (PESSIMISTIC_1_STUB_FRAME_SIZE+8)(sp)
	stq	t2, 8(sp)
	ldq	t2, (PESSIMISTIC_1_STUB_FRAME_SIZE+16)(sp)
	stq	t2, 16(sp)
	ldq	t2, (PESSIMISTIC_1_STUB_FRAME_SIZE+24)(sp)
	stq	t2, 24(sp)
	ldq	t2, (PESSIMISTIC_1_STUB_FRAME_SIZE+32)(sp)
	stq	t2, 32(sp)
	ldq	t2, (PESSIMISTIC_1_STUB_FRAME_SIZE+40)(sp)
	stq	t2, 40(sp)
	ldq	t2, (PESSIMISTIC_1_STUB_FRAME_SIZE+48)(sp)
	stq	t2, 48(sp)
	ldq	t2, (PESSIMISTIC_1_STUB_FRAME_SIZE+56)(sp)
	stq	t2, 56(sp)
	ldq	t2, (PESSIMISTIC_1_STUB_FRAME_SIZE+64)(sp)
	stq	t2, 64(sp)

	br	pessimistic_1_execute_guard
	
pessimistic_1_guard_closure:		
	# set the arguments in registers
	ldq	a0, handlerClosureOffset(s6)
	bis	s0, s0, a1
	bis	s1, s1, a2
	bis	s2, s2, a3
	bis	s3, s3, a4
	ldq	a5, (ARGUMENTS_FRAME_1+GUARD_RESULTS_FRAME_1+8)(sp)

	# copy arguments 5th through 15th
	ldq	t2, (ARGUMENTS_FRAME_1+GUARD_RESULTS_FRAME_1+16)(sp)
	stq	t2, 0(sp)
	ldq	t2, (PESSIMISTIC_1_STUB_FRAME_SIZE+0)(sp)
	stq	t2, 8(sp)
	ldq	t2, (PESSIMISTIC_1_STUB_FRAME_SIZE+8)(sp)
	stq	t2, 16(sp)
	ldq	t2, (PESSIMISTIC_1_STUB_FRAME_SIZE+16)(sp)
	stq	t2, 24(sp)
 	ldq	t2, (PESSIMISTIC_1_STUB_FRAME_SIZE+24)(sp)
	stq	t2, 32(sp)
	ldq	t2, (PESSIMISTIC_1_STUB_FRAME_SIZE+32)(sp)
	stq	t2, 40(sp)
	ldq	t2, (PESSIMISTIC_1_STUB_FRAME_SIZE+40)(sp)
	stq	t2, 48(sp)
	ldq	t2, (PESSIMISTIC_1_STUB_FRAME_SIZE+48)(sp)
	stq	t2, 56(sp)
	ldq	t2, (PESSIMISTIC_1_STUB_FRAME_SIZE+56)(sp)
	stq	t2, 64(sp)
	ldq	t2, (PESSIMISTIC_1_STUB_FRAME_SIZE+64)(sp)
	stq	t2, 72(sp)

pessimistic_1_execute_guard:
	# execute a guard
	jsr     ra, (t12), 0
pessimistic_1_guard_gp_patch:
	ldgp	gp, 0(ra)

	# skip if false
	beq     v0, pessimistic_1_gl_loop_tail

pessimistic_1_guard_true:
	# increase the counter of handlers to be invoked
	bis	s4, s5, s5

	# check if it cancels
	ldq	t12, cancelOffset(s6)
	bne	t12, pessimistic_1_handler_loop

pessimistic_1_gl_loop_tail:
	# check whether there is one more handler
	sll	s4, 1, s4
	lda	s6, SizeOfHandlerDesc(s6)
	ldq	t12, handlerOffset(s6)
	beq	t12, pessimistic_1_handler_loop

	# repeat
	br	pessimistic_1_gl_loop_head
	
	# execute all handlers in the call list
pessimistic_1_handler_loop:
	# check whether there are any handlers called
	bne	s5, skip_4
	bsr	pessimistic_1_no_handler
skip_4:	

	# reload pointers
#ifdef SHORT
pessimistic_1_handlers_hl_h:
	ldah	s6, 0x0(gp)
pessimistic_1_handlers_hl_l:
	lda	s6, 0x0(s6)
#endif SHORT	
#ifdef LONG
pessimistic_1_handlers_hl_long:
	ldah	s6, 0x0(zero)
	lda	s6, 0x0(s6)
	sll	s6, 0x20, s6
	ldah	s6, 0x0(s6)
	lda	s6, 0x0(s6)	
#endif LONG
	ldq	s6, 0(s6)

	# skip result handling procedure
	lda	s6, SizeOfDispatchDesc(s6)

	# set result to 0
	bis	zero, zero, s4

pessimistic_1_hl_loop_head:
	# find the next handler to call or the end of the list
	ldq	t12, handlerOffset(s6)
	beq	t12, pessimistic_1_return
	and	s5, 1, t1
	bne	t1, pessimistic_1_call_handler
	srl	s5, 1, s5
	lda	s6, SizeOfHandlerDesc(s6)
	br	pessimistic_1_hl_loop_head

pessimistic_1_call_handler:
	# check for closure
	ldq	t1, useHandlerClosureOffset(s6)
	bne	t1, pessimistic_1_handler_closure

	# set the arguments in registers
	bis	s0, s0, a0
	bis	s1, s1, a1
	bis	s2, s2, a2
	bis	s3, s3, a3
	ldq	a4, (ARGUMENTS_FRAME_1+GUARD_RESULTS_FRAME_1+8)(sp)
	ldq	a5, (ARGUMENTS_FRAME_1+GUARD_RESULTS_FRAME_1+16)(sp)

	# set the arguments on stack
	ldq	t2, (PESSIMISTIC_1_STUB_FRAME_SIZE+0)(sp)
	stq	t2, 0(sp)
	ldq	t2, (PESSIMISTIC_1_STUB_FRAME_SIZE+8)(sp)
	stq	t2, 8(sp)
	ldq	t2, (PESSIMISTIC_1_STUB_FRAME_SIZE+16)(sp)
	stq	t2, 16(sp)
 	ldq	t2, (PESSIMISTIC_1_STUB_FRAME_SIZE+24)(sp)
	stq	t2, 24(sp)
	ldq	t2, (PESSIMISTIC_1_STUB_FRAME_SIZE+32)(sp)
	stq	t2, 32(sp)
	ldq	t2, (PESSIMISTIC_1_STUB_FRAME_SIZE+40)(sp)
	stq	t2, 40(sp)
	ldq	t2, (PESSIMISTIC_1_STUB_FRAME_SIZE+48)(sp)
	stq	t2, 48(sp)
	ldq	t2, (PESSIMISTIC_1_STUB_FRAME_SIZE+56)(sp)
	stq	t2, 56(sp)
	ldq	t2, (PESSIMISTIC_1_STUB_FRAME_SIZE+64)(sp)
	stq	t2, 64(sp)

	# should we pass callee-saved registers?
pessimistic_1_pass_callee_saved_1:	
	lda	t1, 0(zero)
	beq	t1, pessimistic_1_execute_handler

	# pass callee-saved registers (s0-s6, sp, pc)
pessimistic_1_copy_callee_saved_1:
	lda	sp, 0(sp)
	ldq	t1, (ARGUMENTS_FRAME_1+GUARD_RESULTS_FRAME_1+24)(sp)
	bis	t0, t1, t2
	ldq	t1, (ARGUMENTS_FRAME_1+GUARD_RESULTS_FRAME_1+32)(sp)
	bis	t0, t1, t2
	ldq	t1, (ARGUMENTS_FRAME_1+GUARD_RESULTS_FRAME_1+40)(sp)
	bis	t0, t1, t2
	ldq	t1, (ARGUMENTS_FRAME_1+GUARD_RESULTS_FRAME_1+48)(sp)
	bis	t0, t1, t2
	ldq	t1, (ARGUMENTS_FRAME_1+GUARD_RESULTS_FRAME_1+56)(sp)
	bis	t0, t1, t2
	ldq	t1, (ARGUMENTS_FRAME_1+GUARD_RESULTS_FRAME_1+64)(sp)
	bis	t0, t1, t2
	ldq	t1, (ARGUMENTS_FRAME_1+GUARD_RESULTS_FRAME_1+72)(sp)
	bis	t0, t1, t2
	ldq	t1, (ARGUMENTS_FRAME_1+GUARD_RESULTS_FRAME_1+80)(sp)
	bis	t0, t1, t2
	ldq	t1, (ARGUMENTS_FRAME_1+GUARD_RESULTS_FRAME_1+0)(sp)
	bis	t0, t1, t2

	br	pessimistic_1_execute_handler

pessimistic_1_handler_closure:
	# set the arguments in registers
	ldq	a0, handlerClosureOffset(s6)
	bis	s0, s0, a1
	bis	s1, s1, a2
	bis	s2, s2, a3
	bis	s3, s3, a4
	ldq	a5, (ARGUMENTS_FRAME_1+GUARD_RESULTS_FRAME_1+8)(sp)

	# copy arguments 5th through 15th
	ldq	t2, (ARGUMENTS_FRAME_1+GUARD_RESULTS_FRAME_1+16)(sp)
	stq	t2, 0(sp)
	ldq	t2, (PESSIMISTIC_1_STUB_FRAME_SIZE+0)(sp)
	stq	t2, 8(sp)
	ldq	t2, (PESSIMISTIC_1_STUB_FRAME_SIZE+8)(sp)
	stq	t2, 16(sp)
	ldq	t2, (PESSIMISTIC_1_STUB_FRAME_SIZE+16)(sp)
	stq	t2, 24(sp)
 	ldq	t2, (PESSIMISTIC_1_STUB_FRAME_SIZE+24)(sp)
	stq	t2, 32(sp)
	ldq	t2, (PESSIMISTIC_1_STUB_FRAME_SIZE+32)(sp)
	stq	t2, 40(sp)
	ldq	t2, (PESSIMISTIC_1_STUB_FRAME_SIZE+40)(sp)
	stq	t2, 48(sp)
	ldq	t2, (PESSIMISTIC_1_STUB_FRAME_SIZE+48)(sp)
	stq	t2, 56(sp)
	ldq	t2, (PESSIMISTIC_1_STUB_FRAME_SIZE+56)(sp)
	stq	t2, 64(sp)
	ldq	t2, (PESSIMISTIC_1_STUB_FRAME_SIZE+64)(sp)
	stq	t2, 72(sp)

	# should we pass callee-saved registers?
pessimistic_1_pass_callee_saved_2:
	lda	t1, 0(zero)
	beq	t1, pessimistic_1_execute_handler

	# pass callee-saved registers (s0-s6, sp, pc)
pessimistic_1_copy_callee_saved_2:
	lda	sp, 0(sp)
	ldq	t1, (ARGUMENTS_FRAME_1+GUARD_RESULTS_FRAME_1+24)(sp)
	bis	t0, t1, t2
	ldq	t1, (ARGUMENTS_FRAME_1+GUARD_RESULTS_FRAME_1+32)(sp)
	bis	t0, t1, t2
	ldq	t1, (ARGUMENTS_FRAME_1+GUARD_RESULTS_FRAME_1+40)(sp)
	bis	t0, t1, t2
	ldq	t1, (ARGUMENTS_FRAME_1+GUARD_RESULTS_FRAME_1+48)(sp)
	bis	t0, t1, t2
	ldq	t1, (ARGUMENTS_FRAME_1+GUARD_RESULTS_FRAME_1+56)(sp)
	bis	t0, t1, t2
	ldq	t1, (ARGUMENTS_FRAME_1+GUARD_RESULTS_FRAME_1+64)(sp)
	bis	t0, t1, t2
	ldq	t1, (ARGUMENTS_FRAME_1+GUARD_RESULTS_FRAME_1+72)(sp)
	bis	t0, t1, t2
	ldq	t1, (ARGUMENTS_FRAME_1+GUARD_RESULTS_FRAME_1+80)(sp)
	bis	t0, t1, t2
	ldq	t1, (ARGUMENTS_FRAME_1+GUARD_RESULTS_FRAME_1+0)(sp)
	bis	t0, t1, t2

pessimistic_1_execute_handler:	
	# execute a handler
	jsr     ra, (t12), 0
pessimistic_1_handler_gp_patch:
	ldgp	gp, 0(ra)
	bis	v0, v0, s4 /* result */

	# have we passed callee-saved registers?
pessimistic_1_passed_callee_saved:	
	lda	t1, 0(zero)
	beq	t1, pessimistic_1_hl_loop_tail

	# readjust the stack pointer
pessimistic_1_inc_stack:
	lda	sp, 32(sp)

pessimistic_1_hl_loop_tail:
	# check if it cancels
	ldq	t12, cancelOffset(s6)
	bne	t12, pessimistic_1_return

	# repeat
	lda	s6, SizeOfHandlerDesc(s6)
	br	pessimistic_1_hl_loop_head
	
	# return
pessimistic_1_return:
	bis	s4, s4, v0 /* result */
	ldq	s0, (ARGUMENTS_FRAME_1+GUARD_RESULTS_FRAME_1+24)(sp)
	ldq	s1, (ARGUMENTS_FRAME_1+GUARD_RESULTS_FRAME_1+32)(sp)
	ldq	s2, (ARGUMENTS_FRAME_1+GUARD_RESULTS_FRAME_1+40)(sp)
	ldq	s3, (ARGUMENTS_FRAME_1+GUARD_RESULTS_FRAME_1+48)(sp)
	ldq	s4, (ARGUMENTS_FRAME_1+GUARD_RESULTS_FRAME_1+56)(sp)
	ldq	s5, (ARGUMENTS_FRAME_1+GUARD_RESULTS_FRAME_1+64)(sp)
	ldq	s6, (ARGUMENTS_FRAME_1+GUARD_RESULTS_FRAME_1+72)(sp)
	ldq	ra, (ARGUMENTS_FRAME_1+GUARD_RESULTS_FRAME_1+0)(sp)
	lda	sp, PESSIMISTIC_1_STUB_FRAME_SIZE(sp)
	ret	zero, (ra), 1

pessimistic_1_no_handler:	
	lda	t1,  MI_DispatcherPrivate
	ldq	t12, RaiseNoHandler_OFFSET(t1)
	jsr	ra, (t12), 0

	/* never reached */
pessimistic_1_stub_end:	
	.end	pessimistic_1_stub

/******************************************************************************
 *	
 *				LEVEL 2
 *
 *****************************************************************************/

/* counter of handlers to be invoked */	
#define XXXX	s6
#define YYYY    s5

#define DispatchListL2		s4

#define FinalResultL2		s6
#define CallFlagsIdxL2		s6
#define CallFlagsL2		s5

#define FinalResultL2MH		s5
#define CallFlagsPtrL2MH	s6
#define CallCntL2MH		s5
	
	.globl	snippet_prolog_2 
	.globl	snippet_prolog_2_end 
	.globl	snippet_save_callee
	.globl	snippet_save_callee_end
	.globl	snippet_save_regs 
	.globl	snippet_save_regs_end 
	.globl	snippet_load_desc 
	.globl	snippet_load_desc_end 
	.globl	snippet_start_up
	.globl	snippet_start_up_end
	.globl	snippet_start_up_mh
	.globl	snippet_start_up_mh_end
	.globl	snippet_gl_loop_head
	.globl	snippet_gl_loop_head_end
	.globl	snippet_gl_loop_head_mh
	.globl	snippet_gl_loop_head_mh_end
	.globl	snippet_call_guard
	.globl	snippet_call_guard_end
	.globl	snippet_call_guard_mh
	.globl	snippet_call_guard_mh_end
	.globl	snippet_handler_loop
	.globl	snippet_handler_loop_end
	.globl	snippet_handler_loop_mh
	.globl	snippet_handler_loop_mh_end
	.globl	snippet_call_handler
	.globl	snippet_call_handler_end
	.globl	snippet_call_handler_mh
	.globl	snippet_call_handler_mh_end
	.globl	snippet_hl_loop_head
	.globl	snippet_hl_loop_head_end
	.globl	snippet_hl_loop_head_mh
	.globl	snippet_hl_loop_head_mh_end
	.globl	snippet_hl_loop_tail
	.globl	snippet_hl_loop_tail_end
	.globl	snippet_hl_loop_tail_mh
	.globl	snippet_hl_loop_tail_mh_end
	.globl	snippet_epilog_g
	.globl	snippet_epilog_g_end
	.globl	snippet_epilog_mh
	.globl	snippet_epilog_mh_end
	.globl	snippet_no_handler
	.globl	snippet_no_handler_end
	.globl	snippet_no_handler_gp
	.globl	snippet_no_handler_gp_end
	.globl  snippet_pass_callee_saved
	.globl	snippet_pass_callee_saved_end
	.globl	snippet_adjust_sp
	.globl	snippet_adjust_sp_end
	.globl	snippet_check_closure
	.globl	snippet_check_closure_end
	.globl	snippet_check_no_closure
	.globl	snippet_check_no_closure_end
	.globl	snippet_branch_closure
	.globl	snippet_branch_closure_end
	
	.ent	snippet_code
snippet_code:	
		
	.globl	snippet_prolog 
	.globl	snippet_profile_gp_patch 
	.globl	snippet_prolog_end 

	# procedure prolog
snippet_prolog:
	ldgp	gp, 0(t12)

#ifdef CALL_GRAPH
	.set	noat
	bis	ra, 1, ra
	ldah	$at, 0(zero)
	lda	$at, 0($at)
	sll	$at, 32, $at
	ldah	$at, 0($at)
	lda	$at, 0($at)
	jsr	$at, ($at), _mcount	# Call profile routine
	.set	at
snippet_profile_gp_patch:
	ldgp	gp,0(pv)
#endif CALL_GRAPH

	bis	sp, sp, t1
	lda	sp, 1(sp)	# patch stack frame size
	stq	ra, 2(sp)	# patch offset to ra
	
snippet_prolog_end:

	.globl	snippet_prolog_1
	.globl	snippet_profile_gp_patch_1
	.globl	snippet_prolog_1_end 

snippet_prolog_1:
	ldgp	gp, 0(t12)

#ifdef CALL_GRAPH
	.set	noat
	bis	ra, 1, ra
	ldah	$at, 0(zero)
	lda	$at, 0($at)
	sll	$at, 32, $at
	ldah	$at, 0($at)
	lda	$at, 0($at)
	jsr	$at, ($at), _mcount	# Call profile routine
	.set	at
snippet_profile_gp_patch_1:
	ldgp	gp,0(pv)
#endif CALL_GRAPH

	lda	sp, 1(sp)	# patch stack frame size
	stq	ra, 2(sp)	# patch offset to ra
snippet_prolog_1_end:

snippet_prolog_2:
	bis	t12, t12, gp
	lda	sp, 1(sp)	# patch stack frame size
	stq	ra, 2(sp)	# patch offset to ra
snippet_prolog_2_end:


	# save callee-saved registers
snippet_save_callee:	
	stq	s0, 3(sp)	# patch offset for the register
	stq	s1, 4(sp)	# patch offset for the register
	stq	s2, 5(sp)	# patch offset for the register
	stq	s3, 6(sp)	# patch offset for the register
	stq	s4, 7(sp)	# patch offset for the register
	stq	s5, 8(sp)	# patch offset for the register
	stq	s6, 9(sp)	# patch offset for the register
	stq	t1, 10(sp)	# patch offset for the register
snippet_save_callee_end:
	
	# save arguments passed in registers
snippet_save_regs:		
	bis	a0, a0, s0
	bis	a1, a1, s1
	bis	a2, a2, s2
	bis	a3, a3, s3
	stq	a4, 0(sp)	# patch offset for the register
	stq	a5, 1(sp)	# patch offset for the register
snippet_save_regs_end:	
snippet_save_regs_2:		
	bis	a0, a0, s0
	bis	a1, a1, s1
	bis	a2, a2, s2
	bis	a3, a3, s3
	stq	a5, 1(sp)	# patch offset for the register
snippet_save_regs_end_2:	
	
	# load pointer to handler descriptor list from the alias descriptor
#ifdef SHORT
snippet_load_desc:
	ldah	DispatchListL2, 0x0(gp)	# patch pointer to the alias descriptor
	lda	DispatchListL2, 0x0(DispatchListL2)	# patch pointer to the alias descriptor
	ldq	DispatchListL2, 0(DispatchListL2)

	# skip result handling procedure
	lda	DispatchListL2, resultHanldingOffset(DispatchListL2)
snippet_load_desc_end:
#endif SHORT
#ifdef LONG
snippet_load_desc:		
	ldah	DispatchListL2, 0x0(zero)
	lda	DispatchListL2, 0x0(DispatchListL2)
	sll	DispatchListL2, 0x20, DispatchListL2
	ldah	DispatchListL2, 0x0(DispatchListL2)
	lda	DispatchListL2, 0x0(DispatchListL2)	
	ldq	DispatchListL2, 0(DispatchListL2)

	# skip result handling procedure
	lda	DispatchListL2, SizeOfDispatchDesc(DispatchListL2)
snippet_load_desc_end:
#endif LONG
			
	# start-up code
	# check whether there is any handler, set the pointer to the 
	# call flags list and set the count of handlers to be invoked
snippet_start_up:		
	ldq	t12, handlerOffset(DispatchListL2)
	bne	t12, skip_5
	bsr	snippet_no_handler		# patch branch
skip_5:	
	lda	CallFlagsIdxL2, 1(zero)
	bis	zero, zero, CallFlagsL2
snippet_start_up_end:	

snippet_start_up_mh:
	ldq	t12, handlerOffset(DispatchListL2)
	bne	t12, skip_6
	bsr	snippet_no_handler	# patch branch
skip_6:	
	lda	CallFlagsPtrL2MH, 0(sp)	# patch offset to call flag frame
	bis	zero, zero, CallCntL2MH
snippet_start_up_mh_end:

	# head of the guard loop
	# check for NIL guard and for closure
snippet_gl_loop_head:
	ldq	t12, guardOffset(DispatchListL2)
	bne	t12, snippet_check_no_closure	# patch branch
	br	snippet_guard_true		# patch branch
snippet_gl_loop_head_end:

snippet_gl_loop_head_mh:
	ldq	t12, guardOffset(DispatchListL2)
	bne	t12, snippet_check_no_closure	# patch branch
	lda	v0, 1(zero)
	stq	v0, 0(CallFlagsPtrL2MH)
	br	snippet_guard_true		# patch branch
snippet_gl_loop_head_mh_end:
	
	# set the arguments to be passed to a guard or handler
snippet_check_no_closure:		
	ldq	t1, useGuardClosureOffset(DispatchListL2)
	beq	t1, snippet_check_no_closure
snippet_check_no_closure_end:	

snippet_branch_closure:	
	br	snippet_check_no_closure
snippet_branch_closure_end:	
	
	# guard loop tail
	# call the guard, save the result in the call-flag list
	# increase the counter of handlers to be invoked, 
	# check if it cancels, loop back
snippet_call_guard:
	jsr     ra, (t12), 0
	ldgp	gp, 0(ra)
	beq     v0, snippet_gl_loop_tail	# patch branch
snippet_guard_true:
	bis	CallFlagsIdxL2, CallFlagsL2, CallFlagsL2
	ldq	t12, cancelOffset(DispatchListL2)
	bne	t12, snippet_handler_loop	# patch branch
snippet_gl_loop_tail:
	sll	CallFlagsIdxL2, 1, CallFlagsIdxL2
	lda	DispatchListL2, SizeOfHandlerDesc(DispatchListL2)
	ldq	t12, handlerOffset(DispatchListL2)
	beq	t12, snippet_handler_loop	# patch branch | FIXME
	br	snippet_gl_loop_head		# patch branch | one branch
snippet_call_guard_end:	
	
snippet_call_guard_mh:
	jsr     ra, (t12), 0
	ldgp	gp, 0(ra)
	stq	v0, 0(CallFlagsPtrL2MH)
	beq     v0, snippet_gl_loop_tail	# patch branch
snippet_guard_true_mh:
	lda	CallCntL2MH, 1(CallCntL2MH)
	ldq	t12, cancelOffset(DispatchListL2)
	bne	t12, snippet_handler_loop	# patch branch
snippet_gl_loop_tail_mh:
	lda	CallFlagsPtrL2MH, 8(CallFlagsPtrL2MH)
	lda	DispatchListL2, SizeOfHandlerDesc(DispatchListL2)
	ldq	t12, handlerOffset(DispatchListL2)
	beq	t12, snippet_handler_loop	# patch branch | FIXME
	br	snippet_gl_loop_head		# patch branch | one branch
snippet_call_guard_mh_end:	
	
	# head of the handler loop
	# check whether there are any handlers called
	# reload pointer to the beginning of the call list
	# set result to 0
snippet_handler_loop:
	bne	CallFlagsL2, skip_7
	bsr	snippet_no_handler		# patch branch
skip_7:	
	bis	zero, zero, FinalResultL2
snippet_handler_loop_end:

snippet_handler_loop_mh:
	bne	CallCntL2MH, skip_8
	bsr	snippet_no_handler		# patch branch
skip_8:	
	lda	CallFlagsPtrL2MH, ARGUMENTS_FRAME(sp)
	bis	zero, zero, FinalResultL2MH
snippet_handler_loop_mh_end:

	# find the next handler to call or the end of the list
	# check for closure
snippet_hl_loop_head:
	ldq	t12, handlerOffset(DispatchListL2)
	beq	t12, snippet_epilog		# patch branch
	and	CallFlagsL2, 1, t1
	bne	t1, snippet_call_handler	# patch branch
	srl	CallFlagsL2, 1, CallFlagsL2
	lda	DispatchListL2, SizeOfHandlerDesc(DispatchListL2)
	br	snippet_hl_loop_head		# patch branch
snippet_hl_loop_head_end:

snippet_hl_loop_head_mh:
	ldq	t12, handlerOffset(DispatchListL2)
	beq	t12, snippet_epilog		# patch branch
	ldq	t1, 0(CallFlagsPtrL2MH)
	bne	t1, snippet_call_handler	# patch branch
	lda	CallFlagsPtrL2MH, 8(CallFlagsPtrL2MH)
	lda	DispatchListL2, SizeOfHandlerDesc(DispatchListL2)
	br	snippet_hl_loop_head		# patch branch
snippet_hl_loop_head_mh_end:

	# pass callee-saved registers (s0-s4, sp, pc)
snippet_pass_callee_saved:	
	lda	t2, (ARGUMENTS_FRAME+GUARD_RESULTS_FRAME+0)(sp)
	bis	t2, t2, t3
snippet_pass_callee_saved_end:
	
	# execute a handler
	# save result
snippet_call_handler:	
	jsr     ra, (t12), 0
	ldgp	gp, 0(ra)
	bis	v0, v0, FinalResultL2
snippet_call_handler_end:

/* for the case there is result */
snippet_call_result_handler:	
	lda	a0, 0(sp)	# address of the final result
	bis	v0, v0, a1	# value of this result
	lda	a2, 0(zero)	# last flag
	lda	a3, 0(sp)	# address of the extra argument
snippet_call_result_handler_end:	

/* for the case there is no result */
snippet_call_result_handler_nr:	
	lda	a0, 0(zero)	# last flag
	lda	a1, 0(sp)	# address of the extra argument
snippet_call_result_handler_nr_end:	


snippet_call_handler_mh:	
	jsr     ra, (t12), 0
	ldgp	gp, 0(ra)
	bis	v0, v0, FinalResultL2MH
snippet_call_handler_mh_end:	

	# have we passed callee-saved registers?
snippet_passed_callee_saved:	
	lda	t1, 0(zero)
	beq	t1, snippet_hl_loop_tail	# patch branch
snippet_passed_callee_saved_end:

snippet_adjust_sp:			
	# readjust the stack pointer
snippet_inc_stack:
	lda	sp, 32(sp)
snippet_adjust_sp_end:
	
snippet_hl_loop_tail:
	# check if it cancels
	ldq	t12, cancelOffset(DispatchListL2)
	bne	t12, snippet_epilog		# patch branch

	# repeat
	srl	CallFlagsL2, 1, CallFlagsL2
	lda	DispatchListL2, SizeOfHandlerDesc(DispatchListL2)
	br	snippet_hl_loop_head		# patch branch
snippet_hl_loop_tail_end:	
		
snippet_hl_loop_tail_mh:
	# check if it cancels
	ldq	t12, cancelOffset(DispatchListL2)
	bne	t12, snippet_epilog		# patch branch

	# repeat
	lda	CallFlagsPtrL2MH, 8(CallFlagsPtrL2MH)
	lda	DispatchListL2, SizeOfHandlerDesc(DispatchListL2)
	br	snippet_hl_loop_head		# patch branch
snippet_hl_loop_tail_mh_end:	
		
	.globl	snippet_epilog
	.globl	snippet_epilog_end
snippet_epilog:
	bis	FinalResultL2, FinalResultL2, v0
	ldq	s0, 1(sp)	# patch offset to the saved register
	ldq	s1, 2(sp)	# patch offset to the saved register
	ldq	s2, 3(sp)	# patch offset to the saved register
	ldq	s3, 4(sp)	# patch offset to the saved register
	ldq	s4, 5(sp)	# patch offset to the saved register
	ldq	s5, 6(sp)	# patch offset to the saved register
	ldq	s6, 7(sp)	# patch offset to the saved register
	ldq	ra, 8(sp)	# patch offset to the saved register
	lda	sp, 9(sp)	# patch frame size
	ret	zero, (ra), 1
snippet_epilog_end:	

	.globl	snippet_restore
	.globl	snippet_restore_end
snippet_restore:
	bis	FinalResultL2, FinalResultL2, v0
	ldq	s0, 1(sp)	# patch offset to the saved register
	ldq	s1, 2(sp)	# patch offset to the saved register
	ldq	s2, 3(sp)	# patch offset to the saved register
	ldq	s3, 4(sp)	# patch offset to the saved register
	ldq	s4, 5(sp)	# patch offset to the saved register
	ldq	s5, 6(sp)	# patch offset to the saved register
	ldq	s6, 7(sp)	# patch offset to the saved register
	ldq	ra, 8(sp)	# patch offset to the saved register
	lda	sp, 9(sp)	# patch frame size
snippet_restore_end:	

	.globl	snippet_restore_mh
	.globl	snippet_restore_mh_end
snippet_restore_mh:
	bis	FinalResultL2MH, FinalResultL2MH, v0
	ldq	s0, 1(sp)	# patch offset to the saved register
	ldq	s1, 2(sp)	# patch offset to the saved register
	ldq	s2, 3(sp)	# patch offset to the saved register
	ldq	s3, 4(sp)	# patch offset to the saved register
	ldq	s4, 5(sp)	# patch offset to the saved register
	ldq	s5, 6(sp)	# patch offset to the saved register
	ldq	s6, 7(sp)	# patch offset to the saved register
	ldq	ra, 8(sp)	# patch offset to the saved register
	lda	sp, 9(sp)	# patch frame size
snippet_restore_mh_end:
	
snippet_epilog_g:
	bis	CallFlagsL2, CallFlagsL2, v0
	ldq	s0, 1(sp)	# patch offset to the saved register
	ldq	s1, 2(sp)	# patch offset to the saved register
	ldq	s2, 3(sp)	# patch offset to the saved register
	ldq	s3, 4(sp)	# patch offset to the saved register
	ldq	s4, 5(sp)	# patch offset to the saved register
	ldq	s5, 6(sp)	# patch offset to the saved register
	ldq	s6, 7(sp)	# patch offset to the saved register
	ldq	ra, 8(sp)	# patch offset to the saved register
	lda	sp, 9(sp)	# patch frame size
	ret	zero, (ra), 1
snippet_epilog_g_end:	

snippet_epilog_mh:
	bis	FinalResultL2MH, FinalResultL2MH, v0
	ldq	s0, 1(sp)	# patch offset to the saved register
	ldq	s1, 2(sp)	# patch offset to the saved register
	ldq	s2, 3(sp)	# patch offset to the saved register
	ldq	s3, 4(sp)	# patch offset to the saved register
	ldq	s4, 5(sp)	# patch offset to the saved register
	ldq	s5, 6(sp)	# patch offset to the saved register
	ldq	s6, 7(sp)	# patch offset to the saved register
	ldq	ra, 8(sp)	# patch offset to the saved register
	lda	sp, 9(sp)	# patch frame size
	ret	zero, (ra), 1
snippet_epilog_mh_end:

snippet_no_handler:	
	lda	t1,  MI_DispatcherPrivate
	ldq	t12, RaiseNoHandler_OFFSET(t1)
	jsr	ra, (t12), 0
snippet_no_handler_end:	

snippet_no_handler_gp:
	br	t1, next
next:	ldgp	gp, 0(t1)
	lda	t1,  MI_DispatcherPrivate
	ldq	t12, RaiseNoHandler_OFFSET(t1)
	jsr	ra, (t12), 0
snippet_no_handler_gp_end:

snippet_guard_closure:
snippet_handler_closure:
	.end	snippet_code
		
/******************************************************************************
 *	
 *
 *
 *****************************************************************************/
	
 /*
  * bis
  */	
	.globl	snippet_bis
	.globl	snippet_bis_end

	.ent	snippet_bis
snippet_bis:	
	bis	t0, t1, t2
snippet_bis_end:
	.end	snippet_bis


 /*
  * bis_i
  */	
	.globl	snippet_bis_i
	.globl	snippet_bis_i_end

	.ent	snippet_bis_i
snippet_bis_i:	
	bis	v0, 13, v0
snippet_bis_i_end:
	.end	snippet_bis_i


 /*
  * lda
  */	
	.globl	snippet_lda
	.globl	snippet_lda_end

	.ent	snippet_lda
snippet_lda:	
	lda	zero, 0(zero)
snippet_lda_end:
	.end	snippet_lda


 /*
  * stq
  */	
	.globl	snippet_stq
	.globl	snippet_stq_end

	.ent	snippet_stq
snippet_stq:	
	stq	zero, 0(zero)
snippet_stq_end:
	.end	snippet_stq


 /*
  * ldq
  */	
	.globl	snippet_ldq
	.globl	snippet_ldq_end

	.ent	snippet_ldq
snippet_ldq:	
	ldq	t1, 0(t2)
snippet_ldq_end:
	.end	snippet_ldq


 /*
  * a patch to load zero
  */	
	.globl	st_write_zero
	.globl	st_write_zero
	.globl	st_write_zero_end

	.ent	st_write_zero
st_write_zero:	
	lda	t12, 0(zero)
st_write_zero_end:	
	.end	st_write_zero



/******************************************************************************
 *	
 *				LEVEL 3
 *
 *****************************************************************************/


	.globl	snippet_load_long
	.globl	snippet_load_long_end
	.ent	snippet_load_long
	
	# load a pointer
snippet_load_long:	
	ldah	t12, 0x0(zero)
	lda	t12, 0x0(t12)
	sll	t12, 0x20, t12
	ldah	t12, 0x0(t12)
	lda	t12, 0x0(t12)	
snippet_load_long_end:	
	.end	snippet_load_long
	
	.globl	snippet_load_short
	.globl	snippet_load_short_end
	.ent	snippet_load_short
	
	# load a pointer
snippet_load_short:	
	ldah	t12, 0x0(gp)
	lda	t12, 0x0(t12)
snippet_load_short_end:	
	.end	snippet_load_short
	
	.globl	snippet_call_guard_dir
	.globl	snippet_call_guard_dir_end
	.ent	snippet_call_guard_dir
snippet_call_guard_dir:	
	# execute a guard
	jsr     ra, (t12), 0
	ldgp	gp, 0(ra)

	# skip if false
	beq     v0, snippet_call_guard_dir_end

	# increase the counter of handlers to be invoked
	/* OPT OPT OPT */
	lda	t0, 1(zero)
	sll	t0, 13, t0
	bis	t0, YYYY, YYYY
snippet_call_guard_dir_end:	
	.end	snippet_call_guard_dir
	
	.globl	snippet_call_all_guard
	.globl	snippet_call_all_guard_end
	.ent	snippet_call_all_guard
snippet_call_all_guard:	
	# execute a guard
	jsr     ra, (t12), 0
	ldgp	gp, 0(ra)

	# load the flags from the result
	bis	v0, v0, YYYY
snippet_call_all_guard_end:	
	.end	snippet_call_all_guard
	
	.globl	snippet_call_imposed_dir
	.globl	snippet_call_imposed_dir_end
	.ent	snippet_call_imposed_dir
snippet_call_imposed_dir:	
	# execute an imposed guard
	jsr     ra, (t12), 0
	ldgp	gp, 0(ra)

	# skip if false
	beq     v0, snippet_call_imposed_dir_end
snippet_call_imposed_dir_end:
	.end	snippet_call_imposed_dir
	
	.globl	snippet_call_imposed_dir_2
	.globl	snippet_call_imposed_dir_2_end
	.ent	snippet_call_imposed_dir_2
snippet_call_imposed_dir_2:	
	jsr     ra, (t12), 0
	lda	gp, 0(ra)
	beq     v0, snippet_call_imposed_dir_2_end
snippet_call_imposed_dir_2_end:	
	.end	snippet_call_imposed_dir_2
	
	.globl	snippet_bis_value
	.globl	snippet_bis_value_end
	.ent	snippet_bis_value
snippet_bis_value:	
	bis	YYYY, 1, YYYY
snippet_bis_value_end:
	.end	snippet_bis_value

	.globl	snippet_start_up_unrolled
	.globl	snippet_start_up_unrolled_end
	.ent	snippet_start_up_unrolled
snippet_start_up_unrolled:		
	bis	zero, zero, YYYY
snippet_start_up_unrolled_end:	
	.end	snippet_start_up_unrolled

	.globl	snippet_start_up_unrolled_2
	.globl	snippet_start_up_unrolled_2_end
	.ent	snippet_start_up_unrolled_2
snippet_start_up_unrolled_2:		
	bis	zero, zero, FinalResultL2
	bis	zero, zero, CallFlagsL2
snippet_start_up_unrolled_2_end:	
	.end	snippet_start_up_unrolled_2

	.globl	snippet_check_call
	.globl	snippet_check_call_end
	.ent	snippet_check_call
snippet_check_call:	
	lda	t0, 1(zero)
	sll	t0, 13, t0
	and	YYYY, t0, t0
	beq	t0, snippet_check_call
snippet_check_call_end:	
	.end	snippet_check_call

	.globl	snippet_check_call_old
	.globl	snippet_check_call_end_old
	.ent	snippet_check_call_old
snippet_check_call_old:	
	and	YYYY, 1, t0
	beq	t0, snippet_check_call
snippet_check_call_end_old:	
	.end	snippet_check_call_old

	.globl	snippet_call_handler_dir
	.globl	snippet_call_handler_dir_end
	.ent	snippet_call_handler_dir 2
snippet_call_handler_dir:	
	# execute a handler
	jsr     ra, (t12), 0
	ldgp	gp, 0(ra)

	# save result
	bis	v0, v0, XXXX
snippet_call_handler_dir_end:	
	.end	snippet_call_handler_dir
	
	.globl	snippet_check_handlers
	.globl	snippet_check_handlers_end
	.ent	snippet_check_handlers 2
snippet_check_handlers:
	bne	CallFlagsL2, skip_9
	bsr	snippet_no_handler
skip_9:	
snippet_check_handlers_end:	
	.end	snippet_check_handlers 2

	#
	# call a procedure and restore the gp on return
	# 
	.globl	snippet_call_dir
	.globl	snippet_call_dir_end
	.ent	snippet_call_dir
snippet_call_dir:	
	jsr     ra, (t12), 0
	ldgp	gp, 0(ra)
snippet_call_dir_end:	
	.end	snippet_call_dir
	
	.globl	snippet_save_result
	.globl	snippet_save_result_end
	.ent	snippet_save_result
snippet_save_result:
	bis	v0, v0, FinalResultL2
	lda	CallFlagsL2, 1(zero)
snippet_save_result_end:
	.end	snippet_save_result
	
	.globl	snippet_call_handler_dir_1
	.globl	snippet_call_handler_dir_1_end
	.ent	snippet_call_handler_dir_1 2
snippet_call_handler_dir_1:	
	jsr     ra, (t12), 0
	lda	gp, 0(ra)
	bis	v0, v0, XXXX
snippet_call_handler_dir_1_end:	
	.end	snippet_call_handler_dir_1
	
	.globl	snippet_call_handler_dir_2
	.globl	snippet_call_handler_dir_2_end
	.ent	snippet_call_handler_dir_2 2
snippet_call_handler_dir_2:	
	jsr     ra, (t12), 0
	lda	gp, 0(ra)
snippet_call_handler_dir_2_end:	
	.end	snippet_call_handler_dir_2
	

	.globl	snippet_shift_value
	.globl	snippet_shift_value_end
	.ent	snippet_shift_value
snippet_shift_value:	
	lda	t0, 1(zero)
	sll	t0, 13, t0
	bis	t0, YYYY, YYYY
snippet_shift_value_end:	
	.end	snippet_shift_value_end


/******************************************************************************
 *	
 *				BYPASS
 *
 *****************************************************************************/

	.globl patch_stub
	.globl patch_stub_end
	.globl patch_long_patch
	.ent patch_stub
patch_stub:
	ldah	t2, 0x0(zero)
	lda	t2, 0x0(t2)
	sll	t2, 0x20, t2
	ldah	t2, 0x0(t2)
	lda	t12, 0x0(t2)
	jsr	ra, (t12), 0
patch_stub_end:	
	.end patch_stub

	.globl bypass_stub
	.globl bypass_stub_end
	.ent bypass_stub
bypass_stub:
	lda	sp, -16(sp)
	stq	ra, 0(sp)
	bis	ra, ra, a1
	ldah	t2, 0x0(zero)
	lda	t2, 0x0(t2)
	sll	t2, 0x20, t2
	ldah	t2, 0x0(t2)
	lda	t12, 0x0(t2)
	jsr	ra, (t12), 0
	ldq	ra, 0(sp)
	lda	sp, 16(sp)
	ret	zero, (ra), 1
bypass_stub_end:	
	.end bypass_stub

	.globl jumpin_stub
	.globl jumpin_stub_end
	.ent jumpin_stub
jumpin_stub:	
	ldgp	gp, 0(t12)
	lda	sp, -16(sp)
	stq	ra, 0(sp)

	lda	ra, (23*4)(t12)
	
	ldah	t2, 0x0(zero)
	lda	t2, 0x0(t2)
	sll	t2, 0x20, t2
	ldah	t2, 0x0(t2)
	lda	t12, 0x0(t2)	
	
	bis	a1, a1, t1
	bis	a2, a2, t2
	bis	a3, a3, t3
	bis	a4, a4, t4
	bis	a5, a5, t5
	bis	a0, a0, t6
	bis	a1, a1, t1
	bis	a2, a2, t2
	bis	a3, a3, t3
	bis	a4, a4, t4
	bis	a5, a5, t5
	bis	a0, a0, t6

	jsr	ra, (t12), 0

	ldq	ra, 0(sp)
	lda	sp, 16(sp)
	ret	zero, (ra), 1
jumpin_stub_end:	
	.end jumpin_stub

	.globl dec_sp
dec_sp:	lda	sp, -16(sp)
	

	.globl snippet_return
	.globl snippet_return_end
	.ent snippet_return
snippet_return:
	ret	zero, (ra), 1
snippet_return_end:
	.end snippet_return

	.globl  snippet_branch_fwd
	.globl  snippet_branch_fwd_end
	.ent    snippet_branch_fwd
snippet_branch_fwd:   
	br      snippet_branch_fwd
snippet_branch_fwd_end:       
	.end    snippet_branch_fwd


	.globl  snippet_beq_v0
	.globl  snippet_beq_v0_end
	.ent    snippet_beq_v0
snippet_beq_v0:
	beq     v0, snippet_beq_v0
snippet_beq_v0_end:
	.end    snippet_beq_v0

/******************************************************************************
 *	
 *				TRACING
 *
 *****************************************************************************/

	#
	# snippets for keeping track of the number of invoked guards and
	# handler and of the time spent in the stub
	#
	
	#
	# additional space allocated at the top of the stack to pass
	# extra arguments to the code imposed on the handler
	#
#define TRACE_CALL_AR            0

	#
	# additional space allocated at the bottom of the stack to keep the 
	# statistics for this call, must accomodate all of the offsets below.
	#
#define TRACE_STORE_AR          80

	#
	# locations on stack used by the tracing code
	# the stitcher knows that: 
	#    total + 16 => guard + 16 => imposed + 16 => handler
	#
#define TMP_TIMER_OFFSET      (-8)
#define STUB_OFFSET           (-16)
#define SCRATCH_OFFSET        (-24)
#define TOTAL_TIMER_OFFSET    (-32)
#define GUARD_CNT_OFFSET      (-40)
#define GUARD_TIMER_OFFSET    (-48)
#define IMPOSED_CNT_OFFSET    (-56)
#define IMPOSED_TIMER_OFFSET  (-64)
#define HANDLER_CNT_OFFSET    (-72)
#define HANDLER_TIMER_OFFSET  (-80)

	#
	# initialize all information on stack
	# store the new tmp_timer in the scratch location on the stack
	#
	.globl	snippet_trace_enter
	.globl	snippet_trace_enter_end
	.ent	snippet_trace_enter
snippet_trace_enter:
	stq	zero, (TOTAL_TIMER_OFFSET)(sp)
	stq	zero, (GUARD_TIMER_OFFSET)(sp)
	stq	zero, (HANDLER_TIMER_OFFSET)(sp)
	stq	zero, (GUARD_CNT_OFFSET)(sp)
	stq	zero, (HANDLER_CNT_OFFSET)(sp)
	stq	zero, (TOTAL_TIMER_OFFSET)(sp)
	stq	t12,  (STUB_OFFSET)(sp)
	rpcc	t0
	zapnot	t0, 15, t0
	stq	t0, (TMP_TIMER_OFFSET)(sp)
snippet_trace_enter_end:
	.end	snippet_trace_enter

	#
	# compute the time spent in the stub and add to the total
	# capture start time for the call it in the tmp timer location
	#
	.globl	snippet_trace_capture
	.globl	snippet_trace_capture_end
	.ent	snippet_trace_capture
snippet_trace_capture:
	rpcc	t0				# capture time
	zapnot	t0, 15, t0			# zero its high bits
	ldq	t1, (TMP_TIMER_OFFSET)(sp)	# load previous time
	subq	t0, t1, t2			# compute time spent
	ldq	t4, (TOTAL_TIMER_OFFSET)(sp)	# load previous total
	cmplt	t2, zero, t3			# is time spent negative
	stq	t0, (TMP_TIMER_OFFSET)(sp)	# store the caputured time
	beq	t3, stcb			# branch over if positive
	bis	zero, 1, t3			# adjust the time spent
	sll	t3, 32, t3			# 
	subq	t3, t2, t2			# 
stcb:	addq	t2, t4, t2			# add it to total
	stq	t2, (TOTAL_TIMER_OFFSET)(sp)	# store the total
snippet_trace_capture_end:
	.end	snippet_trace_capture

	#
	# increment a counter
	#
	.globl	snippet_trace_increment
	.globl	snippet_trace_increment_end
	.ent	snippet_trace_increment
snippet_trace_increment:
	ldq	t0, (TOTAL_TIMER_OFFSET)(sp)	# load previous counter
	addq	t0, 1, t0			# increment
	ldq	t0, (TOTAL_TIMER_OFFSET)(sp)	# store new value
snippet_trace_increment_end:
	.end	snippet_trace_increment

	#
	# revert some state to make a correct call and return
	#
	.globl	snippet_trace_exit_1
	.globl	snippet_trace_exit_1_end
	.ent	snippet_trace_exit_1
snippet_trace_exit_1:
	lda	sp, 1(sp)
	stq	v0, (SCRATCH_OFFSET)(sp)
snippet_trace_exit_1_end:
	.end	snippet_trace_exit_1

	#
	# call the atomic update function and redo a part of epilog
	#
	.globl	snippet_trace_exit_2
	.globl	snippet_trace_exit_2_end
	.ent	snippet_trace_exit_2
snippet_trace_exit_2:
	# read all the accumulated data
	# make sure you do it before you change the stack pointer
	ldq	a0, (STUB_OFFSET)(sp)
	ldq	a1, (TOTAL_TIMER_OFFSET)(sp)
	ldq	a2, (IMPOSED_CNT_OFFSET)(sp)
	ldq	a3, (IMPOSED_TIMER_OFFSET)(sp)
	ldq	a4, (GUARD_CNT_OFFSET)(sp)
	ldq	a5, (GUARD_TIMER_OFFSET)(sp)
	ldq	t0, (HANDLER_CNT_OFFSET)(sp)
	ldq	t1, (HANDLER_TIMER_OFFSET)(sp)

	# add a bit to the stack frame to pass extra two arguments
	lda	sp, -16(sp)
	stq	t0, 0(sp)
	stq	t1, 8(sp)

	# make the call
	ldah	t12, 0x0(zero)
	lda	t12, 0x0(t12)
	sll	t12, 0x20, t12
	ldah	t12, 0x0(t12)
	lda	t12, 0x0(t12)	
	jsr	ra, (t12), 0

	# remove the extra activation record
	lda	sp, 16(sp)

	# we need to redo this code because that state was garbled
	# by the call to the counting routine, we do it twice so
	# that the last rpcc takes its execution into acccount
	ldq	v0, (SCRATCH_OFFSET)(sp)
	ldq	ra, 1(sp)
	lda	sp, 1(sp)
snippet_trace_exit_2_end:
	.end	snippet_trace_exit_2

	
/******************************************************************************
 *	
 *				LINEARIZED ARGUMENTS
 *
 *****************************************************************************/

/*
 * Turns arguments into a vector allocated on stack and passed as the
 * first argument to a given procedure.
 */
	.globl	argument_prolog
	.globl	argument_prolog_end
	.globl  argument_prolog_sp
	.globl  argument_array_size
	.globl	argument_epilog
	.globl	argument_epilog_end
	.globl	argument_epilog_sp
	.globl	argument_gp_patch
	.globl	argument_profile_gp_patch
	
	.ent	argument_prolog
argument_prolog:
argument_gp_patch:
	ldgp	gp, 0(t12)

#ifdef CALL_GRAPH
	.set	noat
	bis	ra, 1, ra
	lda	$at, _mcount
	jsr	$at, ($at), _mcount
	.set	at
argument_profile_gp_patch:	
	ldgp	gp,0(pv)
#endif CALL_GRAPH

argument_prolog_sp:
	lda	sp, 0(sp)
	stq	ra, 0(sp)

argument_load_array:
	lda	t0, 16(sp)	# pointer to the array
	lda	t1, 16(t0)	# pointer to the array data
	stq	t1, 0(t0)
argument_array_size:
	lda	t1, 13(zero)	# array size
	stq	t1, 8(t0)
argument_prolog_end:

argument_epilog:
	# make the call
	jsr	ra, (t12), 0
	ldgp	gp, 0(ra)

	# return
	ldq	ra, 0(sp)
argument_epilog_sp:
	lda	sp, 0(sp)
	ret	zero, (ra), 1
argument_epilog_end:	

	.end

	





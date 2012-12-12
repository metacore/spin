/*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 22-Aug-95  Emin Gun Sirer (egs) at the University of Washington
 *	Added CallContinuation for dec osf/mach compatibility support.
 *
 *  28-Nov-94  Emin Gun Sirer (egs) at the University of Washington
 *	Created. In-kernel thread startup routine.
 *
 */
#include <machine/asmacros.h>

	.text
	.align	4

/*
 * Get a thread started up.
 *	Called from the run handler by restoring the callee save
 *      registers.
 */
ENTRY(Startup)
	pushl	%edi
	call	*%esi
	/* not reached */

/*
 * Start up a continuation.
 *	Called directly from M3.
 *
 * PROCEDURE(thread: Thread.T; 
 *    bodystarter: PROCEDURE(a:Thread.T); newsp: Word.T);
 */
ENTRY(CallContinuation)
	movl	4(%esp),%eax
	movl	8(%esp),%ecx
	movl	12(%esp),%esp
	pushl	%eax
	call	*%ecx
	/* not reached */
	

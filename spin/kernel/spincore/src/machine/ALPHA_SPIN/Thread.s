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
#define ASSEMBLER 1	
#include <machine/asm.h>
#include <machine/regdef.h>
	.text
	.align	4

#define	CSW_IMASK	\
	IM_S0|IM_S1|IM_S2|IM_S3|IM_S4|IM_S5|IM_S6|IM_RA|IM_GP

/*
 * Get a thread started up.
 *	Called from the run handler by restoring the callee save
 *      registers.
 */
LEAF(Startup)
	mov	s0, a0
	mov	s1, pv
	jmp	zero,(pv),1
	END(Startup)

/*
 * Start up a continuation.
 *	Called directly from M3.
 */
NESTED(CallContinuation, 3, zero)
	mov	a1, pv
	mov	a2, sp
	jmp	zero,(pv),1
	END(CallContinuation)

/*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 24-Dec-97  Tsutomu Owa (owa) at the University of Washington
 *	Created.
 */

#include <machine/asmacros.h>
#define PCB_ESP0 0x4 /* see Core.S */

	.text
/*
 * Save PCB_ESP0(_curpcb)
 */
ENTRY(SaveESP)
	pushl	%eax			/* save eax */
	pushl	%ecx			/* save ecx */
	movl	_curpcb, %eax		/* current pcb */
	movl	PCB_ESP0(%eax), %eax	/* ESP0 -> eax */
	movl	12(%esp), %ecx 		/* ecx points to the arg */
	movl	%eax, (%ecx)		/* ESP0 -> on stack */
	popl	%eax			/* restore eax */
	popl	%ecx			/* restore eax */
	ret

/*
 * Restore PCB_ESP0(_curpcb)
 */
ENTRY(RestoreESP)
	pushl	%eax			/* save eax */
	pushl	%ecx			/* save ecx */
	movl	12(%esp), %ecx		/* */
	movl	_curpcb, %eax		/* current pcb */
	movl	(%ecx), %ecx
	movl	%ecx, PCB_ESP0(%eax)	/* restore ESP0 */
	popl	%ecx			/* restore ecx */
	popl	%eax			/* restore eax */
	ret

/*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 16-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Created from Alpha version.
 *
 *
 */
#include <machine/asmacros.h>

	.text

ENTRY(EnableUserFloatOps)
	pushl	$0x362
	call	_npxinit
	addl	$4,%esp
	clts
	ret

ENTRY(GetUserFloatRegs)
	movl	4(%esp),%ecx
	pushl	%ecx
	call	_npxsave
	addl	$4,%esp
	clts
	ret

ENTRY(SetUserFloatRegs)
	movl	4(%esp),%ecx
	clts
	frstor  0(%ecx)
	ret









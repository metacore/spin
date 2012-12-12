/*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 10-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	Changed to use ldt instead of ldg.
 *
 * 10-Dec-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Routines to save/restore ALPHA FPU state.
 *
 */
#include <machine/asm.h>
#include <machine/regdef.h>
#include <machine/reg.h>

/*
 * The fpustate.T structure is declared in Modula-3 in alpha/FPUState.i3
 */
	.text
	.align 3
	.set noat
	.set noreorder

LEAF(EnableUserFloatOps)
	ldiq	a0, 1
	call_pal PAL_wrfen
	RET
	END(EnableUserFloatOps)

LEAF(GetUserFloatRegs)
	/* save f0 */
        stt     $f0, (0 << 3)(a0)
	/* save fpcr */
	excb
	mf_fpcr	$f0,$f0,$f0
	trapb
        stt     $f0, (32 << 3)(a0)
	/* save f1-f30 */
        stt     $f1, (1 << 3)(a0)
        stt     $f2, (2 << 3)(a0)
        stt     $f3, (3 << 3)(a0)
        stt     $f4, (4 << 3)(a0)
        stt     $f5, (5 << 3)(a0)
        stt     $f6, (6 << 3)(a0)
        stt     $f7, (7 << 3)(a0)
        stt     $f8, (8 << 3)(a0)
        stt     $f9, (9 << 3)(a0)
        stt     $f10, (10 << 3)(a0)
        stt     $f11, (11 << 3)(a0)
        stt     $f12, (12 << 3)(a0)
        stt     $f13, (13 << 3)(a0)
        stt     $f14, (14 << 3)(a0)
        stt     $f15, (15 << 3)(a0)
        stt     $f16, (16 << 3)(a0)
        stt     $f17, (17 << 3)(a0)
        stt     $f18, (18 << 3)(a0)
        stt     $f19, (19 << 3)(a0)
        stt     $f20, (20 << 3)(a0)
        stt     $f21, (21 << 3)(a0)
        stt     $f22, (22 << 3)(a0)
        stt     $f23, (23 << 3)(a0)
        stt     $f24, (24 << 3)(a0)
        stt     $f25, (25 << 3)(a0)
        stt     $f26, (26 << 3)(a0)
        stt     $f27, (27 << 3)(a0)
        stt     $f28, (28 << 3)(a0)
        stt     $f29, (29 << 3)(a0)
        stt     $f30, (30 << 3)(a0)
        stt     $f31, (31 << 3)(a0)
	RET
	END(GetUserFloatRegs)

LEAF(SetUserFloatRegs)
	/* set fpcr */
	ldt	$f0, (32 << 3)(a0)
	trapb
	mt_fpcr	$f0,$f0,$f0
	trapb
	/* set f0-f30 */
        ldt     $f0, (0 << 3)(a0)
        ldt     $f1, (1 << 3)(a0)
        ldt     $f2, (2 << 3)(a0)
        ldt     $f3, (3 << 3)(a0)
        ldt     $f4, (4 << 3)(a0)
        ldt     $f5, (5 << 3)(a0)
        ldt     $f6, (6 << 3)(a0)
        ldt     $f7, (7 << 3)(a0)
        ldt     $f8, (8 << 3)(a0)
        ldt     $f9, (9 << 3)(a0)
        ldt     $f10, (10 << 3)(a0)
        ldt     $f11, (11 << 3)(a0)
        ldt     $f12, (12 << 3)(a0)
        ldt     $f13, (13 << 3)(a0)
        ldt     $f14, (14 << 3)(a0)
        ldt     $f15, (15 << 3)(a0)
        ldt     $f16, (16 << 3)(a0)
        ldt     $f17, (17 << 3)(a0)
        ldt     $f18, (18 << 3)(a0)
        ldt     $f19, (19 << 3)(a0)
        ldt     $f20, (20 << 3)(a0)
        ldt     $f21, (21 << 3)(a0)
        ldt     $f22, (22 << 3)(a0)
        ldt     $f23, (23 << 3)(a0)
        ldt     $f24, (24 << 3)(a0)
        ldt     $f25, (25 << 3)(a0)
        ldt     $f26, (26 << 3)(a0)
        ldt     $f27, (27 << 3)(a0)
        ldt     $f28, (28 << 3)(a0)
        ldt     $f29, (29 << 3)(a0)
        ldt     $f30, (30 << 3)(a0)
        ldt     $f31, (31 << 3)(a0)
	RET
	END(SetUserFloatRegs)

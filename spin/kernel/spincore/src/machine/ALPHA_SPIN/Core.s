/*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *
 * 21-Aug-97  Robert Grimm (rgrimm) at the University of Washington
 *      Removed unnecessary store from longjmp
 *
 * 24-Feb-97  Yasushi Saito (yasushi) at the University of Washington
 *	Added pfm interrupt handling.
 *
 * 10-Jan-97  Yasushi Saito (yasushi) at the University of Washington
 *	Merged {Set,Restore}InterruptMask into SpinSwapIpl.
 *
 * 07-Jan-97  becker at the University of Washington
 *	Added profile call in SpinSwapIPL
 *
 * 27-Jun-96  Marc Fiuczynski (mef) at the University of Washington
 *	Export corebegin and coreend as procedures so that CoreExtern
 *      can talk about it.  Core.m3 then adds the core code region
 *      explicitly with the KernelRegions module.
 *
 * 07-May-96  Stefan Savage (savage) at the University of Washington
 *	Put longjmp and setjmp here (since they are really M3 support)
 *	and changed them to not save and restore ps.
 *
 * 30-Apr-96  Emin Gun Sirer (egs) at the University of Washington
 *	Pass extra args about system state to mm, una, arith and if faults.
 *	Fixed the register corruption from last mod.
 *
 * 27-Apr-96  Stefan Savage (savage) at the University of Washington
 *	Pass extra arguments for MM, Unaligned, and Arith faults
 *
 * 19-Apr-96  Emin Gun Sirer (egs) at the University of Washington
 *	Don't save and restore t12 across system calls.
 *	
 * 09-Apr-96  Emin Gun Sirer (egs) at the University of Washington
 *	Restored MSS_SIZE to the correct magic value.
 *
 * 15-Dec-95  David Dion (ddion) at the University of Washington
 *	Added restoration of a3 and a4 to TRAP_spin_returnsyncinterrupt.
 *	a3 for UNIX errno and a4 for UNIX fork().
 *
 * 10-Oct-95  Emin Gun Sirer (egs) at the University of Washington
 *	Pass the faulting virtual address to M3 land.
 *
 * 28-Sep-95  Emin Gun Sirer (egs) at the University of Washington
 *	Route machine checks into m3 land.
 *
 * 19-Aug-95  Emin Gun Sirer (egs) at the University of Washington
 *	Added SpinSwapIpl so ipl resides in a preemptible code region.
 *      Changed SetupCoreLimits to support fast range checks.
 *
 * 07-Aug-95  Stefan Savage (savage) at the University of Washington
 *	Keep a0 from being clobbered (fix from david and david)
 *
 * 30-Jul-95  Przemek Pardyak (pardy) at the University of Washington
 *	Fixed SaveAllRegs to save relative to a0 instead of sp
 *
 * 22-Jun-95  Emin Gun Sirer (egs) at the University of Washington
 *	Added returns to wrent and and changesoftscb.
 *
 * 01-Mar-95  Emin Gun Sirer (egs) at the University of Washington
 *	Added sync vs. async trap entry points.
 *	Removed unnecessary saves.
 *	Added direct spl manipulation.
 *	General cleanup.
 *
 * 28-Nov-94  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Routines to save/restore ALPHA CPU state.
 *               Trap/interrupt/exception entry/exit points.
 *
 */
#include <machine/asm.h>
#include <machine/regdef.h>
#include <machine/reg.h>
#include "profile.h"
	/*
	 * This structure is declared in Modula-3 in alpha/Machine.i3
	 */
#define TFRAME_v0       (0 <<3)
#define TFRAME_t0       (1 <<3)
#define TFRAME_t1       (2 <<3)
#define TFRAME_t2       (3 <<3)
#define TFRAME_t3       (4 <<3)
#define TFRAME_t4       (5 <<3)
#define TFRAME_t5       (6 <<3)
#define TFRAME_t6       (7 <<3)
#define TFRAME_t7       (8 <<3)
#define TFRAME_s0       (9 <<3)
#define TFRAME_s1       (10 <<3)
#define TFRAME_s2       (11 <<3)
#define TFRAME_s3       (12 <<3)
#define TFRAME_s4       (13 <<3)
#define TFRAME_s5       (14 <<3)
#define TFRAME_s6       (15 <<3)
#define TFRAME_a3       (16 <<3)
#define TFRAME_a4       (17 <<3)
#define TFRAME_a5       (18 <<3)
#define TFRAME_t8       (19 <<3)
#define TFRAME_t9       (20 <<3)
#define TFRAME_t10      (21 <<3)
#define TFRAME_t11      (22 <<3)
#define TFRAME_ra       (23 <<3)
#define TFRAME_t12      (24 <<3)
#define TFRAME_at       (25 <<3)
#define TFRAME_ksp      (26 <<3)
#define TFRAME_usp      (27 <<3)
#define TFRAME_ps	(28 <<3)
#define TFRAME_pc       (29 <<3)
#define TFRAME_gp       (30 <<3)
#define TFRAME_a0       (31 <<3)
#define TFRAME_a1       (32 <<3)
#define TFRAME_a2       (33 <<3)

#define TSS_SIZE (28<<3) /* space for all but PAL-saved registers */
#define MSS_SIZE (34<<3) /* space for the whole structure */

#define ALPHA_SPL_HIGH 7

/*
 * These are the offsets into the softscb vector. The softscb vector
 * is maintained purely by our code and does not correpond to anything
 * in PAL land. We use it to demux and dispatch obscure PAL interrupts
 * to the sensible handlers. We should write our own PAL code and get
 * rid of this crappy demux requirement.
 */
#define ENTKNIO  0
#define ENTARITH (1<<3)
#define ENTMM    (2<<3)
#define ENTIF    (3<<3)
#define ENTUNA	 (4<<3)
#define ENTSYS	 (5<<3)
#define ENTCLOCK (6<<3)
#define ENTMCHCK (7<<3)
#define ENTPFM   (8<<3)
#define ENTSTRAY (9<<3)

	.data
			/* Reserve space for entry points. */
softscb:
	.quad 0 /* entInt IO */
	.quad 0	/* entArith */
	.quad 0 /* entMM */
	.quad 0 /* entIF */
	.quad 0	/* entUnaligned */
	.quad 0 /* entSyscall */
	.quad 0 /* entInt Clock */
        .quad 0 /* entInt Machine Check */
        .quad 0 /* entInt PFM */
        .quad 0 /* entInt Stray */
	.text
	.align 3
	.set noat
	.set noreorder
	
LEAF(SaveAllGeneralRegs)	
        stq     a0, TFRAME_a0(a0)
        stq     a1, TFRAME_a1(a0)
        stq     a2, TFRAME_a2(a0)
        stq     a3, TFRAME_a3(a0)
        stq     a4, TFRAME_a4(a0)
        stq     a5, TFRAME_a5(a0)
        stq     ra, TFRAME_ra(a0)
        stq     v0, TFRAME_v0(a0)
        stq     t0, TFRAME_t0(a0)
        stq     t1, TFRAME_t1(a0)
        stq     t2, TFRAME_t2(a0)
        stq     t3, TFRAME_t3(a0)
        stq     t4, TFRAME_t4(a0)
        stq     t5, TFRAME_t5(a0)
        stq     t6, TFRAME_t6(a0)
        stq     t7, TFRAME_t7(a0)
        stq     t8, TFRAME_t8(a0)
        stq     t9, TFRAME_t9(a0)
        stq     t10, TFRAME_t10(a0)
        stq     t11, TFRAME_t11(a0)
        stq     t12, TFRAME_t12(a0)
        stq     AT, TFRAME_at(a0)
	stq	gp,TFRAME_gp(a0)
	stq	s0,TFRAME_s0(a0)
	stq	s1,TFRAME_s1(a0)
	stq	s2,TFRAME_s2(a0)
	stq	s3,TFRAME_s3(a0)
	stq	s4,TFRAME_s4(a0)
	stq	s5,TFRAME_s5(a0)
	stq	s6,TFRAME_s6(a0)
	stq	sp,TFRAME_ksp(a0)
	stq	ra,TFRAME_pc(a0)
	lda	v0,1(zero)
	RET
	END(SaveAllGeneralRegs)

	/*
	 * Save callee-save registers
	 */
LEAF(SaveCalleeSaveGeneralRegs)	
	stq	s0,TFRAME_s0(a0)
	stq	s1,TFRAME_s1(a0)
	stq	s2,TFRAME_s2(a0)
	stq	s3,TFRAME_s3(a0)
	stq	s4,TFRAME_s4(a0)
	stq	s5,TFRAME_s5(a0)
	stq	s6,TFRAME_s6(a0)
	stq	sp,TFRAME_ksp(a0)
	stq	ra,TFRAME_pc(a0)
	lda	v0,1(zero)
	RET
	END(SaveCalleeSaveGeneralRegs)

	/*
	 * Restore callee-save registers
	 */
LEAF(RestoreCalleeSaveGeneralRegs)	
	ldq	s0,TFRAME_s0(a0)
	ldq	s1,TFRAME_s1(a0)
	ldq	s2,TFRAME_s2(a0)
	ldq	s3,TFRAME_s3(a0)
	ldq	s4,TFRAME_s4(a0)
	ldq	s5,TFRAME_s5(a0)
	ldq	s6,TFRAME_s6(a0)
	ldq	sp,TFRAME_ksp(a0)
	ldq	ra,TFRAME_pc(a0)
	lda	a0,0(zero)
	call_pal PAL_swpipl
	lda	v0,0(zero)
	RET
	END(RestoreCalleeSaveGeneralRegs)

        .globl corebegin
	.ent   corebegin,0 # defines the beginning of the spincore code region.
corebegin:
	END(corebegin)

/*
 *	Synchronous interrupts get here from the SCB
 */
	.align	3
	.globl	TRAP_entSys
	.ent	TRAP_entSys,0
TRAP_entSys:
	.frame	sp,MSS_SIZE,zero
	/*
	 * XXX we are saving a0-a6 onto stack, whereas we can change
	 * syscall signature and pass them directly.
	 */
	addq	sp, -TSS_SIZE, sp
        stq     v0, TFRAME_v0(sp)
        stq     a0, TFRAME_a0(sp)
        stq     ra, TFRAME_ra(sp)

	/* Try to place stq's staggeredly to make use of the CPU pipeline. */
	
	/* s0 .. s6 are saved here although they are not destroyed.   
	   This is to make fork(2) work: the child needs to read the parent's
	   caller-saved registers. */
        stq     s0, TFRAME_s0(sp) 
	stq     s1, TFRAME_s1(sp)
	stq     s2, TFRAME_s2(sp)
	stq     s3, TFRAME_s3(sp)
	stq     s4, TFRAME_s4(sp)
	stq     s5, TFRAME_s5(sp)				 
	stq     s6, TFRAME_s6(sp)
        stq     a3, TFRAME_a3(sp)
        stq     a4, TFRAME_a4(sp)
        stq     a5, TFRAME_a5(sp)
        stq     a1, TFRAME_a1(sp)
        stq     a2, TFRAME_a2(sp)

	call_pal PAL_rdusp
        stq     v0, TFRAME_usp(sp)
	
	# Get a GP  -- OSF uses static gp; we do not
	br	t0 ,1f
 1:	ldgp	gp,0(t0)

	/* a0 = stack frame address */
	lda	a0, 0(sp)
	/*
	 * find the right handler from the software scb vector
	 */
	lda	t1, softscb
	ldq	pv, ENTSYS(t1)
	jsr	ra, (pv)
	
	ldq     a0, TFRAME_usp(sp)
	call_pal	PAL_wrusp
        ldq     v0, TFRAME_v0(sp)
	ldq	a0, TFRAME_a0(sp)
	ldq	a1, TFRAME_a1(sp)
	ldq	a2, TFRAME_a2(sp)
	ldq	a3, TFRAME_a3(sp) 
	ldq	a4, TFRAME_a4(sp) 
        ldq     ra, TFRAME_ra(sp)
        /*ldq     pv, TFRAME_t12(sp)*/
	addq	sp, TSS_SIZE, sp
	call_pal	PAL_rtsys
	END(TRAP_entSys)

/*
 * Asynchronous trap entrypoints
 */
	
	.align	3
	.globl	TRAP_entInt
	.ent	TRAP_entInt,0
TRAP_entInt:
	.frame	sp,MSS_SIZE,zero
	/*
	 * at this point a0 contains
	 * 1 for clock, 2 for machine check, 3 for io interrupt
	 * 0 for interproc interrupt, 4 for perf count intr.
	 * we need to differentiate between them.
	 */
	addq	sp, -TSS_SIZE, sp
        stq     v0, TFRAME_v0(sp)
        stq     t0, TFRAME_t0(sp)
        stq     t8, TFRAME_t8(sp)
        stq     t9, TFRAME_t9(sp)
        stq     t10,TFRAME_t10(sp)
        stq     t11,TFRAME_t11(sp)
        stq     a3, TFRAME_a3(sp)
        stq     a4, TFRAME_a4(sp)
        stq     a5, TFRAME_a5(sp)
	subq	a0, 1, a5		# 1 is clock
	lda	a0, ALPHA_SPL_HIGH
	call_pal	PAL_swpipl
	bne	a5, notclock
	/*
	 * Clock
	 */
	lda	a5, ENTCLOCK(zero)
	br	TRAP_spinasyncinterrupt
notclock:
        subq	a5, 1, a5		# 2 is machine check
	bne	a5, notmchck
	/*
	 * Machine check
	 */
        mb
        lda     a5, ENTMCHCK(zero)
        br      TRAP_spinasyncinterrupt
notmchck:
        subq	a5, 1, a5		# 3 is io interrupt
	bne	a5, notio
	/*
	 * I/O
	 */
        lda     a5, ENTKNIO(zero)
        br      TRAP_spinasyncinterrupt
notio:
	subq	a5, 1, a5		# 4 is performance monitor intr
	bne	a5, notpfm
	lda	a5, ENTPFM(zero)
        lda	a1, 0(a2)	# a2 holds the counter #. we need it.
        br      TRAP_spinasyncinterrupt
notpfm:		
	lda	a1, 4(a5)		# add back the 4 we subtracted
        lda     a5, ENTSTRAY(zero)	# rest are strays
        br      TRAP_spinasyncinterrupt
        END(TRAP_entInt)

	.globl	TRAP_entArith
	.ent	TRAP_entArith,0
TRAP_entArith:	
	.frame	sp,MSS_SIZE,zero
	addq	sp, -TSS_SIZE, sp
        stq     v0, TFRAME_v0(sp)
        stq     t0, TFRAME_t0(sp)
        stq     t8, TFRAME_t8(sp)
        stq     t9, TFRAME_t9(sp)
        stq     t10,TFRAME_t10(sp)
        stq     t11,TFRAME_t11(sp)
        stq     a3, TFRAME_a3(sp)
        stq     a4, TFRAME_a4(sp)
        stq     a5, TFRAME_a5(sp)
	lda	a5, ENTARITH(zero)
	lda	a3, 0(a2)		
	lda	a2, 0(a1)	
	lda	a1, 0(a0)	
	lda	a0, ALPHA_SPL_HIGH
	call_pal	PAL_swpipl
	br	TRAP_spinasyncinterrupt
	END(TRAP_entArith)

	.globl	TRAP_entMM
	.ent	TRAP_entMM,0
TRAP_entMM:	
	.frame	sp,MSS_SIZE,zero
	addq	sp, -TSS_SIZE, sp
        stq     v0, TFRAME_v0(sp)
        stq     t0, TFRAME_t0(sp)
        stq     t8, TFRAME_t8(sp)
        stq     t9, TFRAME_t9(sp)
        stq     t10,TFRAME_t10(sp)
        stq     t11,TFRAME_t11(sp)
        stq     a3, TFRAME_a3(sp)
        stq     a4, TFRAME_a4(sp)
        stq     a5, TFRAME_a5(sp)
	lda	a5, ENTMM(zero)
	lda	a3, 0(a2)		
	lda	a2, 0(a1)		
	lda	a1, 0(a0)
	lda	a0, ALPHA_SPL_HIGH
	call_pal	PAL_swpipl
	br	TRAP_spinasyncinterrupt
	END(TRAP_entMM)
			
	.globl	TRAP_entIF
	.ent	TRAP_entIF,0
TRAP_entIF:	
	.frame	sp,MSS_SIZE,zero
	addq	sp, -TSS_SIZE, sp
        stq     v0, TFRAME_v0(sp)
        stq     t0, TFRAME_t0(sp)
        stq     t8, TFRAME_t8(sp)
        stq     t9, TFRAME_t9(sp)
        stq     t10,TFRAME_t10(sp)
        stq     t11,TFRAME_t11(sp)
        stq     a3, TFRAME_a3(sp)
        stq     a4, TFRAME_a4(sp)
        stq     a5, TFRAME_a5(sp)
	lda	a5, ENTIF(zero)
	lda	a1, 0(a0)	
	lda	a0, ALPHA_SPL_HIGH
	call_pal	PAL_swpipl
	br	TRAP_spinasyncinterrupt
	END(TRAP_entIF)

	.globl	TRAP_entUna
	.ent	TRAP_entUna,0
TRAP_entUna:
	.frame	sp,MSS_SIZE,zero
	addq	sp, -TSS_SIZE, sp
        stq     v0, TFRAME_v0(sp)
        stq     t0, TFRAME_t0(sp)
        stq     t8, TFRAME_t8(sp)
        stq     t9, TFRAME_t9(sp)
        stq     t10,TFRAME_t10(sp)
        stq     t11,TFRAME_t11(sp)
        stq     a3, TFRAME_a3(sp)
        stq     a4, TFRAME_a4(sp)
        stq     a5, TFRAME_a5(sp)
	lda	a5, ENTUNA(zero)
	lda	a3, 0(a2)		
	lda	a2, 0(a1)		
	lda	a1, 0(a0)
	lda	a0, ALPHA_SPL_HIGH
	call_pal	PAL_swpipl
	br	TRAP_spinasyncinterrupt
	END(TRAP_entUna)
			
/*
 *	Common context save routine for asynchronous interrupts.
 *
 */
	.globl	TRAP_spinasyncinterrupt
	.ent	TRAP_spinasyncinterrupt,0
TRAP_spinasyncinterrupt:
	.frame	sp,MSS_SIZE,zero
        stq     ra, TFRAME_ra(sp)
        stq     t1, TFRAME_t1(sp)
        stq     t2, TFRAME_t2(sp)
        stq     t3, TFRAME_t3(sp)
        stq     t4, TFRAME_t4(sp)
        stq     t5, TFRAME_t5(sp)
        stq     t6, TFRAME_t6(sp)
        stq     t7, TFRAME_t7(sp)
        stq     t12,TFRAME_t12(sp)
/*      stq     s0, TFRAME_s0(sp)
        stq     s1, TFRAME_s1(sp)
        stq     s2, TFRAME_s2(sp)
        stq     s3, TFRAME_s3(sp)
        stq     s4, TFRAME_s4(sp)
        stq     s5, TFRAME_s5(sp)
        stq     s6, TFRAME_s6(sp)*/
        stq     AT, TFRAME_at(sp)
	lda	a0, 0(sp)
	addq	sp, TSS_SIZE, t0
        stq     t0, TFRAME_ksp(sp)
	call_pal PAL_rdusp
        stq     v0, TFRAME_usp(sp)

	/*
	 * find the right handler from the software scb vector
	 */

	/* Get a GP  -- we'll need it to reference softscb */
	br	t0 ,1f
 1:	ldgp	gp,0(t0)

	lda	t1, softscb
	addq	a5, t1, t2
	ldq	pv, 0(t2)
	jsr	ra,(pv)
	END(TRAP_spinasyncinterrupt)
	/* FALL THROUGH */

	/*
	 * Given a trap frame on stack, go back.
	 */
	.globl	TRAP_spin_returnasyncinterrupt
	.ent	TRAP_spin_returnasyncinterrupt,0
TRAP_spin_returnasyncinterrupt:	; 
	.frame	sp,MSS_SIZE,zero
        ldq     a0, TFRAME_usp(sp)
	call_pal PAL_wrusp
        ldq     a3, TFRAME_a3(sp)
        ldq     a4, TFRAME_a4(sp)
        ldq     a5, TFRAME_a5(sp)
        ldq     ra, TFRAME_ra(sp)
        ldq     v0, TFRAME_v0(sp)
        ldq     t0, TFRAME_t0(sp)
        ldq     t1, TFRAME_t1(sp)
        ldq     t2, TFRAME_t2(sp)
        ldq     t3, TFRAME_t3(sp)
        ldq     t4, TFRAME_t4(sp)
        ldq     t5, TFRAME_t5(sp)
        ldq     t6, TFRAME_t6(sp)
        ldq     t7, TFRAME_t7(sp)
        ldq     t8, TFRAME_t8(sp)
        ldq     t9, TFRAME_t9(sp)
        ldq     t10, TFRAME_t10(sp)
        ldq     t11, TFRAME_t11(sp)
        ldq     t12, TFRAME_t12(sp)
/*        ldq     s0, TFRAME_s0(sp)
        ldq     s1, TFRAME_s1(sp)
        ldq     s2, TFRAME_s2(sp)
        ldq     s3, TFRAME_s3(sp)
        ldq     s4, TFRAME_s4(sp)
        ldq     s5, TFRAME_s5(sp)
        ldq     s6, TFRAME_s6(sp)*/
        ldq     AT, TFRAME_at(sp)
	addq	sp, TSS_SIZE, sp
	call_pal	PAL_rti
	END(TRAP_spin_returnasyncinterrupt)

	/*
	 * Given a trap frame, go to it.
	 */
LEAF(RestoreUserGeneralRegs)
        ldq     a1, TFRAME_ksp(a0)	/* set ksp */
	addq	a1, 63, a1
	and	a1, ~63, a1
	addq	a1, -MSS_SIZE, sp

        stq     a0, TFRAME_a0(sp)	/* save away a0 */
        ldq     a0, TFRAME_usp(a0)	/* set usp */
	call_pal PAL_wrusp
        ldq     a0, TFRAME_a0(sp)	/* restore a0 */

	ldq     a1, TFRAME_a0(a0)	/* copy a0-a2 into eframe */
        ldq     a2, TFRAME_a1(a0)
        ldq     a3, TFRAME_a2(a0)
        stq     a1, TFRAME_a0(sp)
        stq     a2, TFRAME_a1(sp)
        stq     a3, TFRAME_a2(sp)

        ldq     a1, TFRAME_gp(a0)	/* copy gp,pc,ps into eframe */
        ldq     a2, TFRAME_pc(a0)
        ldq     a3, TFRAME_ps(a0)
	and	a3, ~0xf, a3		/* ensure that ps is in userland */
	or	a3, 0x8, a3
	
        stq     a1, TFRAME_gp(sp)
        stq     a2, TFRAME_pc(sp)
        stq     a3, TFRAME_ps(sp)


        ldq     a1, TFRAME_a1(a0)	/* get everything else into regs */
        ldq     a2, TFRAME_a2(a0)
        ldq     a3, TFRAME_a3(a0)
        ldq     a4, TFRAME_a4(a0)
        ldq     a5, TFRAME_a5(a0)
        ldq     ra, TFRAME_ra(a0)
        ldq     v0, TFRAME_v0(a0)
        ldq     t0, TFRAME_t0(a0)
	ldq     t1, TFRAME_t1(a0)
        ldq     t2, TFRAME_t2(a0)
        ldq     t3, TFRAME_t3(a0)
        ldq     t4, TFRAME_t4(a0)
        ldq     t5, TFRAME_t5(a0)
        ldq     t6, TFRAME_t6(a0)
        ldq     t7, TFRAME_t7(a0)
        ldq     t8, TFRAME_t8(a0)
        ldq     t9, TFRAME_t9(a0)
        ldq     t10, TFRAME_t10(a0)
        ldq     t11, TFRAME_t11(a0)
        ldq     t12, TFRAME_t12(a0)
        ldq     s0, TFRAME_s0(a0)
        ldq     s1, TFRAME_s1(a0)
        ldq     s2, TFRAME_s2(a0)
        ldq     s3, TFRAME_s3(a0)
        ldq     s4, TFRAME_s4(a0)
        ldq     s5, TFRAME_s5(a0)
        ldq     s6, TFRAME_s6(a0)
	ldq     AT, TFRAME_at(a0) # restore assembler temp
        ldq     a0, TFRAME_a0(a0)
	addq	sp, TSS_SIZE, sp
	call_pal	PAL_rti
	END(RestoreUserRegs)

        .globl coreend
	.ent   coreend,0 # defines the end of spincore code region.
coreend:
	END(coreend) # needed to quiet assembler

LEAF(ChangeSoftSCB)
	sll	a0, 3, t0
	lda	t1, softscb
	addq	t0, t1, t2
	stq	a1, 0(t2)
	RET
	END(ChangeSoftSCB)

LEAF(Wrent)
	call_pal PAL_wrent
	RET
	END(Wrent)

LEAF(mynop)
	or zero,zero,zero
	RET
	END(mynop)

LEAF(SpinSwapIpl)
XLEAF(SetInterruptMask)
XLEAF(RestoreInterruptMask)
	CALL_PROFILER_NO_GP
	call_pal PAL_swpipl
	RET
	END(SpinSwapIpl)

LEAF(setjmp)
XLEAF(_setjmp)
        stq     ra, JB_PC*8(a0)
        stq     sp, JB_SP*8(a0)
        stq     s0, JB_S0*8(a0)
        stq     s1, JB_S1*8(a0)
        stq     s2, JB_S2*8(a0)
        stq     s3, JB_S3*8(a0)
        stq     s4, JB_S4*8(a0)
        stq     s5, JB_S5*8(a0)
        stq     s6, JB_S6*8(a0)
	or	zero,zero,v0
	ret	zero,(ra),1
	END(setjmp)

LEAF(longjmp)	
XLEAF(_longjmp)
        ldq     ra,JB_PC*8(a0)
        ldq     sp,JB_SP*8(a0)
        ldq     s0,JB_S0*8(a0)
        ldq     s1,JB_S1*8(a0)
        ldq     s2,JB_S2*8(a0)
        ldq     s3,JB_S3*8(a0)
        ldq     s4,JB_S4*8(a0)
        ldq     s5,JB_S5*8(a0)
        ldq     s6,JB_S6*8(a0)
	or	a1,zero,v0
	ret	zero,(ra),1
	END(longjmp)

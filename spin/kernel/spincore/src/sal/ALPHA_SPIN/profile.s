/*
 * HISTORY
 * 29-Jul-96  Charles Garrett (garrett) at the University of Washington
 *	Profiling was changed from PC sample timing to Cycle Counter
 *	 timing. This means executing profile code at the beginning and
 *	 end of every profiled procedure. Therefore we have both an
 *	 _mcount and an _mcount_epilogue procedure defined in this file.
 *
 * 18-Apr-96  Charles Garrett (garrett) at the University of Washington
 *	Based on the OSF file mcount.s. Counts every distinct combination
 *	of caller and callee.
 *
 */

/* Patches of notes:

   What the calling convention expects:
	A normal call on the Alpha uses the t12 register for the
	destination procedure and the ra register for the return
	value. A call to the _mcount routine uses the $at register
	for both. After the call to _mcount has finished, the
	caller will use the t12 register to reset its gp. This
	means that _mcount is responsible for setting the t12
	register to the value of the $at register on entry to
	_mcount.

*/
	
#include <machine/asm.h>
#include <machine/regdef.h>
#include <machine/reg.h>
#include "assym.s"

#define PROF_DEBUG 0
	
/* Assembly language programming conventions. I will use mixed case
   words, beginning with a capital letter to indicate variables stored
   in registers. And I will use all capitalized words to indicate
   constant values such as offsets from the stack pointer. */
	
#define SelfPC	ra
#define ATsave  t12
#define Tmp1    t7
#define Tmp2    t8

#define Delta	a2
#define AuxPtr	a3
#define AuxBegin a4
#define IntLev	v0
	
/* Named ra t7 a2 a3 a4 t8 (t9 t10 t11) t12 */
	
			# SaveA0 could be t11
			# t9 is used by the ldwu instruction
			# t9 and t10 are used by the stw instruction

				# Highest interrupt level
#define SPL_HIGH	7

	# Keep these offsets in sync with the ArcInfo structure 
	# declared in ProfileSupport.m3
#define TOS_TIME	8


	# Some stack offsets	
#define SAVE_V0		0
#define SAVE_A0		8
#define SAVE_A1		16
#define SAVE_A2		24
#define SAVE_A3		32
#define SAVE_A4		40
#define SAVE_A5		48
#define SAVE_T0		56
#define SAVE_T1		64
#define SAVE_T2		72
#define SAVE_T3		80
#define SAVE_T4		88
#define SAVE_T5		96
#define SAVE_T6		104
#define SAVE_T7		112
#define SAVE_T8		120
#define SAVE_T9		128
#define SAVE_T10	136
#define SAVE_T11	144
#define SAVE_GP		152
#define SAVE_INTLEV	160
#define SAVE_PV		168
#define FRAME_SIZE	176
	
	# The Auxiliary stack records.
	# The situation when _mcount is called is:

	#		  --------------
	#		  | Callee Arc |
	#		  --------------
	#		  | Callee RA  |
	#		  --------------
	#		  | Callee SP  |
	#		  --------------
	#	          | Caller Arc |
	#		  --------------
	#		  | Caller RA  |
	#		  --------------
	#	AuxPtr -> | Caller SP  |
	#		  --------------

	# At the beginning of _mcount, AuxPtr is incremented to
	# point to the Callee Arc.
	
	# When _mcount_epilogue is called, the situation is 
	# reversed. That is, AuxPtr points to the callee arc
	# and is is decremented after all references which
	# use AuxPtr.
	
#define CALLEE_ARC 16
#define CALLEE_RA  8
#define CALLEE_SP  0
#define RECORD_SZ  24
		
.data
	.align 3
EXPORT(last_counter)
	.quad 0

max_prev:
	.quad 0
max_cycles:
	.quad 0
max_delta:
	.quad 0
	
	# profile_call_graph is a flag indicating whether we are really counting
	# every call-arc.
EXPORT(profile_call_graph)
#ifdef CALL_GRAPH
	.quad 1
#else
	.quad 0
#endif		


IMPORT(aux_stack,8)
IMPORT(profile_flag,8)
			
#define Cycles t2
/* Named ra t2 t7 a2 a3 a4 t8 (t9 t10 t11) t12 */

#ifdef CALL_GRAPH
	
.text
LEAF(_mtest)
	ldgp	gp,0(t12)
	lda	sp,-112(sp)
	stq	a0,64(sp)
	stq	a1,72(sp)
	stq	a2,80(sp)
	stq	a3,88(sp)
	stq	a4,96(sp)
	stq	zero,24(sp)
	stq	zero,40(sp)
	ldq	t0,profile_flag
	lda	sp,112(sp)
	ret	zero,(ra)
	.end _mtest
	
	.globl _mcount
	.ent _mcount 0
_mcount:
	.set		noat
	mov		AT,ATsave		# Save caller's return address
	.set		at
	.frame sp,FRAME_SIZE,ATsave

	.set		noreorder		# easier to debug
		
	/* Before we can use global variables, we must raise SPL and
	   set the gp register. */

	subq		sp,FRAME_SIZE,sp	# Make space on stack
	stq		a0,SAVE_A0(sp)		# registers to be used
	stq		v0,SAVE_V0(sp)		# by call_pal

	mov		SPL_HIGH,a0		# raise interrupt level
	call_pal	PAL_swpipl		# remember old level in v0

	rpcc		Cycles			# Get current cycle count
						# as early as possible

	and		ra,1,Tmp1		# Test for save regs event
	bne		Tmp1,DONT_PROF

	stq		gp,SAVE_GP(sp)		# Caller's gp
	stq		a1,SAVE_A1(sp)		# Argument registers which
	stq		a2,SAVE_A2(sp)		# could be live and get
	stq		a3,SAVE_A3(sp)		# trashed by the call to _gprof.
	stq		a4,SAVE_A4(sp)		
	stq		a5,SAVE_A5(sp)		

	br		Tmp1,DUMMY
DUMMY:						# OSF had a single gp, 
	ldgp		gp,0(Tmp1)		# we have many

	/* Get the current thread's profile stack pointer, stored in
	   ThreadPrivate.curProfileStack. */
	ldq		AuxBegin,aux_stack
	ldq		AuxPtr,0(AuxBegin)	

						# Increment the auxiliary
	addq		AuxPtr,RECORD_SZ,AuxPtr	# as soon as possible, because
	stq		AuxPtr,0(AuxBegin)	# ttd can reenter this code
						# while stepping

	stq		sp,CALLEE_SP(AuxPtr)	# Remember sp
	stq		ra,CALLEE_RA(AuxPtr)	# and ra for the epilogue
	stq		zero,CALLEE_ARC(AuxPtr)	# zero in case there is no arc

	zap		Cycles,0xf0,Cycles	# quash the non cycle count part
	ldq		Delta,last_counter	# Find elapsed time since
	subq		Cycles,Delta,Delta	# last rpcc call
	zap		Delta,0xf0,Delta


	# Call to the C code which updates the profile count and time
	# for the current arc. The prototype for this procedure is:
	# void _gprof(unsigned long ra, unsigned long pc, unsigned long cycles,
	#   	      struct ProfileRec *AuxPtr, struct ProfileRec *AuxBegin)

	# We have already saved the register contents for v0, a0, a2, a3, a4
	# and gp because they are used in this routine. If they are not
	# live across the call to _gprof, then we don't have to save their
	# present values. Unfortunately, v0 is live across the call. It
	# holds the spl level before enterring _mcount. So we save its
	# current value.

	stq		IntLev,SAVE_INTLEV(sp)
	stq		pv,SAVE_PV(sp)		# the jsr instruction

	stq		t0,SAVE_T0(sp)		# This reg is relied on somewhere
	stq		t1,SAVE_T1(sp)		
	stq		t2,SAVE_T2(sp)		
	stq		t3,SAVE_T3(sp)		
	stq		t4,SAVE_T4(sp)		
	stq		t5,SAVE_T5(sp)		
	stq		t6,SAVE_T6(sp)		
	stq		t7,SAVE_T7(sp)		
	stq		t8,SAVE_T8(sp)		
	stq		t9,SAVE_T9(sp)		
	stq		t10,SAVE_T10(sp)		
	stq		t11,SAVE_T11(sp)		

 	mov		ra,a0
	mov		ATsave,a1
	CALL(_gprof)
		
	ldq		t11,SAVE_T11(sp)		
	ldq		t10,SAVE_T10(sp)		
	ldq		t9,SAVE_T9(sp)		
	ldq		t8,SAVE_T8(sp)		
	ldq		t7,SAVE_T7(sp)		
	ldq		t6,SAVE_T6(sp)		
	ldq		t5,SAVE_T5(sp)		
	ldq		t4,SAVE_T4(sp)		
	ldq		t3,SAVE_T3(sp)		
	ldq		t2,SAVE_T2(sp)		
	ldq		t1,SAVE_T1(sp)		
	ldq		t0,SAVE_T0(sp)		
	ldq		pv,SAVE_PV(sp)		
	ldq		IntLev,SAVE_INTLEV(sp)

						# patch the saved return
	lda		SelfPC,_mcount_epilogue	# address to make the procedure
						# return to the _CycleCount
						# routine


	mov		IntLev,a0		# get old interrupt level

	rpcc		Cycles			# Read the counter again as
	zap		Cycles,0xf0,Cycles	#
	stq		Cycles,last_counter	# late as possible
	
	call_pal	PAL_swpipl		# reset interrupt level

	ldq		gp,SAVE_GP(sp)		# Restore caller's GP
	ldq		a5,SAVE_A5(sp)		# and argument registers
	ldq		a4,SAVE_A4(sp)		# 
	ldq		a3,SAVE_A3(sp)		# 
	ldq		a2,SAVE_A2(sp)		# 
	ldq		a1,SAVE_A1(sp)		
	ldq		a0,SAVE_A0(sp)		# 
	ldq		v0,SAVE_V0(sp)		# restore registers
	addq		sp,FRAME_SIZE,sp	# restore stack pointer

	ret		(ATsave)		# Return to caller
						# If we leave t12 alone,
						# the caller can use it to
						# reset its gp.
DONT_PROF:
	bic		ra,1,ra			# Clear the bit from ra
	mov		IntLev,a0		# get old interrupt level
	call_pal	PAL_swpipl		# reset interrupt level
		
	ldq		v0,SAVE_V0(sp)		# restore registers trashed
	ldq		a0,SAVE_A0(sp)		# by PAL_swpipl
	addq		sp,FRAME_SIZE,sp	# restore stack pointer

	ret		(ATsave)		# Return to caller
						# If we leave t12 alone,
						# the caller can use it to
						# reset its gp.
END(_mcount)
	


	
	/**************************************************
	 *****         The Epilogue                   *****
	 **************************************************/

#undef AuxBegin
#undef AuxPtr
#undef Delta

	/* These definitions are better for a leaf routine,
	   since they require no saving and restoring of
	   registers. */
	
#define AuxBegin t3
#define AuxPtr   t4
#define Delta t5
	
LEAF(_mcount_epilogue)
	/* Before we can use global variables, we must raise SPL and
	   set the gp register. */
	subq		sp,FRAME_SIZE,sp
	stq		a0,SAVE_A0(sp)		# registers to be used
	stq		v0,SAVE_V0(sp)		# by call_pal
	stq		gp,SAVE_GP(sp)		# 
	
	mov		SPL_HIGH,a0		# raise interrupt level
	call_pal	PAL_swpipl		# remember old level in v0
	
	rpcc		Cycles			# Check the time

	br		Tmp1,DUMMY2
DUMMY2:						# OSF had a single gp, 
	ldgp		gp,0(Tmp1)		# we have many

	zap		Cycles,0xf0,Cycles	# quash the non cycle count part
	ldq		Delta,last_counter	# calculate the time since
	subq		Cycles,Delta,Delta	# last rpcc call
	zap		Delta,0xf0,Delta

#if PROF_DEBUG
	ldq		Tmp1,max_delta
	cmpule		Tmp1,Delta,Tmp1
	beq		Tmp1,LESS_2
	ldq		Tmp2,last_counter
	stq		Tmp2,max_prev
	stq		Cycles,max_cycles
	stq		Delta,max_delta
LESS_2:	
#endif	
	/* Get the current thread's profile stack pointer, stored in
	   ThreadPrivate.curProfileStack. */
	ldq		AuxBegin,aux_stack
	ldq		AuxPtr,0(AuxBegin)

UNWIND:	
	ldq		Tmp1,CALLEE_SP(AuxPtr)	# Back up the stack until

	cmpeq		sp,Tmp1,Tmp2		# we get to the first record 
	bne		Tmp2,MATCH_FOUND	# with a matching sp.
	subq		AuxPtr,RECORD_SZ,AuxPtr
CONTINUE:	
	jmp		UNWIND

MATCH_FOUND:	
	ldq		ra,CALLEE_RA(AuxPtr)	# Real return address
	ldq		Tmp1,CALLEE_ARC(AuxPtr)	# Place to add total time

	subq		AuxPtr,RECORD_SZ,AuxPtr
	stq		AuxPtr,0(AuxBegin)	# Remember the matching frame

	beq		Tmp1,NO_ARC		# If profiling was off 

	ldq		Tmp2,TOS_TIME(Tmp1)
	addq		Tmp2,Delta,Tmp2		# Combine previous and current
	stq		Tmp2,TOS_TIME(Tmp1)	# times and store total

NO_ARC:			
	mov		IntLev,a0		# get old interrupt level

	rpcc		Cycles
	zap		Cycles,0xf0,Cycles	# quash the non cycle count part
	stq		Cycles,last_counter	# Read the counter as late as
						# possible
	call_pal	PAL_swpipl		# reset interrupt level
		
	ldq		gp,SAVE_GP(sp)		# 
	ldq		v0,SAVE_V0(sp)		# Restore register values
	ldq		a0,SAVE_A0(sp)		# trashed by call_pal
	addq		sp,FRAME_SIZE,sp
	
	ret		(ra)
END(_mcount_epilogue)

#endif  /* CALL_GRAPH */
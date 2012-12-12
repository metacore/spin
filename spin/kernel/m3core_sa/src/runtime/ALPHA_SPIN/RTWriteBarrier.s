 #
 #
 # Copyright 1996 University of Washington
 # All rights reserved.
 # See COPYRIGHT file for a full description
 #
 #
 #
 # HISTORY
 # 10-Dec-97  Tian Fung Lim (tian) at the University of Washington
 #	Added EnqueueAddr
 #
 # 17-Oct-97  Tian Fung Lim (tian) at the University of Washington
 #	Moved COLLECTING global here for faster write barrier.  Shortened
 #	RASes.
 #
 # 27-Jan-97  Wilson Hsieh (whsieh) at the University of Washington
 #	created
 #
 #

/* note :	to get this file recompiled, touch RTWB.s */
	
/* get this offset from RTHeapTM.ic */	
#define WRITEBARRIER 184
#define WRITEBARRIERENQ 200
#define WRITEBARRIERCAS 192
#define WRITEBARRIERDEQ 208
#include <machine/regdef.h>
#include <machine/asm.h>

	.text	
	.align	4
	.set	noreorder
	
	# exported procedures
	.globl AssignTracedToGlobal


	# exported global variables
	.globl TransactionQueue
	.globl TransactionQueueEnd
	.globl COLLECTING

#ifdef WB
	# exported RAS-es
	.globl  UpdateRC
	.globl  UpdateRC_End
	.globl  EnqueueRC1
	.globl  EnqueueRC1_End
	.globl  EnqueueRC2
	.globl  EnqueueRC2_End

	.globl	Enqueue
	.globl	EnqueueAddr
	.globl	EnqueueAddrUpd
	.globl	EnqueueAddrEnd
	.globl	EnqueueUpd
	.globl	EnqueueLog

	.globl	Dequeue
	.globl	DequeueUpd
	.globl	DequeueLog

	.globl	CompareAndSwap
	.globl	CompareAndSwapUpd
	.globl	CompareAndSwapLog

	.globl	LockOrEnqueue
	.globl	LockOrEnqueueUpd1
	.globl	LockOrEnqueueUpd2
	.globl	LockOrEnqueueLog

	.globl	UnlockAndDequeue
	.globl	UnlockAndDequeueUpd1
	.globl	UnlockAndDequeueUpd2
	.globl	UnlockAndDequeueLog
	
#endif WB

	# external data
	.globl	NilOld
	.globl	NilNew
	.globl	OldEqualNew
	.globl	AllocationClock
	.globl	HeapClock
	.globl	HeapTotal
	.globl	HeapTotal2
	.globl	HeapCount
	.globl	LocalClock
	.globl	LocalTotal
	.globl	LocalTotal2
	.globl	LocalCount
	.globl	GlobalClock
	.globl	GlobalTotal
	.globl	GlobalTotal2
	.globl	GlobalCount
	.globl	PossibleHeap
	.globl	PossibleGlobal
	.globl	PossibleLocal

	# procedures
	.globl	SpinAtom
	.globl	TraceRefHeap
	.globl	TraceRefGlobal
	.globl	TraceCount

	.globl	TraceRAS0
	.globl	TraceRAS1
	.globl	TraceRAS2
	.globl	TraceRAS3
	.globl	TraceRAS4
	.globl	TraceRAS5
	.globl	TraceRAS6
	.globl	TraceRAS7
	.globl	TraceRAS8
	.globl	GlobalRAS0
	.globl	GlobalRAS1
	.globl	GlobalRAS2
	.globl	GlobalRAS3
	.globl	GlobalRAS4
	.globl	GlobalRAS5
	.globl	GlobalRAS6
	.globl	GlobalRAS7
	.globl	GlobalRAS8
	.globl	CountRAS1
	.globl	CountRAS2
	.globl	CountRAS3
	.globl	CountRAS4
	.globl	CountRAS5

#ifdef WB

/*
 * Additional code to capture all references and make an upcall to
 * the runtime, for example, for debugging purposes. 
 * Assumptions:	
 *	- up to 3 arguments passed to the procedures being instrumented
 *	- the procedures do not use t9, t10, t11
 * You must modify the macros if you violate these assumptions
 */
	
	#
	# a0 :	void **
	# a1 :	void *
	# atomically store a1 into *a0
	# if object holding a0 is already black
	# then it needs to be marked somehow.
	
	.ent	AssignTracedToGlobal 2
AssignTracedToGlobal:
	ldgp	gp, 0($27)

	.frame  $30, 32, $26,0

	.prologue	1


	# do the assignement
UpdateRC:	
	stq	a1, 0(a0)
UpdateRC_End:
EnqueueRC1:
	
EnqueueRC1_End:
EnqueueRC2:
EnqueueRC2_End:

	# check collecting flag and nil
	ldq	t3, COLLECTING
	# check COLLECTING
	beq	t3,ATTG_End  # this is likely to be the case, put it earlier
	# check referent is not nil
	beq	a1,ATTG_End

	# perform up call
	lda	t2, MI_RTHeapTM
	lda	$30, -32($30)		
	stq	$26,0($30)
	ldq	$27, WRITEBARRIER(t2)

	jsr	$26, ($27), 0

	ldq	$26, 0($30)
	addq	$30, 32,$30
ATTG_End:	
	ret	zero, (ra), 1
	.end	AssignTracedToGlobal


	
		
#define NEXT_OFFSET   8         /* offset of the next field in FastList.T */

	/* a0 = elem
	   NEXT_OFFSET(a0) = elem.next
	   a1 =  original head
	*/
	/* PROCEDURE Enqueue(elem: T; list: REF T);
	 * BEGIN
	 *   elem.nextelem := list;
	 *   list := elem;
	 * END;
	 */

	.ent	Enqueue 2
Enqueue:

	ldgp	gp, 0($27)

	.frame  $30, 40, $26,0
	.prologue	1

	# the following 3 instructions are a RAS
EnqueueUpd:
	# perform the enqueueing
	ldq	a2, 0(a1)		/* elem.next := head */
	stq	a2, NEXT_OFFSET(a0)
	stq	a0, 0(a1)		/* head := elem *//* RHS */
EnqueueUpd_End:

EnqueueLog:
EnqueueLog_End:
	# check collecting flag and nil
	ldq	t3, COLLECTING
	beq	t3,Enq_End  # this is likely to be the case, put it earlier
	beq	a2,Enq_End  # quit if nil

	
	lda	$30, -48($30)		
	stq	$26,0($30)
	
	lda	t2, MI_RTHeapTM
	ldq	$27, WRITEBARRIER(t2)

	/* we need to gray a2 (the old head) if a0 was already black */
	/* XXX do we need to gray a0 ?  what if dest(old head) was black? */
	mov	a2, a1
	jsr	$26, ($27), 0

	/* XXX graying new thing as well,in case head was already blackened */	
	mov	a0, a1
	jsr	$26, ($27), 0

	ldq	$26, 0($30)
	addq	$30, 48,$30
Enq_End:		
	ret	zero, (ra), 1
	.end	Enqueue


	/*
		ldq	a3, 0(a0)
		stq	a3, 0(a2)
		stq	a1, 0(a0)
		RET

	*/

	.ent	EnqueueAddr 2
EnqueueAddr:

	ldgp	gp, 0($27)

	.frame  $30, 40, $26,0
	.prologue	1

	# the following 3 instructions are a RAS
EnqueueAddrUpd:
	# perform the enqueueing
	ldq	a3, 0(a0)
	stq	a3, 0(a2)
	stq	a1, 0(a0)
EnqueueAddrEnd:
	# check collecting flag and nil
	ldq	t3, COLLECTING
	beq	t3,EnqAddr_End  # this is likely to be the case, put it earlier
	beq	a2,EnqAddr_End  # quit if nil

	
	lda	$30, -48($30)		
	stq	$26,0($30)
	
	lda	t2, MI_RTHeapTM
	ldq	$27, WRITEBARRIER(t2)

	# a1 is already elem
	jsr	$26, ($27), 0

	# gray old head
	mov	a3, a1
	jsr	$26, ($27), 0

	ldq	$26, 0($30)
	addq	$30, 48,$30
EnqAddr_End:		
	ret	zero, (ra), 1
	.end	EnqueueAddr

	
	#
	#
	#
	/*
	PROCEDURE Dequeue(list: REF T) : T;
	 * BEGIN
	 *   t := head;
	 *   IF t = NIL THEN RETURN NIL; END;
	 *   head := head.nextelem;
	 *   RETURN t;
	 * END;
	*/
	
	.ent	Dequeue 2
Dequeue:

	ldgp	gp, 0($27)

	.frame  $30, 32, $26,0
	.prologue	1

DequeueUpd:
	# perform the dequeueing
	ldq	v0, 0(a0)		/* res := head */ /* LHS */
	beq	v0, out			/* if head = NIL THEN RETURN */
	ldq	a2, NEXT_OFFSET(v0)	/* head := head.next */ /* RHS */
	stq	a2, 0(a0)
DequeueUpd_End:
DequeueLog:
DequeueLog_End:
	# check collecting flag and nil
	ldq	t3, COLLECTING
	beq	t3,out

	
	lda	t2, MI_RTHeapTM
	
	lda	$30, -32($30)		
	stq	$26,0($30)
	stq	v0, 24($30)

	
	ldq	$27, WRITEBARRIERDEQ(t2)
	# need to change a1 a0 to appropriate args
	mov	v0, a0
	mov	a2, a1

	jsr	$26, ($27), 0

	ldq	$26, 0($30)
	ldq	v0, 24($30)

	addq	$30, 32,$30

out:

	ret	zero, (ra), 1
	.end	Dequeue

	#
	#
	#
	
	.ent	CompareAndSwap
CompareAndSwap:


	.frame  $30, 32, $26,0

	.prologue 1

	# do the conditional update
CompareAndSwapUpd:	
	ldq	t0, 0(a0)       /* load current value *//* LHS */
	cmpeq	t0, a1, v0      /* see if it changed, set the return value */
	beq	v0, fail3	/* if changed, return false */
	stq	a2, 0(a0)	/* do the update *//* RHS */
CompareAndSwapUpd_End:
CompareAndSwapLog:
CompareAndSwapLog_End:
	# check collecting flag and nil
	ldq	t3, COLLECTING
	beq	t3,fail3  # this is likely to be the case, put it earlier
	beq	a1,fail3
	lda	t2, MI_RTHeapTM
	
	lda	$30, -32($30)		
	stq	$26,0($30)
	

	ldq	$27, WRITEBARRIER(t2)
	mov	a2, a1
	ldq	a2, 0($30)
	jsr	$26, ($27), 0

	ldq	$26, 0($30)
	addq	$30, 32,$30
fail3:

	
	ret	zero, (ra), 1
	.end	CompareAndSwap

	#
	# (VAR lock : INTEGER, strand:	Strand.T):	BOOLEAN
	#
	
#define MUNEXTOFFSET   NEXT_OFFSET /* use the next field of FastList.T */

	.ent	LockOrEnqueue



	.frame  $30, 48, $26,0

	.prologue	1
/*
  PROCEDURE LockOrEnqueue(VAR lock: INTEGER; strand: Strand.T) : BOOLEAN;
	IF lock # 0 THEN
	  IF lock-1 = 0 THEN
            strand.next = lock-1
	  ELSE
	    strand.next = lock
	  END
	  lock = strand
	ELSE
	  lock = 1
	  RET TRUE
	END  
*/
LockOrEnqueue:
	
LockOrEnqueueUpd1:
	ldq	t0,0(a0)
	bne	t0,already_held	/* already set, go to enqueue */
lock_free:		
	or	zero,1,v0	/* build new lock value */
	stq	v0,0(a0)
LockOrEnqueueUpd1_End:
	/* restore stack, no references changed */
	/*	addq	$30, 40,$30*/
	ret	zero, (ra), 1
LockOrEnqueueUpd2:
restart_already_held:	
	ldq	t0,0(a0)		/* reread for RAS begin */
	beq	t0,lock_free		/* unlocked after we rolled back */
already_held:
	# make two assignements, but ref-count only the net result (one)

	subq	t0,1,t0		/* are we the first to arrive ? */
	beq	t0,no_other_waiters	/* if so, skip over */
	addq	t0,1,t0		/* undo the damage to the waiter ptr */
no_other_waiters:		
	stq	t0,MUNEXTOFFSET(a1) /* store next pointer value */
	stq	a1,0(a0)	    /* RHS */
LockOrEnqueueUpd2_End:
	# check collecting flag and nil
	ldq	t3, COLLECTING
	beq	t3,loe_end  # this is likely to be the case, put it earlier
	
	lda	$30, -48($30)		
	stq	$26,0($30)

	/* force both a1, the element being inserted, and
	   the next element to be grayed */
	mov	t0, a0
	
	lda	t2, MI_RTHeapTM
	ldq	$27, WRITEBARRIERDEQ(t2)
	jsr	$26, ($27), 0

	ldq	$26, 0($30)

	addq	$30, 48,$30
loe_end :	
	mov	zero,v0		/* failed to acquire lock */
	ret	zero, (ra), 1 /*	RET*/
LockOrEnqueueLog:
LockOrEnqueueLog_End:	
LockOrEnqueue_End:
	
	.end	LockOrEnqueue


	#
	#
	#
	.extern unlock_unlocked

	.ent	UnlockAndDequeue


	.frame  $30, 48, $26,0

	.prologue	1
	/*
	PROCEDURE UnlockAndDequeue(VAR lock: INTEGER) : INTEGER;
	  IF lock -1 = 0 THEN
	    lock = 0
	    RET 0
	  ELSE
	    if lock = 0 then goto badthingtodo
	    t2 = lock (orig strand )
	    if lock.next # 0 then
		lock = lock.next
	        ret t2
	    else
	      lock = 1
	      ret t2
	    end
	  END
	*/
UnlockAndDequeue:
UnlockAndDequeueUpd1:
	ldq	t0,0(a0)		/* get the lock value */ /* LHS */
	subq	t0,1,t1			/* were we the exclusive owner ? */
	bne	t1,unlock_shared	/* if not, do the dequeue */
unlock_exclusive:	
	stq	zero,0(a0)		/* write new value */
UnlockAndDequeueUpd1_End:
	mov	zero,v0
	RET
UnlockAndDequeueUpd2:	
restart_unlock_shared:
	ldq	t0,0(a0)		/* reread for RAS begin */
	subq	t0,1,t1			/* were we the exclusive owner ? */
	beq	t1,unlock_exclusive	/* if so, just unlock */
	
unlock_shared:
	beq	t0,badthingtodo	      /* unlocking an unheld mutex */
	ldq	t2,MUNEXTOFFSET(t0)   /* t2 := strand.munext *//* RHS */
	bne	t2,notlaststrand      /* is this the last strand */
UnlockAndDequeueUpd2_End:
	
	/*bis	t2, t2, t3	*/	/* FIXME */
	or	zero,1,t2	      /* turn it into an exclusive lock */
	/*bis	zero, zero, t3	*/	/* FIXME */
notlaststrand:
	stq	t2,0(a0)	      /* mulock := t2 */
	mov	t0,v0		      /* return strand */

	# check collecting flag and nil
	ldq	t3, COLLECTING
	beq	t3,uad_end  # this is likely to be the case, put it earlier

	/* upcall to runtime */

	lda	$30, -48($30)

	stq	$26,0($30)
	stq	v0, 32($30)	


	/*	mov	t0, a0  strand queue */ /* a0 is already the queue */
	mov	t0,a0  /* make sure the newly dequeued thing is grayed */
	mov	t2, a1 /* make sure next strand will be accounted for */
	lda	t2, MI_RTHeapTM
	ldq	$27, WRITEBARRIERDEQ(t2)
	jsr	$26, ($27), 0


	ldq	$26,0($30)
	ldq	v0, 32($30)	

	addq	$30, 48,$30
uad_end:	
	RET
badthingtodo:
	jsr     unlock_unlocked
				      /* no one should do this */
	mov	zero,v0		      /* put breakpoint here to catch */
	RET
UnlockAndDequeueLog:
UnlockAndDequeueLog_End:	
UnlockAndDequeue_End:	
	.end	UnlockAndDequeue

#endif WB

#if !defined(WB)
	 
	#
	# Some bogus code so that the right symbols are defined
	# and do not point into anything being used.  If some reference
	# counting is called with this on, we will jump to zero and die.
	# It should be at least 5 instructions because these symbols
	# define RAS-es.
	#
AssignTracedToGlobal:	
AssignKnown:	
UpdateRC:
#endif !defined(WB)

#if !defined(WB)
EnqueueLog:
DequeueLog:
CompareAndSwapLog:
EnqueueRC1:
EnqueueRC2:
	bis	ra, ra, a0
	jsr	ra, shouldnotrefcount
	bis	ra, ra, a0
	jsr	ra, shouldnotrefcount
	bis	ra, ra, a0
	jsr	ra, shouldnotrefcount
#endif !defined(WB)

	.ent	TraceRefHeap 2
TraceRefHeap:
	#	a0 :	void **
	#	a1 :	void *
	#	a2 :	typecode
	#	a3 :	BOOLEAN
	#	atomically store a1 into *a0
	#	return old value
	ldgp	gp, 0($27)
	.frame	sp, 0, $26, 0
	.prologue	1
	# update memory location
TraceRAS0:	
	ldq	$0, 0(a0)
	stq	a1, 0(a0)
	lda	$24, NilOld
	bne	$0, OldNotNil
TraceRAS1:	
	#	Old is nil
	ldq	$23, 0 ($24)	# load old value, NilOld
	addq	$23, 1, $23
	stq	$23, 0 ($24)	# store new value, NilOld
OldNotNil:
	bne	a1, NewNotNil
TraceRAS2:	
	#	New is nil
	ldq	$23, 8 ($24)	# load old value, NilNew
	addq	$23, 1, $23
	stq	$23, 8 ($24)	# store new value, NilNew
NewNotNil:
	cmpeq	$0, a1, $23
	bne	$23, TraceRAS4
TraceRAS3:	
	#	Old = New
	ldq	$23, 16 ($24)	# load old value, OldEqualNew
	addq	$23, 1, $23
	stq	$23, 16 ($24)	# store new value, OldEqualNew
TraceRAS4:
	ldq	$23, 24 ($24)	# load old value, AllocationClock
	ldq	$22, 32 ($24)	# load old value, HeapClock
	stq	$23, 32 ($24)	# AllocationClock -> HeapClock
	subq	$23, $22, $23	# diff
TraceRAS5:
	ldq	$22, 40 ($24)	# HeapTotal
	addq	$22, $23, $22	# add
	stq	$22, 40 ($24)	# store back
	mulq	$23, $23, $23	# compute diff squared
TraceRAS6:
	ldq	$22, 48 ($24)	# HeapTotal2
	addq	$22, $23, $22	# increment
	stq	$22, 48 ($24)	# store back
TraceRAS7:
	ldq	$23, 56 ($24)	# HeapCount
	addq	$23, 1, $23	# increment
	stq	$23, 56 ($24)	# store back
	bne	a3, TraceRefDone
TraceRAS8:
	ldq	$23, 128 ($24)	# PossibleHeap
	addq	$23, 1, $21	# increment
	stq	$23, 128 ($24)	# store back
TraceRefDone:	
	#	finished
	ret	$31, ($26), 1
	.end	TraceRefHeap
	#
	#
	#
	.ent	TraceRefGlobal 2
TraceRefGlobal:	
	#	a0 :	void **
	#	a1 :	void *
	#	a2 :	typecode
	#	a3 :	BOOLEAN
	#	atomically store a1 into *a0
	#	return old value
	ldgp	gp, 0($27)
	.frame	sp, 0, $26, 0
	.prologue	1
	# update memory location
GlobalRAS0:	
	ldq	$0, 0(a0)
	stq	a1, 0(a0)
	lda	$24, NilOld
	bne	$0, OldNotNilG
GlobalRAS1:	
	#	Old is nil
	ldq	$23, 0 ($24)	# load old value, NilOld
	addq	$23, 1, $23
	stq	$23, 0 ($24)	# store new value, NilOld
OldNotNilG:
	bne	a1, NewNotNilG
GlobalRAS2:	
	#	New is nil
	ldq	$23, 8 ($24)	# load old value, NilNew
	addq	$23, 1, $23
	stq	$23, 8 ($24)	# store new value, NilNew
NewNotNilG:
	cmpeq	$0, a1, $23
	bne	$23, GlobalRAS4
GlobalRAS3:	
	#	Old = New
	ldq	$23, 16 ($24)	# load old value, OldEqualNew
	addq	$23, 1, $23
	stq	$23, 16 ($24)	# store new value, OldEqualNew
GlobalRAS4:
	ldq	$23, 24 ($24)	# load old value, AllocationClock
	ldq	$22, 96 ($24)	# load old value, GlobalClock
	stq	$23, 96 ($24)	# AllocationClock -> GlobalClock
	subq	$23, $22, $23	# diff
GlobalRAS5:
	ldq	$22, 104 ($24)	# GlobalTotal
	addq	$23, $22, $22	# add
	stq	$22, 104 ($24)	# store back
	mulq	$23, $23, $23	# compute diff squared
GlobalRAS6:
	ldq	$22, 112 ($24)	# GlobalTotal2
	addq	$22, $23, $22	# increment
	stq	$22, 112 ($24)	# store back
GlobalRAS7:
	ldq	$23, 120 ($24)	# GlobalCount
	addq	$23, 1, $23	# increment
	stq	$23, 120 ($24)	# store back
	bne	a3, TraceRefGlobalDone
GlobalRAS8:
	ldq	$23, 136 ($24)	# PossibleGlobal
	addq	$23, 1, $23	# increment
	stq	$23, 136 ($24)	# store back
TraceRefGlobalDone:
	# done
	ret	$31, ($26), 1
	.end	TraceRefGlobal
	#
	#
	#
	.ent	TraceCount 2
TraceCount:
	#	a0 :	typecode
	#	a1 :	BOOLEAN
	ldgp	gp, 0($27)
	.frame	sp, 0, $26, 0
	.prologue	1
	lda	$24, NilOld
CountRAS1:
	ldq	$23, 24 ($24)	# load old value, AllocationClock
	ldq	$22, 64 ($24)	# load old value, LocalClock
	stq	$23, 64 ($24)	# AllocationClock -> LocalClock
	subq	$23, $22, $23	# diff
CountRAS2:
	ldq	$22, 72 ($24)	# LocalTotal
	addq	$22, $23, $22	# add
	stq	$22, 72 ($24)	# store back
	mulq	$23, $23, $23	# compute diff squared
CountRAS3:
	ldq	$22, 80 ($24)	# LocalTotal2
	addq	$22, $23, $22	# increment
	stq	$22, 80 ($24)	# store back
CountRAS4:
	ldq	$23, 88 ($24)	# LocalCount
	addq	$23, 1, $23	# increment
	stq	$23, 88 ($24)	# store back
	bne	a3, TraceCountDone
CountRAS5:
	ldq	$23, 144 ($24)	# PossibleLocal
	addq	$23, 1, $23	# increment
	stq	$23, 144 ($24)	# store back
TraceCountDone:
	# done
	ret	$31, ($26), 1
	.end	TraceCount

	#
	# the variable holding the pointer to the end of the
	# transaction queue, this variable is updated by 
	# procedures in this file and by the collector routine
	# which cleans up the queue.
	#
	.data
COLLECTING:
	.quad	0
TransactionQueue:
	.quad	0

TransactionQueueEnd:
	.quad	0

NilOld:			# offsets from NilOld
	.quad	0
NilNew:			# 8
	.quad	0
OldEqualNew:		# 16
	.quad	0
AllocationClock:	# 24
	.quad	0
HeapClock:		# 32
	.quad	0
HeapTotal:		# 40
	.quad	0
HeapTotal2:		# 48
	.quad	0
HeapCount:		# 56
	.quad	0		
LocalClock:		# 64
	.quad	0
LocalTotal:		# 72
	.quad	0
LocalTotal2:		# 80
	.quad	0
LocalCount:		# 88
	.quad	0
GlobalClock:		# 96
	.quad	0
GlobalTotal:		# 104
	.quad	0
GlobalTotal2:		# 112
	.quad	0
GlobalCount:		# 120
	.quad	0
PossibleHeap:		# 128
	.quad	0
PossibleGlobal:		# 136
	.quad	0
PossibleLocal:		# 144
	.quad	0

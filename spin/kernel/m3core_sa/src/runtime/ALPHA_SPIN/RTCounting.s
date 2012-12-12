 #
 #
 # Copyright 1996 University of Washington
 # All rights reserved.
 # See COPYRIGHT file for a full description
 #
 #
 #
 # HISTORY
 # 19-Nov-97  Tian Fung Lim (tian) at the University of Washington
 #	Added COLLECTING flag for treadmill collectors.
 #
 # 27-Jan-97  Wilson Hsieh (whsieh) at the University of Washington
 #	created
 #
 #

#include <machine/regdef.h>
#include <machine/asm.h>

	.text	
	.align	4

	# exported procedures
	.globl AssignTracedToGlobal
	.globl AssignKnown

	# exported global variables
	.globl COLLECTING
	.globl TransactionQueue
	.globl TransactionQueueEnd

#ifdef REFCOUNT
	# exported RAS-es
	.globl  UpdateRC
	.globl  UpdateRC_End
	.globl  EnqueueRC1
	.globl  EnqueueRC1_End
	.globl  EnqueueRC2
	.globl  EnqueueRC2_End

	.globl	Enqueue
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
	
#endif REFCOUNT

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

#ifdef REFCOUNT

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
	# put old and new values onto TransactionQueue
	#
	
	.ent	AssignTracedToGlobal 2
AssignTracedToGlobal:
	ldgp	gp, 0(t12)
	.frame	sp, 0, ra, 0
	.prologue	1

	# get the current pointer to the transacton queue
	lda	a3, TransactionQueue

	# do the assignement
UpdateRC:
	ldq	t0, 0(a0)
	stq	a1, 0(a0)
UpdateRC_End:

	# update the log
	# depend on sentinel page at the end of queue to trap the overflow
EnqueueRC1:
	ldq	t1, 0(a3)
	addq	t1, 16, t2
	stq	t0, 0(t1)
	stq	a1, 8(t1)
	stq	t2, 0(a3)
EnqueueRC1_End:

	ret	zero, (ra), 1
	.end	AssignTracedToGlobal

	#
	# a0 :	void *
	# a1 :	void *
	#
	# enqueue a record to decrement a0 and increment a1
	# without making the assignement
	#
	
	.ent AssignKnown
AssignKnown:	
	ldgp	gp, 0(t12)
	.frame	sp, 0, ra, 0
	.prologue	1

	# get the current pointer to the transacton queue
	lda	a3, TransactionQueue

	# update the log
	# depend on sentinel page at the end of queue to trap the overflow
EnqueueRC2:
	ldq	t1, 0(a3)
	addq	t1, 16, t2
	stq	a0, 0(t1)
	stq	a1, 8(t1)
	stq	t2, 0(a3)
EnqueueRC2_End:
	
	ret	zero, (ra), 1
	.end AssignKnown

	#
	#
	#
	
#define NEXT_OFFSET   8         /* offset of the next field in FastList.T */

	.ent	Enqueue 2
Enqueue:
	ldgp	gp, 0(t12)
	.frame	sp, 0, ra, 0
	.prologue	1

	# get the current pointer to the transacton queue
	lda	a3, TransactionQueue

	# make two assignements, but ref-count only the net result (one)
EnqueueUpd:
	# perform the enqueueing
	ldq	t0, NEXT_OFFSET(a0)	/* LHS */
	ldq	t1, 0(a1)		/* elem.next := head */
	stq	t1, NEXT_OFFSET(a0)
	stq	a0, 0(a1)		/* head := elem *//* RHS */
EnqueueUpd_End:

	# update transaction queue pointer and store into queue
	# depend on sentinel page at the end of queue to trap the overflow
EnqueueLog:
	ldq	t1, 0(a3)
	addq	t1, 16, t2
	stq	t0, 0(t1)
	stq	a0, 8(t1)
	stq	t2, 0(a3)
EnqueueLog_End:

	ret	zero, (ra), 1
	.end	Enqueue

	#
	# FIXME: needs to be implemented
	#
	.globl	EnqueueAddr
EnqueueAddr:
	ldq	a3, 0(a0)
	stq	a3, 0(a2)
	stq	a1, 0(a0)
	RET
EnqueueAddrEnd:	

	#
	#
	#
	
	.ent	Dequeue 2
Dequeue:
	ldgp	gp, 0(t12)
	.frame	sp, 0, ra, 0
	.prologue	1

	# get the current pointer to the transacton queue
	lda	a3, TransactionQueue
	
DequeueUpd:
	# perform the dequeueing
	ldq	v0, 0(a0)		/* res := head */ /* LHS */
	beq	v0, out			/* if head = NIL THEN RETURN */
	ldq	a1, NEXT_OFFSET(v0)	/* head := head.next */ /* RHS */
	stq	a1, 0(a0)
DequeueUpd_End:

	# update transaction queue pointer and store into queue
	# depend on sentinel page at the end of queue to trap the overflow
DequeueLog:
	ldq	t1, 0(a3)
	addq	t1, 16, t2
	stq	v0, 0(t1)
	stq	a1, 8(t1)
	stq	t2, 0(a3)
DequeueLog_End:
out:
	ret	zero, (ra), 1
	.end	Dequeue

	#
	#
	#
	
	.ent	CompareAndSwap
CompareAndSwap:
	
	# get the current pointer to the transacton queue
	lda	a3, TransactionQueue

	# do the conditional update
CompareAndSwapUpd:	
	ldq	t0, 0(a0)       /* load current value *//* LHS */
	cmpeq	t0, a1, v0      /* see if it changed, set the return value */
	beq	v0, fail3	/* if changed, return valse */
	stq	a2, 0(a0)	/* do the update *//* RHS */
CompareAndSwapUpd_End:

	# if store done, update transaction queue pointer and store into queue
	# depend on sentinel page at the end of queue to trap the overflow
CompareAndSwapLog:
	ldq	t1, 0(a3)
	addq	t1, 16, t2
	stq	t0, 0(t1)
	stq	a2, 8(t1)
	stq	t2, 0(a3)
CompareAndSwapLog_End:
fail3:	
	ret	zero, (ra), 1
	.end	CompareAndSwap

	#
	#
	#
	
#define MUNEXTOFFSET   NEXT_OFFSET /* use the next field of FastList.T */

	.ent	LockOrEnqueue
LockOrEnqueue:
LockOrEnqueueUpd1:
	ldq	t0,0(a0)
	bne	t0,already_held	/* already set, go to enqueue */
	or	zero,1,v0	/* build new lock value */
	stq	v0,0(a0)
LockOrEnqueueUpd1_End:
	ret	zero, (ra), 1
	
already_held:
	# make two assignements, but ref-count only the net result (one)
LockOrEnqueueUpd2:
	subq	t0,1,t0		/* are we the first to arrive ? */
	beq	t0,no_other_waiters	/* if so, skip over */
	addq	t0,1,t0		/* undo the damage to the waiter ptr */
no_other_waiters:		
	ldq	t2,MUNEXTOFFSET(a1) /* store next pointer value *//* LHS */
	stq	t0,MUNEXTOFFSET(a1) /* store next pointer value */
	stq	a1,0(a0)	    /* RHS */
LockOrEnqueueUpd2_End:

	# get the current pointer to the transacton queue
	lda	a3, TransactionQueue

	# update transaction queue pointer and store into queue
	# depend on sentinel page at the end of queue to trap the overflow
LockOrEnqueueLog:
	ldq	t1, 0(a3)
	addq	t1, 16, t2
	stq	t2, 0(t1)
	stq	a1, 8(t1)
	stq	t2, 0(a3)
LockOrEnqueueLog_End:

	mov	zero,v0		/* failed to acquire lock */
	RET
LockOrEnqueue_End:
	.end	LockOrEnqueue


	#
	#
	#
		
	.ent	UnlockAndDequeue
UnlockAndDequeue:
UnlockAndDequeueUpd1:
	ldq	t0,0(a0)		/* get the lock value */ /* LHS */
	subq	t0,1,t1			/* were we the exclusive owner ? */
	bne	t1,unlock_shared	/* if not, do the dequeue */
	stq	zero,0(a0)		/* write new value */
UnlockAndDequeueUpd1_End:
	mov	zero,v0
	RET
unlock_shared:
UnlockAndDequeueUpd2:
				      /* we'll be forgiving and accept */
	beq	t0,badthingtodo	      /* unlocking an unheld mutex */
	ldq	t2,MUNEXTOFFSET(t0)   /* t2 := strand.munext *//* RHS */
	bne	t2,notlaststrand      /* is this the last strand */
UnlockAndDequeueUpd2_End:
	
	bis	t2, t2, t3		/* FIXME */
	or	zero,1,t2	      /* turn it into an exclusive lock */
	bis	zero, zero, t3		/* FIXME */
notlaststrand:
	stq	t2,0(a0)	      /* mulock := t2 */

	# get the current pointer to the transacton queue
	lda	a3, TransactionQueue

	# update transaction queue pointer and store into queue
	# depend on sentinel page at the end of queue to trap the overflow
UnlockAndDequeueLog:
	ldq	t1, 0(a3)
	addq	t1, 16, t2
	stq	t0, 0(t1)
	stq	t3, 8(t1)
	stq	t2, 0(a3)
UnlockAndDequeueLog_End:	
	
	mov	t0,v0		      /* return strand */
	RET
badthingtodo:
				      /* no one should do this */
	mov	zero,v0		      /* put breakpoint here to catch */
	RET
UnlockAndDequeue_End:	
	.end	UnlockAndDequeue

#endif REFCOUNT

#if !defined(REFCOUNT)
	 
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
#endif !defined(REFCOUNT)

#if !defined(REFCOUNT)
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
#endif !defined(REFCOUNT)

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

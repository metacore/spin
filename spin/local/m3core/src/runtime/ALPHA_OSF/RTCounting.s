 #
 #
 # Copyright 1996 University of Washington
 # All rights reserved.
 # See COPYRIGHT file for a full description
 #
 #
 #
 # HISTORY
 # 27-Jan-97  Wilson Hsieh (whsieh) at the University of Washington
 #	created
 #
 #
	.text	
	.align	4
	# external data
	.globl	TransactionQueue
	# procedures
#	.globl	SpinAtom
#	.globl	AssignTracedToGlobal
	.globl	AssignKnown
	# for RAS
	.globl  EnqueueRC
	.globl  EnqueueRCNew
	#
	#
	#
	.ent	AssignTracedToGlobal 2
SpinAtom:	
AssignTracedToGlobal:
	#	$16 :	void **
	#	$17 :	void *
	#	atomically store $17 into *$16
	#	put old and new values onto TransactionQueue
	ldgp	$gp, 0($27)
	.frame	$sp, 0, $26, 0
	.prologue	1
	lda	$18, TransactionQueue
again:
	# update memory location
	ldq_l	$0, 0($16)
	bis	$17, $17, $19
	stq_c	$19, 0($16)
	beq	$19, repeat
	ret	$31, ($26), 1
EnqueueRC:
	# global label for use in RAS region
	# update transaction queue pointer and store into queue
	# depend on sentinel page at the end of queue
	ldq	$1, 0($18)
	addq	$1, 16, $2
	stq	$0, 0($1)
	stq	$17, 8($1)
	stq	$2, 0($18)
	ret	$31, ($26), 1
repeat:
	br	again
	.end	AssignTracedToGlobal
	#
	#
	#
	.ent AssignKnown
AssignKnown:	
	#	$16 :	void *
	#	$17 :	void *
	#	enqueue a record to decrement $16, increment $17
	ldgp	$gp, 0($27)
	.frame	$sp, 0, $26, 0
	.prologue	1
	lda	$18, TransactionQueue
	ret	$31, ($26), 1
EnqueueRCNew:
	# global label for use in RAS region
	# update transaction queue pointer and store into queue
	# depend on sentinel page at the end of queue
	ldq	$1, 0($18)
	addq	$1, 16, $2
	stq	$16, 0($1)
	stq	$17, 8($1)
	stq	$2, 0($18)
	ret	$31, ($26), 1
	.end AssignKnown
	#
	#
	#
	.data
TransactionQueue:
	.quad	0
	
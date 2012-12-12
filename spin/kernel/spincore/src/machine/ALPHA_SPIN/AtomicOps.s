/*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 */
/* HISTORY
 * 26-Mar-96  Przemek Pardyak (pardy) at the University of Washington
 *	Fixed bugs in LockOrEnqueue and UnlockAndDequeue.  If the second
 *	atomic section was rolled back, the lock was not re-tested for
 *	led to blocking on an empty lock and deadlocks.  Also skipped
 *	re-reading and re-testing of lock value on the first entry to
 *	the second RAS (they need to be executed only on roll-back).
 *
 * 19-Mar-96  Przemek Pardyak (pardy) at the University of Washington
 *	Made some procedures conditional on reference counting.
 *
 * 10-Mar-97  Emin Gun Sirer (egs) at the University of Washington
 *	Reread all regs at the beginning of a RAS region.
 *
 * 29-Jul-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed LLSC_TryLock and RAS_TryLock to use a return value of one
 *	(1) to indicate success rather than two (2).  Added more comments.
 *
 * 23-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Added 32bits atomic increment and decrement routines.
 *
 * 13-Aug-95  Emin Gun Sirer (egs) at the University of Washington
 *	Added RAS based linked-lists.
 *
 * 28-Nov-94  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Fast locks based on RAS, slow locks based on LL/SC.
 */
	
/* THE RAS REGIONS ARE SET UP IN MachineAtomicOps.M3 - Begin pc's and lengths
 * are entered into the master table in that file which must match
 * the implementation here.
 */

#define ASSEMBLER 1
#include <alpha/asm.h>
#include <alpha/reg.h>
#include <alpha/regdef.h>
#include "profile.h"
	.text
	.align	4
	.set	noreorder

/*
 * These fields must match the corresponding fields in the respective
 * data structures
 */
#define NEXT_OFFSET    8         /* offset of the next field in FastList.T */
#define MUNEXTOFFSET   NEXT_OFFSET /* use the next field of FastList.T */

/*
 * Spinlocks using load-linked/store-conditional. Multi compatible.
 */
LEAF(LLSC_TryLock)
	or	zero,1,v0		/* build lock value */
	ldq_l	t0,0(a0)
	bne	t0,lfail		/* already set, forget it */
	stq_c	v0,0(a0)		/* see if we still had the lock */
	beq	v0,lnostore		/* if we just took an interrupt.. */
success:	
#if (NCPUS > 1)
	mb				/* start critical section now */
#endif
	RET
lfail:
	mov	zero,v0			/* failed to acquire lock */
	RET
lnostore:
	br	zero,LLSC_TryLock	/* I love branch predictions.. */
	END(LLSC_TryLock)

/* All the routines below are used as Restartable Atomic Sequences (RAS).
 * Their correctness depends on their start pc address AND the number of
 * instructions that need to be atomic wrt interrupts and preemption.
 * The MachineAtomicOps module enters these procedures into a coderegion
 * table using the {start_pc,instruction_count} tuple.  Changing any of
 * the routines below may require that the MachineAtomicOps module be
 * updated, too.
 */

/*
 * Trylock a spinlock or mutex using RAS.
 */
LEAF(RAS_TryLock)
	ldq	t0,0(a0)
	xor	t0,1,v0			/* build lock value and result */
	bne	t0,rastrylockfail	/* already set, forget it */
	stq	v0,0(a0)
	RET
rastrylockfail:
	mov	zero,v0
	RET
END(RAS_TryLock)

/*
 * Unlock a spinlock. stq's are atomic.
 */
LEAF(Unlock)
#if (NCPUS > 1)
	mb			/* end critical section now */
#endif
	stq	zero,0(a0)	/* no need for interlocks (sec 10.5.2) */
	RET
END(Unlock)

/*
 * Blocking lock of a mutex using RAS.
 */

#ifndef REFCOUNT
LEAF(LockOrEnqueue)
	ldq	t0,0(a0)
	bne	t0,already_held		/* already set, go to enqueue */
lock_free:	
	or	zero,1,v0		/* build new lock value */
	stq	v0,0(a0)
	RET
restart_already_held:			/* execute only if restarted */
	ldq	t0,0(a0)		/* reread for RAS begin */
	beq	t0,lock_free		/* unlocked after we rolled back */
already_held:
	subq	t0,1,t0			/* are we the first to arrive ? */
	beq	t0,no_other_waiters	/* if so, skip over */
	addq	t0,1,t0			/* undo the damage to the waiter ptr */
no_other_waiters:		
	stq	t0,MUNEXTOFFSET(a1)	/* store next pointer value */
	stq	a1,0(a0)
	mov	zero,v0			/* failed to acquire lock */
	RET
END(LockOrEnqueue)
#endif REFCOUNT
	
/*
 * Unlocking of a blocking mutex using RAS.
 */
#ifndef REFCOUNT
	.extern	unlock_unlocked
	
LEAF(UnlockAndDequeue)
	ldq	t0,0(a0)		/* get the lock value */
	subq	t0,1,t1			/* were we the exclusive owner ? */
	bne	t1,unlock_shared	/* if not, do the dequeue */
unlock_exclusive:
	stq	zero,0(a0)		/* write new value */
	mov	zero,v0
	RET
restart_unlock_shared:			/* execute only if restarted */
	ldq	t0,0(a0)		/* reread for RAS begin */
	subq	t0,1,t1			/* were we the exclusive owner ? */
	beq	t1,unlock_exclusive	/* if so, just unlock */
unlock_shared:	
					/* we'll be forgiving and accept */
	beq	t0,badthingtodo		/* unlocking an unheld mutex */
	ldq	t2,MUNEXTOFFSET(t0)	/* t2 := strand.munext */
	bne	t2,notlaststrand	/* is this the last strand */
	or	zero,1,t2		/* turn it into an exclusive lock */
notlaststrand:	
	stq	t2,0(a0)		/* mulock := t2 */
	mov	t0,v0			/* return strand */
	RET
badthingtodo:
	jsr	unlock_unlocked
					/* no one should do this */
	mov	zero,v0			/* put breakpoint here to catch */
	RET
END(UnlockAndDequeue)
#endif REFCOUNT

/*
 * RAS based linked list enqueue
 */
#ifndef REFCOUNT

LEAF(Enqueue)
	ldq	a2, 0(a1)		/* elem.next := head */
	stq	a2, NEXT_OFFSET(a0)
	stq	a0, 0(a1)		/* head := elem */
	RET
END(Enqueue)

LEAF(EnqueueAddr)
	ldq	a3, 0(a0)
	stq	a3, 0(a2)
	stq	a1, 0(a0)
	RET
END(EnqueueAddr)	

#endif

/*
 * RAS based linked list dequeue
 */
#ifndef REFCOUNT
LEAF(Dequeue)
	ldq	v0, 0(a0)		/* res := head */
	beq	v0, out			/* if head = NIL THEN RETURN */
	ldq	a2, NEXT_OFFSET(v0)	/* head := head.next */
	stq	a2, 0(a0)
out:	RET	
END(Dequeue)
	
#endif

/*
 * RAS based linked list delete
 * Delete(prev, elem) leaves prev pointing to elem->next, thus
 * eliding elem out of the linked list. If someone inserts an
 * element between prev and next during a delete, delete fails.
 */
LEAF(Delete)
	ldq	a2, NEXT_OFFSET(a0)	/* x := prev.next */
	cmpeq	a2, a1, a3		/* if next != x then fail */
	beq	a3, fail
	ldq	v0, NEXT_OFFSET(a1)	/* next := elem.next */
	stq	v0, NEXT_OFFSET(a0)	/* prev.next := next */
	RET
fail:	lda	v0, 0(zero)
	RET
END(Delete)

/*
 * RAS based atomic inc
 */
LEAF(AtomicInc)
	ldq	v0, 0(a0)
	addq	v0, a1, v0
	stq	v0, 0(a0)
	RET	
END(AtomicInc)

/*
 * RAS based atomic dec
 */
LEAF(AtomicDec)
	ldq	v0, 0(a0)
	subq	v0, a1, v0
	stq	v0, 0(a0)
	RET	
END(AtomicDec)


/*
 * RAS based atomic inc
 */
LEAF(AtomicInc32)
	ldl	v0, 0(a0)
	addl	v0, a1, v0
	stl	v0, 0(a0)
	RET	
END(AtomicInc32)

/*
 * RAS based atomic dec
 */
LEAF(AtomicDec32)
	ldl	v0, 0(a0)
	subl	v0, a1, v0
	stl	v0, 0(a0)
	RET	
END(AtomicDec32)

/*
 * RAS based compare and swap for references
 */
#ifndef REFCOUNT
LEAF(CompareAndSwap)
	ldq	t0, 0(a0)       /* load current value */
	cmpeq	t0, a1, v0      /* see if it changed, set the return value */
	beq	v0,fail3	/* if changed, return valse */
	stq	a2, 0(a0)	/* do the update */
fail3:
	RET
END(CompareAndSwap)
#endif

/*
 * RAS based compare and swap for integers. On the alpha, the two are
 * identical, since both are 64-bits.
 */
LEAF(CompareAndSwapInt)
	ldq	t0, 0(a0)       /* load current value */
	cmpeq	t0, a1, v0      /* see if it changed, set the return value */
	beq	v0,fail4	/* if changed, return valse */
	stq	a2, 0(a0)	/* do the update */
fail4:
	RET
END(CompareAndSwapInt)

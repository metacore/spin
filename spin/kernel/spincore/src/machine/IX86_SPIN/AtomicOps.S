/*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 10-Mar-97  Emin Gun Sirer (egs) at the University of Washington
 *	Fixed a comparison to use the right condition code.
 *
 * 09-Mar-97  Wilson Hsieh (whsieh) at the University of Washington
 *	fix typos in LockAndEnqueue, UnlockAndDequeue
 *
 * 25-Feb-97  Emin Gun Sirer (egs) at the University of Washington
 *	Added blocking locks and made spinlocks faster.
 *	Got rid of CMU code.
 *
 * 24-Feb-97  Emin Gun Sirer (egs) at the University of Washington
 * 	On the x86, the RAS region boundaries included the final store at the
 * 	end of a RAS region. Hence, the whole region, along with its side
 *      effects, could be reexecuted twice with disasterous consequences if
 *      the system was interrupted at the wrong time.
 * 
 */

#include <machine/asmacros.h>

#define NEXT_OFFSET   4         /* offset of the next field in FastList.T */
#define MUNEXTOFFSET  NEXT_OFFSET /* offset of the field used for mutexes */
	.text
/*
 * Spinlock Unlock
 */
ENTRY(Unlock)
	movl	4(%esp),%ecx		/* point at mutex */
	movl	$0,%eax			/* set unlocked value in acc */
	xchg	%eax,(%ecx)		/* swap with mutex */
					/* xchg with memory is automatically */
					/* locked */
	ret
/*
 * Spinlock and mutex trylock
 */
ENTRY(TryLock)
	movl		4(%esp),%ecx	/* ecx points at mutex */
	movl		$0,%eax		/* set expected lock value in acc */
	movl		$1,%edx		/* set new lock value in edx */
	lock
	cmpxchgl	%edx,(%ecx)	/* place a 1 if lock is unheld */
					/* */
	jne		failure		/* already set ? */
					/* jump if zf = 0 */
	movl		$1,%eax		/* return true */
	ret
failure:
	movl		$0,%eax		/* return false */
	ret

/*
 * Blocking locks. 
 *
 * Signature: LockOrEnqueue(lock, thread)
 */
ENTRY(LockOrEnqueue)
	movl		4(%esp),%ecx	/* ecx points at mutex */
	movl		$0,%eax		/* set expected lock value in acc */
	movl		$1,%edx		/* set new lock value in edx */
	lock
	cmpxchgl	%edx,(%ecx)	/* swap with mutex if lock is unheld */
	jne		already_held	/* already set, go to enqueue */
	movl		$1,%eax		/* return true */
	ret

already_held:
.global _LockOrEnqueue_RASbegin
_LockOrEnqueue_RASbegin:
	movl		(%ecx),%eax	/* reread in case RAS bumps us back */
	cmpl		$1,%eax		/* are we the first waiter ? */
	jne		other_waiters	/* if not, skip over */
	movl		$0,%eax		/* put a zero in the next field */
other_waiters:	
	movl		8(%esp),%edx		/* edx points at strand */
	movl		%eax,MUNEXTOFFSET(%edx) /* store next pointer value */
	movl		%edx,(%ecx)
.global _LockOrEnqueue_RASend
_LockOrEnqueue_RASend:
	movl		$0,%eax		/* failed to acquire lock */
	ret

/*
 * Blocking locks. 
 *
 * Unlock(lock) : thread
 * 
 * Returns the first waiter queued up on this lock.
 */
ENTRY(UnlockAndDequeue)
	movl		4(%esp),%ecx	/* ecx points at mutex */
	movl		$1,%eax
	movl		$0,%edx
	lock				/* put zero in the lock if the lock */
	cmpxchgl	%edx,(%ecx)	/* was exclusively locked */
	jne		unlock_shared_or_zero
	movl		$0,%eax
	ret

unlock_shared_or_zero:
	movl		4(%esp),%eax	/* eax points at mutex */
.global _UnlockAndDequeue_RASbegin
_UnlockAndDequeue_RASbegin:
	movl		(%eax),%ecx	/* reread in case RAS bumps us back*/
	jecxz		badthingtodo	/* unlocking an unheld mutex */
	movl	        MUNEXTOFFSET(%ecx),%edx   /* edx := strand.munext */
	cmpl		$0,%edx
	jnz		notlaststrand    /* is this the last strand */
	movl		$1,%edx		 /* turn it into an exclusive lock */
notlaststrand:	
	xchg		%edx,(%eax)	 /* mulock := t2 */
.global _UnlockAndDequeue_RASend
_UnlockAndDequeue_RASend:
	movl		%ecx,%eax	 /* return strand */
	ret
badthingtodo:			         /* no one should do this */
	xorl		%eax,%eax	 /* put breakpoint here to catch */
	ret

/*
 * RAS based linked list enqueue
 */
ENTRY(Enqueue)
	movl	4(%esp),%eax
	movl	8(%esp),%ecx
	movl	0(%ecx), %edx		/* elem.next := head */
	movl	%edx, NEXT_OFFSET(%eax)
.global _Enqueue_end
_Enqueue_end:
	movl	%eax, 0(%ecx)		/* head := elem */
	ret
	
ENTRY(EnqueueAddr)
	movl	4(%esp),%eax
	movl	12(%esp),%ecx
	movl	0(%eax), %edx
	movl	%edx, 0(%ecx)
	movl	8(%esp), %ecx
.global _EnqueueAddr_end
_EnqueueAddr_end:
	movl	%ecx, 0(%eax)
	ret

/*
 * RAS based linked list dequeue
 */
ENTRY(Dequeue)
	movl	4(%esp),%ecx
	movl	0(%ecx), %eax		/* res := head */
	cmpl	$0, %eax
	jz	out		        /* if head = NIL THEN RETURN */
	movl	NEXT_OFFSET(%eax), %edx	/* head := head.next */
.global _Dequeue_end
_Dequeue_end:
	movl	%edx, 0(%ecx)
out:	ret

ALTENTRY(AtomicInc32)
ENTRY(AtomicInc)
	movl	4(%esp),%ecx		/* point at value to be incremented */
	movl	8(%esp),%edx		/* get delta */
	lock				/* atomically load old value into a */
	xadd	%edx,0(%ecx)            /* register and increment it in */
	                                /* memory */
	movl	8(%esp),%eax	
	add	%edx,%eax		/* increment the in-register copy */
	ret

ALTENTRY(AtomicDec32)
ENTRY(AtomicDec)
	lea	8(%esp),%ecx            /* negate the value of delta */
	negl	0(%ecx)
	jmp	_AtomicInc		/* call AtomicInc */


ALTENTRY(CompareAndSwapInt)
ENTRY(CompareAndSwap)
	movl	4(%esp),%ecx
	movl	8(%esp),%eax
	movl	12(%esp),%edx
	lock
	cmpxchgl %edx,0(%ecx)
	jne	fail2
	movl	$1,%eax
	ret
fail2:
	movl	$0,%eax
	ret

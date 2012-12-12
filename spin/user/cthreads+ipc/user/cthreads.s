#include <alpha/regdef.h>
#include <alpha/asm.h>
		
/* XXX The syscall numbers should not be hard-coded. */

	/*
	 * cthread_t cthread_fork(long (*func)(void *), void *arg)
	 *
	 * The in-kernel handler expects func in a0, arg in a1,
	 *                        gp for func in a2, cleaner in a3.
	 *                        sp to use in a4.
	 */
NESTED(nopsyscall, 0, zero)
	lda v0, 0(zero)
	call_pal PAL_callsys
	RET
	END(nopsyscall)
NESTED(xferbytes, 0, zero)
	lda v0, 2(zero)
	call_pal PAL_callsys
	RET
	END(xferbytes)

/* layered cthreads */
NESTED(cthread_layered_fork, 5, zero)
	ldgp a2, 0(a0)
	lda  a3, cthread_layered_exit
	lda v0, 40(zero)
	call_pal PAL_callsys
	RET
	END(cthread_layered_fork)

NESTED(cthread_layered_exit, 1, zero)
	lda	a0, 0(v0)
	lda	v0, 41(zero)
	call_pal PAL_callsys
loop:
	lda a0, 0xceee(zero)
	lda v0, 6(zero)        #print
	call_pal PAL_callsys
	br	loop	
	END(cthread_layered_exit)

NESTED(cthread_layered_join, 1, zero)
	lda	v0, 42(zero)
	call_pal PAL_callsys
	RET
	END(cthread_layered_join)

NESTED(yield, 0, zero)
	lda	v0, 43(zero)
	call_pal PAL_callsys
	RET
	END(yield)

/* Native strands */
NESTED(cthread_strand_fork, 4, zero)
	ldgp a2, 0(a0)
	lda  a3, cthread_strand_exit
	lda v0, 150(zero)
	call_pal PAL_callsys
	RET
	END(cthread_strand_fork)

NESTED(cthread_strand_exit, 1, zero)
	lda	a0, 0(v0)
	lda	v0, 151(zero)
	call_pal PAL_callsys
loop2:	br	loop2
	END(cthread_strand_exit)

NESTED(cthread_strand_join, 1, zero)
	lda	v0, 152(zero)
	call_pal PAL_callsys
	RET
	END(cthread_strand_join)

NESTED(cthread_yield, 0, zero)
	lda	v0, 153(zero)
	call_pal PAL_callsys
	RET
	END(cthread_yield)

NESTED(spin_sbrk, 1, zero)
	lda	v0, 69(zero)
	call_pal PAL_callsys
	RET
	END(spin_sbrk)

/* domain related syscalls */
NESTED(download, 2, zero)
	lda	v0, 10(zero)
	call_pal PAL_callsys
	RET
	END(download)
	
NESTED(drop, 2, zero)
	lda	v0, 11(zero)
	call_pal PAL_callsys
	RET
	END(drop)

/* sync related syscalls */
NESTED(cond_alloc, 0, zero)
	lda	v0, 140(zero)
	call_pal PAL_callsys
	RET
	END(cond_alloc)
	
NESTED(cond_signal, 1, zero)
	lda	v0, 141(zero)
	call_pal PAL_callsys
	RET
	END(cond_signal)
	
NESTED(condition_wait, 2, zero)
	lda	v0, 142(zero)
	call_pal PAL_callsys
	RET
	END(condition_wait)
	
NESTED(cond_broadcast, 0, zero)
	lda	v0, 143(zero)
	call_pal PAL_callsys
	RET
	END(cond_broadcast)
	
/* rpc related syscalls */
NESTED(rpcrequest, 0, zero)
	lda	v0, 155(zero)
	call_pal PAL_callsys
	RET
	END(rpcrequest)

NESTED(rpcregister, 0, zero)
	lda	v0, 156(zero)
	call_pal PAL_callsys
	RET
	END(rpcregister)

NESTED(rpcreturn, 1, zero)
	lda	v0, 157(zero)
	call_pal PAL_callsys
	RET
	END(rpcreturn)
	
NESTED(rpcrequest2, 2, zero)
	lda	v0, 158(zero)
	call_pal PAL_callsys
	RET
	END(rpcrequest2)

NESTED(rpcregister2, 2, zero)
	lda	v0, 159(zero)
	call_pal PAL_callsys
	RET
	END(rpcregister2)

NESTED(rpcreturn2, 3, zero)
	lda	v0, 160(zero)
	call_pal PAL_callsys
	RET
	END(rpcreturn2)

#include <alpha/regdef.h>
#include <alpha/asm.h>
		
NESTED(rasregister, 4, zero)
	addq sp, -8, sp
	stq  gp, 0(sp)
	lda  a0, atomicinc
	lda  a1, endatomicinc
	subq a1, a0, a1
	lda v0, 145(zero)
	call_pal PAL_callsys
	ldq  gp, 0(sp)

	lda  a0, mutex_trylock
	lda  a1, endmutex_trylock
	subq a1, a0, a1
	lda v0, 146(zero)
	call_pal PAL_callsys
	addq sp, 8, sp
	RET
	END(cthread_layered_fork)

NESTED(atomicinc, 1, zero)
	ldq	a1, 0(a0)
	addq	a1, 1, a1
	stq	a1, 0(a0)
endatomicinc:
	RET
	END(atomicinc)

NESTED(mutex_trylock, 1, zero)
	ldq	t0,0(a0)
	bne	t0,fail2	/* already set, forget it */
	or	zero,2,v0	/* build lock value */
	stq	v0,0(a0)
endmutex_trylock:	
	RET
fail2:
	mov	zero,v0		/* failed to acquire lock */
	RET
	END(mutex_trylock)

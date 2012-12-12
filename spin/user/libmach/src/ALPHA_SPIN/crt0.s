#include <alpha/regdef.h>
#include <alpha/asm.h>
		
NESTED(start, 0, zero)
	br	t0,1f
1:	ldgp	gp, 0(t0)
	lda	t12, main
	jsr	ra, (t12), 1
	lda     v0, 1(zero)           /* exit */
	call_pal PAL_callsys
	/* should not be reached */
	lda	a0, 0xbeefbeeffeebfeeb
	lda	v0, 6(zero)           /* write something */
	call_pal PAL_callsys
	RET
	END(start)

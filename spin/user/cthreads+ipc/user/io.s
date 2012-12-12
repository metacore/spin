#include <alpha/regdef.h>
#include <alpha/asm.h>
		
NESTED(printchar, 1, zero)
	lda v0, 5(zero)
	call_pal PAL_callsys
	RET
	END(printchar)

NESTED(sysprintd, 1, zero)
	lda v0, 6(zero)
	call_pal PAL_callsys
	RET
	END(sysprintd)

NESTED(timingprint, 1, zero)
	lda v0, 7(zero)
	call_pal PAL_callsys
	RET
	END(timingprint)

LEAF(readtimer)
        rpcc    $0
        RET
        END(readtimer)

LEAF(readtimerv)
        rpcc    $0
	and	$0, 0xffffffff, $0
        RET
        END(readtimerv)

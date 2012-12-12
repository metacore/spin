#include <asm.h>
#include <regdef.h>
#define PRINT 6
#define NOP       0
#define NULLSYS   5
#define NCOUNT	100000000
	
	.text
	.align 4
NESTED(main, 0, zero)
	lda  s0,0(zero)
loop2:	
	br loop2
	RET
	END(main)

#include <asm.h>
#include <regdef.h>
#define PRINT 6
	
	.text
	.align 4
NESTED(main, 0, zero)
	lda  gp,0xdead(zero)
loop:	
	subq gp,0xdead,t5
	bne  t5, print
	br loop
print:	
	addq gp, zero, a0
	lda v0, PRINT(zero)		# print
	call_pal 0x83
	RET
	END(main)

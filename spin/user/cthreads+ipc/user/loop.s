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
	lda  t5,NCOUNT(zero)
loop1:	
	subq t5,1,t5
	cmplt t5,0,t6
	beq   t6, loop1

	addq s0,zero, a0
	lda v0,PRINT(zero)		# print
	call_pal 0x83

	addq s0,1,s0
	br loop2
	RET
	END(main)

#include <asm.h>
#include <regdef.h>
#define PRINT 6
#define NOP       0
#define NULLSYS   5
#define NCOUNT	100
	
	.text
	.align 4
NESTED(main, 0, zero)
	rpcc s0			# start timer
	lda  t5,NCOUNT(zero)
loop1:	
	subq t5,1,t5		#  we measure the loop overhead here
	cmplt t5,0,t6
	beq   t6, loop1
	
	rpcc  s1		# end timer

	addq s0,zero, a0	# get the start time into a0
	lda v0,PRINT(zero)		# print
	call_pal 0x83
	addq s1,zero, a0	# get the end time into a0
	lda v0,PRINT(zero)		# print
	call_pal 0x83

	
	rpcc s0			# start timer
	lda  t5,NCOUNT(zero)
loop2:	
	lda v0,NOP(zero)		#  we do nop traps in this loop
	call_pal 0x83

	subq t5,1,t5
	cmplt t5,0,t6
	beq   t6, loop2
	
	rpcc  s1		# end timer

	addq s0,zero, a0	# get the start time into a0
	lda v0,PRINT(zero)		# print
	call_pal 0x83
	addq s1,zero, a0	# get the end time into a0
	lda v0,PRINT(zero)		# print
	call_pal 0x83

	rpcc s0			# start timer
	lda  t5,NCOUNT(zero)
loop3:	
	lda v0,NULLSYS(zero)		#  we do nop traps in this loop
	call_pal 0x83

	subq t5,1,t5
	cmplt t5,0,t6
	beq   t6, loop3
	
	rpcc  s1		# end timer

	addq s0,zero, a0	# get the start time into a0
	lda v0,PRINT(zero)		# print
	call_pal 0x83
	addq s1,zero, a0	# get the end time into a0
	lda v0,PRINT(zero)		# print
	call_pal 0x83
	RET
	END(main)

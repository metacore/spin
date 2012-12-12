/*
 * reads the cycle counter on the alphas
 * 
 * courtesy egs@cs.washington.edu
 *	
 */

	.file 2 "cycles.s"
	.text
	.align 3
	.globl ReadCycles
	.ent ReadCycles 0
ReadCycles:
	rpcc $0
	sll $0, 32, $1
	addq $0, $1, $0
	srl $0, 32, $0
	ret $31, ($26), 1
	.end

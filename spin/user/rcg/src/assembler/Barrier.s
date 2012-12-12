#include <machine/asm.h>
#include <machine/regdef.h>
#include <machine/reg.h>

	.text	
	.align 3
	.set noat
	.set noreorder

LEAF(InstructionMemoryBarrier)
	call_pal PAL_imb
	RET
END(InstructionMemoryBarrier)


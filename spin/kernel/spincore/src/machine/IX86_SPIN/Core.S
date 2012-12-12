/*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 */
#include <machine/asmacros.h>

#define TF_VEC 0
#define TF_PPL 0x4
#define TF_ES 0x8
#define TF_DS 0xc
#define TF_EDI 0x10
#define TF_ESI 0x14
#define TF_EBP 0x18
#define TF_ISP 0x1c
#define TF_EBX 0x20
#define TF_EDX 0x24
#define TF_ECX 0x28
#define TF_EAX 0x2c
#define TF_TRAPNO 0x30
#define TF_ERR 0x34
#define TF_EIP 0x38
#define TF_CS 0x3c
#define TF_EFLAGS 0x40
#define TF_ESP 0x44
#define TF_SS 0x48
#define TF_SIZE 0x4c

#define PCB_ESP0 0x4
#define PCB_SS0 0x8

#define     KDSEL           0x10 
/* XXX Preemption fix */
#define IO_ICU1	0x020	/* 8259A Int Controller #1 */
#define IO_ICU2	0x0A0	/* 8259A Int Controller #2 */
#define CLOCK_INT	0x0000
	
#define IRQ_BYTE(irq_num)       ((irq_num) / 8)
#define IRQ_BIT(irq_num)        (1 << ((irq_num) % 8))
	
	.globl	_clk_int
_clk_int:	.long	0
	
	.globl	_corebegin
_corebegin:

	
ENTRY(SaveAllGeneralRegs)	
	pushl	%eax
	movl	8(%esp),%eax
	movl	%ebx,TF_EBX(%eax)
	movl	%ecx,TF_ECX(%eax)
	movl	%edx,TF_EDX(%eax)
	movl	%esp,TF_ISP(%eax)
	movl	%ebp,TF_EBP(%eax)
	movl	%edi,TF_EDI(%eax)
	movl	%esi,TF_ESI(%eax)
	popl	%ecx
	movl	%ecx,TF_EAX(%eax)
	movl	_cpl,%ecx
	movl	%ecx,TF_PPL(%eax)
	movl	8(%esp),%ecx
	movl	%ecx,TF_EIP(%eax)
	movl	$1,%eax
	ret
	
/*
 * Save callee-save registers
 */
ENTRY(SaveCalleeSaveGeneralRegs)
	movl	4(%esp),%eax
	movl	0(%esp),%ecx
	movl	%ecx,TF_EIP(%eax)	
	movl	%ebx,TF_EBX(%eax)
	movl	%esp,TF_ISP(%eax)
	movl	%ebp,TF_EBP(%eax)
	movl	%edi,TF_EDI(%eax)
	movl	%esi,TF_ESI(%eax)
	movl	_cpl,%ecx
	movl	%ecx,TF_PPL(%eax)
	movl	$1,%eax
	ret

/*
 * Restore callee-save registers
 */
ENTRY(RestoreCalleeSaveGeneralRegs)	
	movl	4(%esp),%eax	
	movl	TF_EBX(%eax),%ebx
	movl	TF_ISP(%eax),%esp
	movl	TF_EBP(%eax),%ebp
	movl	TF_EDI(%eax),%edi
	movl	TF_ESI(%eax),%esi
	pushl	TF_EIP(%eax)
	movl	TF_PPL(%eax),%ecx
	movl	%ecx,_cpl
/* Add for preemption fix */
	cmpl	$0,_clk_int
	je	1f
	movl	$0,_clk_int
	cli
	movb	_imen + IRQ_BYTE(CLOCK_INT), %al
	andb	$~IRQ_BIT(CLOCK_INT),%al
	movb	%al, _imen + IRQ_BYTE(CLOCK_INT)
	outb	%al, $IO_ICU1 + 1	
	sti	
/* end preemption fix */
1:	call	_splz
	movl	$0,%eax
	ret

ENTRY(RestoreUserGeneralRegs)
	movl 4(%esp),%eax
	movl TF_ISP(%eax),%ecx
	movl _curpcb,%edx
	movl %ecx,PCB_ESP0(%edx)
	movl $KDSEL,%ecx
	movl %ecx,PCB_SS0(%edx)
	addl $8,%eax
	cli
	movl %eax,%esp
	popl %es
	popl %ds
	popal
	addl $8,%esp
	iret
	
	.globl _coreend
_coreend:

ALTENTRY(FlushDataCache)
ENTRY(FlushInstructionCache)
	wbinvd
	ret

/* rdtsc places the high order bits in %edx and the low in %eax.
   %eax is the return value register and %edx is caller saved, so
   we don't have to do anything. */
ENTRY(RdtscLow)
	.byte	0x0f, 0x31	# RDTSC
	ret






/*-
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	$Id: support.s,v 1.1.1.1 1996/08/15 03:22:22 fgray Exp $
 */

#include "assym.s"				/* system definitions */
#include "errno.h"				/* error return codes */
#include "machine/asmacros.h"			/* miscellaneous asm macros */
#include "machine/cputypes.h"			/* types of CPUs */

#define KDSEL		0x10			/* kernel data selector */
#define IDXSHIFT	10

/*
 * Support routines for GCC, general C-callable functions
 */
ENTRY(__udivsi3)
	movl 4(%esp),%eax
	xorl %edx,%edx
	divl 8(%esp)
	ret

ENTRY(__divsi3)
	movl 4(%esp),%eax
	cltd
	idivl 8(%esp)
	ret

/*
 * Support for reading real time clock registers
 */
ENTRY(rtcin)					/* rtcin(val) */
	movl	4(%esp),%eax
	outb	%al,$0x70
	NOP
	xorl	%eax,%eax
	inb	$0x71,%al
	NOP
	ret

/*
 * bcopy family
 */

/*
 * void bzero(void *base, u_int cnt)
 * Special code for I486 because stosl uses lots
 * of clocks.  Makes little or no difference on DX2 type
 * machines, but stosl is about 1/2 as fast as
 * memory moves on a standard DX !!!!!
 */
ALTENTRY(blkclr)
ENTRY(bzero)
#if defined(I486_CPU)
	cmpl	$CPUCLASS_486,_cpu_class
	jz	1f
#endif

	pushl	%edi
	movl	8(%esp),%edi
	movl	12(%esp),%ecx
	xorl	%eax,%eax
	shrl	$2,%ecx
	cld
	rep
	stosl
	movl	12(%esp),%ecx
	andl	$3,%ecx
	rep
	stosb
	popl	%edi
	ret

#if defined(I486_CPU)
	SUPERALIGN_TEXT
1:
	movl	4(%esp),%edx
	movl	8(%esp),%ecx
	xorl	%eax,%eax
/
/ do 64 byte chunks first
/
/ XXX this is probably over-unrolled at least for DX2's
/
2:
	cmpl	$64,%ecx
	jb	3f
	movl	%eax,(%edx)
	movl	%eax,4(%edx)
	movl	%eax,8(%edx)
	movl	%eax,12(%edx)
	movl	%eax,16(%edx)
	movl	%eax,20(%edx)
	movl	%eax,24(%edx)
	movl	%eax,28(%edx)
	movl	%eax,32(%edx)
	movl	%eax,36(%edx)
	movl	%eax,40(%edx)
	movl	%eax,44(%edx)
	movl	%eax,48(%edx)
	movl	%eax,52(%edx)
	movl	%eax,56(%edx)
	movl	%eax,60(%edx)
	addl	$64,%edx
	subl	$64,%ecx
	jnz	2b
	ret

/
/ do 16 byte chunks
/
	SUPERALIGN_TEXT
3:
	cmpl	$16,%ecx
	jb	4f
	movl	%eax,(%edx)
	movl	%eax,4(%edx)
	movl	%eax,8(%edx)
	movl	%eax,12(%edx)
	addl	$16,%edx
	subl	$16,%ecx
	jnz	3b
	ret

/
/ do 4 byte chunks
/
	SUPERALIGN_TEXT
4:
	cmpl	$4,%ecx
	jb	5f
	movl	%eax,(%edx)
	addl	$4,%edx
	subl	$4,%ecx
	jnz	4b
	ret

/
/ do 1 byte chunks
/ a jump table seems to be faster than a loop or more range reductions
/
/ XXX need a const section for non-text
/
	SUPERALIGN_TEXT
jtab:
	.long	do0
	.long	do1
	.long	do2
	.long	do3

	SUPERALIGN_TEXT
5:
	jmp	jtab(,%ecx,4)

	SUPERALIGN_TEXT
do3:
	movw	%ax,(%edx)
	movb	%al,2(%edx)
	ret

	SUPERALIGN_TEXT
do2:
	movw	%ax,(%edx)
	ret

	SUPERALIGN_TEXT
do1:
	movb	%al,(%edx)

	SUPERALIGN_TEXT
do0:
	ret
#endif /* I486_CPU */

/* fillw(pat, base, cnt) */
ENTRY(fillw)
	pushl	%edi
	movl	8(%esp),%eax
	movl	12(%esp),%edi
	movl	16(%esp),%ecx
	cld
	rep
	stosw
	popl	%edi
	ret

/* filli(pat, base, cnt) */
ENTRY(filli)
	pushl	%edi
	movl	8(%esp),%eax
	movl	12(%esp),%edi
	movl	16(%esp),%ecx
	cld
	rep
	stosl
	popl	%edi
	ret

ENTRY(bcopyb)
bcopyb:
	pushl	%esi
	pushl	%edi
	movl	12(%esp),%esi
	movl	16(%esp),%edi
	movl	20(%esp),%ecx
	cmpl	%esi,%edi			/* potentially overlapping? */
	jnb	1f
	cld					/* nope, copy forwards */
	rep
	movsb
	popl	%edi
	popl	%esi
	ret

	ALIGN_TEXT
1:
	addl	%ecx,%edi			/* copy backwards. */
	addl	%ecx,%esi
	std
	decl	%edi
	decl	%esi
	rep
	movsb
	popl	%edi
	popl	%esi
	cld
	ret

ENTRY(bcopyw)
bcopyw:
	pushl	%esi
	pushl	%edi
	movl	12(%esp),%esi
	movl	16(%esp),%edi
	movl	20(%esp),%ecx
	cmpl	%esi,%edi			/* potentially overlapping? */
	jnb	1f
	shrl	$1,%ecx				/* copy by 16-bit words */
	cld					/* nope, copy forwards */
	rep
	movsw
	adc	%ecx,%ecx			/* any bytes left? */
	rep
	movsb
	popl	%edi
	popl	%esi
	ret

	ALIGN_TEXT
1:
	addl	%ecx,%edi			/* copy backwards */
	addl	%ecx,%esi
	andl	$1,%ecx				/* any fractional bytes? */
	decl	%edi
	decl	%esi
	std
	rep
	movsb
	movl	20(%esp),%ecx			/* copy remainder by 16-bit words */
	shrl	$1,%ecx
	decl	%esi
	decl	%edi
	rep
	movsw
	popl	%edi
	popl	%esi
	cld
	ret

ENTRY(bcopyx)
	movl	16(%esp),%eax
	cmpl	$2,%eax
	je	bcopyw				/* not _bcopyw, to avoid multiple mcounts */
	cmpl	$4,%eax
	je	bcopy				/* XXX the shared ret's break mexitcount */
	jmp	bcopyb

/*
 * (ov)bcopy(src, dst, cnt)
 *  ws@tools.de     (Wolfgang Solfrank, TooLs GmbH) +49-228-985800
 */
ALTENTRY(ovbcopy)
ENTRY(bcopy)
bcopy:
	pushl	%esi
	pushl	%edi
	movl	12(%esp),%esi
	movl	16(%esp),%edi
	movl	20(%esp),%ecx
	cmpl	%esi,%edi			/* potentially overlapping? */
	jnb	1f
	shrl	$2,%ecx				/* copy by 32-bit words */
	cld					/* nope, copy forwards */
	rep
	movsl
	movl	20(%esp),%ecx
	andl	$3,%ecx				/* any bytes left? */
	rep
	movsb
	popl	%edi
	popl	%esi
	ret

	ALIGN_TEXT
1:
	addl	%ecx,%edi			/* copy backwards */
	addl	%ecx,%esi
	andl	$3,%ecx				/* any fractional bytes? */
	decl	%edi
	decl	%esi
	std
	rep
	movsb
	movl	20(%esp),%ecx			/* copy remainder by 32-bit words */
	shrl	$2,%ecx
	subl	$3,%esi
	subl	$3,%edi
	rep
	movsl
	popl	%edi
	popl	%esi
	cld
	ret


/*
 * Note: memcpy does not support overlapping copies
 */
ENTRY(memcpy)
	pushl	%edi
	pushl	%esi
	movl	12(%esp),%edi
	movl	16(%esp),%esi
	movl	20(%esp),%ecx
	movl	%edi,%eax
	shrl	$2,%ecx				/* copy by 32-bit words */
	cld					/* nope, copy forwards */
	rep
	movsl
	movl	20(%esp),%ecx
	andl	$3,%ecx				/* any bytes left? */
	rep
	movsb
	popl	%esi
	popl	%edi
	ret


/*****************************************************************************/
/* copyout and fubyte family                                                 */
/*****************************************************************************/
/*
 * Access user memory from inside the kernel. These routines and possibly
 * the math- and DOS emulators should be the only places that do this.
 *
 * We have to access the memory with user's permissions, so use a segment
 * selector with RPL 3. For writes to user space we have to additionally
 * check the PTE for write permission, because the 386 does not check
 * write permissions when we are executing with EPL 0. The 486 does check
 * this if the WP bit is set in CR0, so we can use a simpler version here.
 *
 * These routines set curpcb->onfault for the time they execute. When a
 * protection violation occurs inside the functions, the trap handler
 * returns to *curpcb->onfault instead of the function.
 */


ENTRY(copyout)					/* copyout(from_kernel, to_user, len) */
	movl	_curpcb,%eax
	movl	$copyout_fault,PCB_ONFAULT(%eax)
	pushl	%esi
	pushl	%edi
	pushl	%ebx
	movl	16(%esp),%esi
	movl	20(%esp),%edi
	movl	24(%esp),%ebx
	testl	%ebx,%ebx			/* anything to do? */
	jz	done_copyout

	/*
	 * Check explicitly for non-user addresses.  If 486 write protection
	 * is being used, this check is essential because we are in kernel
	 * mode so the h/w does not provide any protection against writing
	 * kernel addresses.
	 */

	/*
	 * First, prevent address wrapping.
	 */
	movl	%edi,%eax
	addl	%ebx,%eax
	jc	copyout_fault
/*
 * XXX STOP USING VM_MAXUSER_ADDRESS.
 * It is an end address, not a max, so every time it is used correctly it
 * looks like there is an off by one error, and of course it caused an off
 * by one error in several places.
 */
	cmpl	$VM_MAXUSER_ADDRESS,%eax
	ja	copyout_fault

#if defined(I386_CPU)

#if defined(I486_CPU) || defined(I586_CPU)
	cmpl	$CPUCLASS_386,_cpu_class
	jne	3f
#endif
/*
 * We have to check each PTE for user write permission.
 * The checking may cause a page fault, so it is important to set
 * up everything for return via copyout_fault before here.
 */
	/* compute number of pages */
	movl	%edi,%ecx
	andl	$NBPG-1,%ecx
	addl	%ebx,%ecx
	decl	%ecx
	shrl	$IDXSHIFT+2,%ecx
	incl	%ecx

	/* compute PTE offset for start address */
	movl	%edi,%edx
	shrl	$IDXSHIFT,%edx
	andb	$0xfc,%dl

1:	/* check PTE for each page */
	movb	_PTmap(%edx),%al
	andb	$0x07,%al			/* Pages must be VALID + USERACC + WRITABLE */
	cmpb	$0x07,%al
	je	2f

	/* simulate a trap */
	pushl	%edx
	pushl	%ecx
	shll	$IDXSHIFT,%edx
	pushl	%edx
	call	_trapwrite			/* trapwrite(addr) */
	popl	%edx
	popl	%ecx
	popl	%edx

	testl	%eax,%eax			/* if not ok, return EFAULT */
	jnz	copyout_fault

2:
	addl	$4,%edx
	decl	%ecx
	jnz	1b				/* check next page */
#endif /* I386_CPU */

	/* bcopy(%esi, %edi, %ebx) */
3:
	movl	%ebx,%ecx
	shrl	$2,%ecx
	cld
	rep
	movsl
	movb	%bl,%cl
	andb	$3,%cl
	rep
	movsb

done_copyout:
	popl	%ebx
	popl	%edi
	popl	%esi
	xorl	%eax,%eax
	movl	_curpcb,%edx
	movl	%eax,PCB_ONFAULT(%edx)
	ret

	ALIGN_TEXT
copyout_fault:
	popl	%ebx
	popl	%edi
	popl	%esi
	movl	_curpcb,%edx
	movl	$0,PCB_ONFAULT(%edx)
	movl	$EFAULT,%eax
	ret

/* copyin(from_user, to_kernel, len) */
ENTRY(copyin)
	movl	_curpcb,%eax
	movl	$copyin_fault,PCB_ONFAULT(%eax)
	pushl	%esi
	pushl	%edi
	movl	12(%esp),%esi			/* caddr_t from */
	movl	16(%esp),%edi			/* caddr_t to */
	movl	20(%esp),%ecx			/* size_t  len */

	/*
	 * make sure address is valid
	 */
	movl	%esi,%edx
	addl	%ecx,%edx
	jc	copyin_fault
	cmpl	$VM_MAXUSER_ADDRESS,%edx
	ja	copyin_fault

	movb	%cl,%al
	shrl	$2,%ecx				/* copy longword-wise */
	cld
	rep
	movsl
	movb	%al,%cl
	andb	$3,%cl				/* copy remaining bytes */
	rep
	movsb

	popl	%edi
	popl	%esi
	xorl	%eax,%eax
	movl	_curpcb,%edx
	movl	%eax,PCB_ONFAULT(%edx)
	ret

	ALIGN_TEXT
copyin_fault:
	popl	%edi
	popl	%esi
	movl	_curpcb,%edx
	movl	$0,PCB_ONFAULT(%edx)
	movl	$EFAULT,%eax
	ret

/*
 * fu{byte,sword,word} : fetch a byte (sword, word) from user memory
 */
ALTENTRY(fuiword)
ENTRY(fuword)
	movl	_curpcb,%ecx
	movl	$fusufault,PCB_ONFAULT(%ecx)
	movl	4(%esp),%edx			/* from */

	cmpl	$VM_MAXUSER_ADDRESS-4,%edx	/* verify address is valid */
	ja	fusufault

	movl	(%edx),%eax
	movl	$0,PCB_ONFAULT(%ecx)
	ret

/*
 * These two routines are called from the profiling code, potentially
 * at interrupt time. If they fail, that's okay, good things will
 * happen later. Fail all the time for now - until the trap code is
 * able to deal with this.
 */
ALTENTRY(suswintr)
ENTRY(fuswintr)
	movl	$-1,%eax
	ret

ENTRY(fusword)
	movl	_curpcb,%ecx
	movl	$fusufault,PCB_ONFAULT(%ecx)
	movl	4(%esp),%edx

	cmpl	$VM_MAXUSER_ADDRESS-2,%edx
	ja	fusufault

	movzwl	(%edx),%eax
	movl	$0,PCB_ONFAULT(%ecx)
	ret

ALTENTRY(fuibyte)
ENTRY(fubyte)
	movl	_curpcb,%ecx
	movl	$fusufault,PCB_ONFAULT(%ecx)
	movl	4(%esp),%edx

	cmpl	$VM_MAXUSER_ADDRESS-1,%edx
	ja	fusufault

	movzbl	(%edx),%eax
	movl	$0,PCB_ONFAULT(%ecx)
	ret

	ALIGN_TEXT
fusufault:
	movl	_curpcb,%ecx
	xorl	%eax,%eax
	movl	%eax,PCB_ONFAULT(%ecx)
	decl	%eax
	ret

/*
 * su{byte,sword,word}: write a byte (word, longword) to user memory
 */
ALTENTRY(suiword)
ENTRY(suword)
	movl	_curpcb,%ecx
	movl	$fusufault,PCB_ONFAULT(%ecx)
	movl	4(%esp),%edx

#if defined(I386_CPU)

#if defined(I486_CPU) || defined(I586_CPU)
	cmpl	$CPUCLASS_386,_cpu_class
	jne	2f				/* we only have to set the right segment selector */
#endif /* I486_CPU || I586_CPU */

	/* XXX - page boundary crossing is still not handled */
	movl	%edx,%eax
	shrl	$IDXSHIFT,%edx
	andb	$0xfc,%dl
	movb	_PTmap(%edx),%dl
	andb	$0x7,%dl			/* must be VALID + USERACC + WRITE */
	cmpb	$0x7,%dl
	je	1f

	/* simulate a trap */
	pushl	%eax
	call	_trapwrite
	popl	%edx				/* remove junk parameter from stack */
	movl	_curpcb,%ecx			/* restore trashed register */
	testl	%eax,%eax
	jnz	fusufault
1:
	movl	4(%esp),%edx
#endif

2:	
	cmpl	$VM_MAXUSER_ADDRESS-4,%edx	/* verify address validity */
	ja	fusufault

	movl	8(%esp),%eax
	movl	%eax,(%edx)
	xorl	%eax,%eax
	movl	%eax,PCB_ONFAULT(%ecx)
	ret

ENTRY(susword)
	movl	_curpcb,%ecx
	movl	$fusufault,PCB_ONFAULT(%ecx)
	movl	4(%esp),%edx

#if defined(I386_CPU)

#if defined(I486_CPU) || defined(I586_CPU)
	cmpl	$CPUCLASS_386,_cpu_class
	jne	2f
#endif /* I486_CPU || I586_CPU */

	/* XXX - page boundary crossing is still not handled */
	movl	%edx,%eax
	shrl	$IDXSHIFT,%edx
	andb	$0xfc,%dl
	movb	_PTmap(%edx),%dl
	andb	$0x7,%dl			/* must be VALID + USERACC + WRITE */
	cmpb	$0x7,%dl
	je	1f

	/* simulate a trap */
	pushl	%eax
	call	_trapwrite
	popl	%edx				/* remove junk parameter from stack */
	movl	_curpcb,%ecx			/* restore trashed register */
	testl	%eax,%eax
	jnz	fusufault
1:
	movl	4(%esp),%edx
#endif

2:
	cmpl	$VM_MAXUSER_ADDRESS-2,%edx	/* verify address validity */
	ja	fusufault

	movw	8(%esp),%ax
	movw	%ax,(%edx)
	xorl	%eax,%eax
	movl	%eax,PCB_ONFAULT(%ecx)
	ret

ALTENTRY(suibyte)
ENTRY(subyte)
	movl	_curpcb,%ecx
	movl	$fusufault,PCB_ONFAULT(%ecx)
	movl	4(%esp),%edx

#if defined(I386_CPU)

#if defined(I486_CPU) || defined(I586_CPU)
	cmpl	$CPUCLASS_386,_cpu_class
	jne	2f
#endif /* I486_CPU || I586_CPU */

	movl	%edx,%eax
	shrl	$IDXSHIFT,%edx
	andb	$0xfc,%dl
	movb	_PTmap(%edx),%dl
	andb	$0x7,%dl			/* must be VALID + USERACC + WRITE */
	cmpb	$0x7,%dl
	je	1f

	/* simulate a trap */
	pushl	%eax
	call	_trapwrite
	popl	%edx				/* remove junk parameter from stack */
	movl	_curpcb,%ecx			/* restore trashed register */
	testl	%eax,%eax
	jnz	fusufault
1:
	movl	4(%esp),%edx
#endif

2:
	cmpl	$VM_MAXUSER_ADDRESS-1,%edx	/* verify address validity */
	ja	fusufault

	movb	8(%esp),%al
	movb	%al,(%edx)
	xorl	%eax,%eax
	movl	%eax,PCB_ONFAULT(%ecx)
	ret

/*
 * copyoutstr(from, to, maxlen, int *lencopied)
 *	copy a string from from to to, stop when a 0 character is reached.
 *	return ENAMETOOLONG if string is longer than maxlen, and
 *	EFAULT on protection violations. If lencopied is non-zero,
 *	return the actual length in *lencopied.
 */
ENTRY(copyoutstr)
	pushl	%esi
	pushl	%edi
	movl	_curpcb,%ecx
	movl	$cpystrflt,PCB_ONFAULT(%ecx)	/* XXX rename copyoutstr_fault */

	movl	12(%esp),%esi			/* %esi = from */
	movl	16(%esp),%edi			/* %edi = to */
	movl	20(%esp),%edx			/* %edx = maxlen */
	cld

#if defined(I386_CPU)

#if defined(I486_CPU) || defined(I586_CPU)
	cmpl	$CPUCLASS_386,_cpu_class
	jne	5f
#endif /* I486_CPU || I586_CPU */

1:
	/*
	 * It suffices to check that the first byte is in user space, because
	 * we look at a page at a time and the end address is on a page
	 * boundary.
	 */
	cmpl	$VM_MAXUSER_ADDRESS-1,%edi
	ja	cpystrflt

	movl	%edi,%eax
	shrl	$IDXSHIFT,%eax
	andb	$0xfc,%al
	movb	_PTmap(%eax),%al
	andb	$7,%al
	cmpb	$7,%al
	je	2f

	/* simulate trap */
	pushl	%edx
	pushl	%edi
	call	_trapwrite
	cld
	popl	%edi
	popl	%edx
	testl	%eax,%eax
	jnz	cpystrflt

2:	/* copy up to end of this page */
	movl	%edi,%eax
	andl	$NBPG-1,%eax
	movl	$NBPG,%ecx
	subl	%eax,%ecx			/* ecx = NBPG - (src % NBPG) */
	cmpl	%ecx,%edx
	jae	3f
	movl	%edx,%ecx			/* ecx = min(ecx, edx) */
3:
	testl	%ecx,%ecx
	jz	4f
	decl	%ecx
	decl	%edx
	lodsb
	stosb
	orb	%al,%al
	jnz	3b

	/* Success -- 0 byte reached */
	decl	%edx
	xorl	%eax,%eax
	jmp	6f

4:	/* next page */
	testl	%edx,%edx
	jnz	1b

	/* edx is zero -- return ENAMETOOLONG */
	movl	$ENAMETOOLONG,%eax
	jmp	cpystrflt_x
#endif /* I386_CPU */

#if defined(I486_CPU) || defined(I586_CPU)
5:
	incl	%edx
1:
	decl	%edx
	jz	2f
	/*
	 * XXX - would be faster to rewrite this function to use
	 * strlen() and copyout().
	 */
	cmpl	$VM_MAXUSER_ADDRESS-1,%edi
	ja	cpystrflt

	lodsb
	stosb
	orb	%al,%al
	jnz	1b

	/* Success -- 0 byte reached */
	decl	%edx
	xorl	%eax,%eax
	jmp	cpystrflt_x
2:
	/* edx is zero -- return ENAMETOOLONG */
	movl	$ENAMETOOLONG,%eax
	jmp	cpystrflt_x

#endif /* I486_CPU || I586_CPU */


/*
 * copyinstr(from, to, maxlen, int *lencopied)
 *	copy a string from from to to, stop when a 0 character is reached.
 *	return ENAMETOOLONG if string is longer than maxlen, and
 *	EFAULT on protection violations. If lencopied is non-zero,
 *	return the actual length in *lencopied.
 */
ENTRY(copyinstr)
	pushl	%esi
	pushl	%edi
	movl	_curpcb,%ecx
	movl	$cpystrflt,PCB_ONFAULT(%ecx)

	movl	12(%esp),%esi			/* %esi = from */
	movl	16(%esp),%edi			/* %edi = to */
	movl	20(%esp),%edx			/* %edx = maxlen */

	movl	$VM_MAXUSER_ADDRESS,%eax

	/* make sure 'from' is within bounds */
	subl	%esi,%eax
	jbe	cpystrflt

	/* restrict maxlen to <= VM_MAXUSER_ADDRESS-from */
	cmpl	%edx,%eax
	jae	1f
	movl	%eax,%edx
	movl	%eax,20(%esp)
1:
	incl	%edx
	cld

2:
	decl	%edx
	jz	3f

	lodsb
	stosb
	orb	%al,%al
	jnz	2b

	/* Success -- 0 byte reached */
	decl	%edx
	xorl	%eax,%eax
	jmp	cpystrflt_x
3:
	/* edx is zero - return ENAMETOOLONG or EFAULT */
	cmpl	$VM_MAXUSER_ADDRESS,%esi
	jae	cpystrflt
4:
	movl	$ENAMETOOLONG,%eax
	jmp	cpystrflt_x

cpystrflt:
	movl	$EFAULT,%eax

cpystrflt_x:
	/* set *lencopied and return %eax */
	movl	_curpcb,%ecx
	movl	$0,PCB_ONFAULT(%ecx)
	movl	20(%esp),%ecx
	subl	%edx,%ecx
	movl	24(%esp),%edx
	testl	%edx,%edx
	jz	1f
	movl	%ecx,(%edx)
1:
	popl	%edi
	popl	%esi
	ret


/*
 * copystr(from, to, maxlen, int *lencopied)
 */
ENTRY(copystr)
	pushl	%esi
	pushl	%edi

	movl	12(%esp),%esi			/* %esi = from */
	movl	16(%esp),%edi			/* %edi = to */
	movl	20(%esp),%edx			/* %edx = maxlen */
	incl	%edx
	cld
1:
	decl	%edx
	jz	4f
	lodsb
	stosb
	orb	%al,%al
	jnz	1b

	/* Success -- 0 byte reached */
	decl	%edx
	xorl	%eax,%eax
	jmp	6f
4:
	/* edx is zero -- return ENAMETOOLONG */
	movl	$ENAMETOOLONG,%eax

6:
	/* set *lencopied and return %eax */
	movl	20(%esp),%ecx
	subl	%edx,%ecx
	movl	24(%esp),%edx
	testl	%edx,%edx
	jz	7f
	movl	%ecx,(%edx)
7:
	popl	%edi
	popl	%esi
	ret

#if 0
/*
 * Miscellaneous kernel support functions
 */
ENTRY(ffs)
	bsfl	4(%esp),%eax
	jz	1f
	incl	%eax
	ret
1:
	xorl	%eax,%eax
	ret

ENTRY(bcmp)
	pushl	%edi
	pushl	%esi
	movl	12(%esp),%edi
	movl	16(%esp),%esi
	movl	20(%esp),%edx
	xorl	%eax,%eax

	movl	%edx,%ecx
	shrl	$2,%ecx
	cld					/* compare forwards */
	repe
	cmpsl
	jne	1f

	movl	%edx,%ecx
	andl	$3,%ecx
	repe
	cmpsb
	je	2f
1:
	incl	%eax
2:
	popl	%esi
	popl	%edi
	ret
#endif

/*
 * Handling of special 386 registers and descriptor tables etc
 */
/* void lgdt(struct region_descriptor *rdp); */
ENTRY(lgdt)
	/* reload the descriptor table */
	movl	4(%esp),%eax
	lgdt	(%eax)

	/* flush the prefetch q */
	jmp	1f
	nop
1:
	/* reload "stale" selectors */
	movl	$KDSEL,%eax
	movl	%ax,%ds
	movl	%ax,%es
	movl	%ax,%ss

	/* reload code selector by turning return into intersegmental return */
	movl	(%esp),%eax
	pushl	%eax
#	movl	$KCSEL,4(%esp)
	movl	$8,4(%esp)
	lret

/*
 * void lidt(struct region_descriptor *rdp);
 */
ENTRY(lidt)
	movl	4(%esp),%eax
	lidt	(%eax)
	ret

/*
 * void lldt(u_short sel)
 */
ENTRY(lldt)
	lldt	4(%esp)
	ret

/*
 * void ltr(u_short sel)
 */
ENTRY(ltr)
	ltr	4(%esp)
	ret

/* ssdtosd(*ssdp,*sdp) */
ENTRY(ssdtosd)
	pushl	%ebx
	movl	8(%esp),%ecx
	movl	8(%ecx),%ebx
	shll	$16,%ebx
	movl	(%ecx),%edx
	roll	$16,%edx
	movb	%dh,%bl
	movb	%dl,%bh
	rorl	$8,%ebx
	movl	4(%ecx),%eax
	movw	%ax,%dx
	andl	$0xf0000,%eax
	orl	%eax,%ebx
	movl	12(%esp),%ecx
	movl	%edx,(%ecx)
	movl	%ebx,4(%ecx)
	popl	%ebx
	ret

/* load_cr0(cr0) */
ENTRY(load_cr0)
	movl	4(%esp),%eax
	movl	%eax,%cr0
	ret

/* rcr0() */
ENTRY(rcr0)
	movl	%cr0,%eax
	ret

/* rcr3() */
ENTRY(rcr3)
	movl	%cr3,%eax
	ret

/* void load_cr3(caddr_t cr3) */
ENTRY(load_cr3)
	movl	4(%esp),%eax
	movl	%eax,%cr3
	ret


/*****************************************************************************/
/* setjump, longjump                                                         */
/*****************************************************************************/

#ifdef SPIN
ALTENTRY(_setjmp)
ALTENTRY(__setjmp)
#endif
ENTRY(setjmp)
	movl	4(%esp),%eax
	movl	%ebx,(%eax)			/* save ebx */
	movl	%esp,4(%eax)			/* save esp */
	movl	%ebp,8(%eax)			/* save ebp */
	movl	%esi,12(%eax)			/* save esi */
	movl	%edi,16(%eax)			/* save edi */
	movl	(%esp),%edx			/* get rta */
	movl	%edx,20(%eax)			/* save eip */
	xorl	%eax,%eax			/* return(0); */
	ret

#ifdef SPIN
ALTENTRY(_longjmp)
ALTENTRY(__longjmp)
#endif
ENTRY(longjmp)
	movl	4(%esp),%eax
	movl	(%eax),%ebx			/* restore ebx */
	movl	4(%eax),%esp			/* restore esp */
	movl	8(%eax),%ebp			/* restore ebp */
	movl	12(%eax),%esi			/* restore esi */
	movl	16(%eax),%edi			/* restore edi */
	movl	20(%eax),%edx			/* get rta */
	movl	%edx,(%esp)			/* put in return frame */
	xorl	%eax,%eax			/* return(1); */
	incl	%eax
	ret

/*
 * Here for doing BB-profiling (gcc -a).
 * We rely on the "bbset" instead, but need a dummy function.
 */
	.text
	.align 2
.globl	___bb_init_func
___bb_init_func:
        movl 4(%esp),%eax
        movl $1,(%eax)
        ret 

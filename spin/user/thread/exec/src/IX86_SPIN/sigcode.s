/*
	Sigcode is a signal trampolin code ; the signal handler first comes
	here, and it dispatches into the real handler.

	This code is not linked into the extension. Instead, the byte image of
	the procedure is copied into a fixed address in the user space
	on process startup by sphinx.

	This code is mostly copied from FreeBSD locore.s.
	
		-- Yas
*/
	
	
#define LCALL(x,y)	.byte 0x9a ; .long y ; .word x
	
	.globl	sigcode
sigcode:		
	call	16(%esp)       /* handler field */
#if 0	/* XXX owa. see.  i386/i386/genassym.c and i386/i386/locore.s	*/
	lea	8(%esp),%eax   /* scp field */
#else
	lea	0x14(%esp),%eax			/* sc field */
#endif
	pushl	%eax
	pushl	%eax				/* junk to fake return address */
	movl	$103,%eax			/* XXX sigreturn() */
	LCALL(0x7,0)				/* enter kernel with args on stack */
	hlt					/* never gets here */

	.globl	szsigcode
szsigcode:
	.long	szsigcode-sigcode

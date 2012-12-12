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
 *	$Id: spl.h,v 1.2 1997/01/31 15:59:57 mef Exp $
 */

#ifndef _MACHINE_IPL_H_
#define	_MACHINE_IPL_H_

#include <machine/ipl.h>	/* XXX "machine" means cpu for i386 */

/*
 * Software interrupt bit numbers in priority order.  The priority only
 * determines which swi will be dispatched next; a higher priority swi
 * may be dispatched when a nested h/w interrupt handler returns.
 */
#define	SWI_TTY		(NHWI + 0)
#define	SWI_NET		(NHWI + 1)
#define	SWI_CLOCK	30
#define	SWI_AST		31

/*
 * Corresponding interrupt-pending bits for ipending.
 */
#define	SWI_TTY_PENDING		(1 << SWI_TTY)
#define	SWI_NET_PENDING		(1 << SWI_NET)
#define	SWI_CLOCK_PENDING	(1 << SWI_CLOCK)
#define	SWI_AST_PENDING		(1 << SWI_AST)

/*
 * Corresponding interrupt-disable masks for cpl.  The ordering is now by
 * inclusion (where each mask is considered as a set of bits). Everything
 * except SWI_AST_MASK includes SWI_CLOCK_MASK so that softclock() doesn't
 * run while other swi handlers are running and timeout routines can call
 * swi handlers.  Everything includes SWI_AST_MASK so that AST's are masked
 * until just before return to user mode.  SWI_TTY_MASK includes SWI_NET_MASK
 * in case tty interrupts are processed at splsofttty() for a tty that is in
 * SLIP or PPP line discipline (this is weaker than merging net_imask with
 * tty_imask in isa.c - splimp() must mask hard and soft tty interrupts, but
 * spltty() apparently only needs to mask soft net interrupts).
 */
#define	SWI_TTY_MASK	(SWI_TTY_PENDING | SWI_CLOCK_MASK | SWI_NET_MASK)
#define	SWI_NET_MASK	(SWI_NET_PENDING | SWI_CLOCK_MASK)
#define	SWI_CLOCK_MASK	(SWI_CLOCK_PENDING | SWI_AST_MASK)
#define	SWI_AST_MASK	SWI_AST_PENDING
#define	SWI_MASK	(~HWI_MASK)

#ifndef	LOCORE

extern	unsigned bio_imask;	/* group of interrupts masked with splbio() */
extern	unsigned cpl;		/* current priority level mask */
extern	volatile unsigned idelayed;	/* interrupts to become pending */
extern	volatile unsigned ipending;	/* active interrupts masked by cpl */
extern	unsigned net_imask;	/* group of interrupts masked with splimp() */
extern	unsigned stat_imask;	/* interrupts masked with splstatclock() */
extern	unsigned tty_imask;	/* group of interrupts masked with spltty() */

/*
 * ipending has to be volatile so that it is read every time it is accessed
 * in splx() and spl0(), but we don't want it to be read nonatomically when
 * it is changed.  Pretending that ipending is a plain int happens to give
 * suitable atomic code for "ipending |= constant;".
 */
#define	setdelayed()	(*(unsigned *)&ipending |= loadandclear(&idelayed))
#define	setsoftast()	(*(unsigned *)&ipending |= SWI_AST_PENDING)
#define	setsoftclock()	(*(unsigned *)&ipending |= SWI_CLOCK_PENDING)
#define	setsoftnet()	(*(unsigned *)&ipending |= SWI_NET_PENDING)
#define	setsofttty()	(*(unsigned *)&ipending |= SWI_TTY_PENDING)

#define	schedsofttty()	(*(unsigned *)&idelayed |= SWI_TTY_PENDING)

#ifdef __GNUC__

void	splz	__P((void));

#define	GENSPL(name, set_cpl) \
static __inline int name(void)			\
{						\
	unsigned x;				\
						\
	__asm __volatile("" : : : "memory");	\
	x = cpl;				\
	set_cpl;				\
	return (x);				\
}

GENSPL(splbio, cpl |= bio_imask)
GENSPL(splclock, cpl = HWI_MASK | SWI_MASK)
GENSPL(splhigh, cpl = HWI_MASK | SWI_MASK)
GENSPL(splimp, cpl |= net_imask)
GENSPL(splnet, cpl |= SWI_NET_MASK)
GENSPL(splsoftclock, cpl = SWI_CLOCK_MASK)
GENSPL(splsofttty, cpl |= SWI_TTY_MASK)
GENSPL(splstatclock, cpl |= stat_imask)
GENSPL(spltty, cpl |= tty_imask)

static __inline void
spl0(void)
{
	cpl = SWI_AST_MASK;
	if (ipending & ~SWI_AST_MASK)
		splz();
}

static __inline void
splx(int ipl)
{
	cpl = ipl;
	if (ipending & ~ipl)
		splz();
}

#endif /* __GNUC__ */

#endif /* !LOCORE */

#endif /* !_MACHINE_IPL_H_ */

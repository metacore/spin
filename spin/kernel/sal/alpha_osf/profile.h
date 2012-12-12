/* HISTORY
 *
 * 02-Dec-96  Charles Garrett (garrett) at the University of Washington
 *	This file contains common functionality used to profile assembly
 *	 language routines. When the profiling option is turned on at
 *	 compile time, the CALL_PROFILER macro will be a sequence of
 *	 instructions that jump to the _mcount procedure. When profiling
 *	 is turned off, the macro will be empty.
 *
 *
 */

#ifndef _PROFILE_H
#define _PROFILE_H

#ifdef __alpha

#if defined(CALL_GRAPH)

/* CALL_PROFILER is to be inserted in assembly code that follows the
   standard calling convention and loads the gp register. Insert 
   CALL_PROFILER immediately after the ldgp instruction. */

#define CALL_PROFILER \
	.set	noat; \
	lda	$at,_mcount; \
	jsr	$at,($at),_mcount; \
        ldgp    gp,0(pv);

/* CALL_PROFILER_NO_GP is for code that doesn't load the gp register.
   We must store the current gp value, load a new gp for the current
   pc value, and restore the old gp after we are done. */

#define CALL_PROFILER_NO_GP \
	.set	noat; \
	ldgp	gp,0(pv); \
	lda	$at,_mcount; \
	jsr	$at,($at),_mcount;\
	br      gp,10f; \
10:	ldgp    gp,0(gp);

/* The assembly language routines called with different calling conventions
   than regular C code which is what the profiler expects. In particular,
   the div and rem routines use non-standard return address registers. We
   accomodate this by saving the contents of ra on the stack, moving the
   actual return address into ra before executing the profiling code, then
   moving the altered ra value back into the return address register and
   restoring the contents of ra after the profiling code. */

#define CALL_PROFILER_NSTD(RA) /* \
	.set	noat; \
        lda     sp,-8(sp); \
        stq     ra,0(sp); \
        bis     RA,zero,ra; \
	lda	$at,_mcount; \
	jsr	$at,($at),_mcount; \
	ldgp	gp,0(pv); \
        bis     ra,zero,RA; \
        ldq     ra,0(sp); \
        lda     sp,8(sp); */
#else
#define CALL_PROFILER
#define CALL_PROFILER_NO_GP
#define CALL_PROFILER_NSTD(RA)
#endif

#endif /* __alpha */
#endif

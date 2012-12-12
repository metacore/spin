/*
 * Kernel interface to machine-dependent clock driver.
 * Garrett Wollman, September 1994.
 * This file is in the public domain.
 */

/* HISTORY
# 6-Oct-97  David Becker at the University of Washington
#	We want to keep the cycle counter free running at all times to
#	do timing analysis.
#
 * 17-Jan-97  Charles Garrett (garrett) at the University of Washington
 *	The profiler needs to keep the cycle counter running, instead of
 *	 resetting every clock tick, so we use the older form of
 *	 CPU_CLOCKUPDATE. 
 *
   
*/
#ifndef _MACHINE_CLOCK_H_
#define	_MACHINE_CLOCK_H_

#if 0
	/*
	 * This resets the CPU cycle counter to zero, to make our
	 * job easier in microtime().  Some fancy ifdefs could speed
	 * this up for Pentium-only kernels.
	 * We want this to be done as close as possible to the actual
	 * timer incrementing in hardclock(), because there is a window
	 * between the two where the value is no longer valid.  Experimentation
	 * may reveal a good precompensation to apply in microtime().
	 */
#define CPU_CLOCKUPDATE(otime, ntime) \
	do { \
	if(pentium_mhz) { \
		__asm __volatile("cli\n" \
				 "movl (%2),%%eax\n" \
				 "movl %%eax,(%1)\n" \
				 "movl 4(%2),%%eax\n" \
				 "movl %%eax,4(%1)\n" \
				 "movl $0x10,%%ecx\n" \
				 "xorl %%eax,%%eax\n" \
				 "movl %%eax,%%edx\n" \
				 ".byte 0x0f, 0x30\n" \
				 "sti\n" \
				 "#%0%1%2" \
				 : "=m"(*otime)	/* no outputs */ \
				 : "c"(otime), "b"(ntime) /* fake input */ \
				 : "ax", "cx", "dx"); \
	} else { \
		*(otime) = *(ntime); \
	} \
	} while(0)

#else
#define CPU_CLOCKUPDATE(otime, ntime) \
		(*(otime) = *(ntime))
#endif

#if defined(KERNEL) && !defined(LOCORE)
#include <sys/cdefs.h>
#include <machine/frame.h>

/*
 * i386 to clock driver interface.
 * XXX almost all of it is misplaced.  i586 stuff is done in isa/clock.c
 * and isa stuff is done in i386/microtime.s and i386/support.s.
 */
extern int	adjkerntz;
extern int	disable_rtc_set;
#ifdef I586_CPU
extern int	pentium_mhz;
#endif
extern int 	timer0_max_count;
extern u_int 	timer0_overflow_threshold;
extern u_int 	timer0_prescaler_count;

#ifdef I586_CPU
void	calibrate_cyclecounter __P((void));
#endif
void	clkintr __P((struct clockframe frame));
void	rtcintr __P((struct clockframe frame));

/*
 * Driver to clock driver interface.
 */
void	DELAY __P((int usec));
int	acquire_timer0 __P((int rate,
			    void (*function)(struct clockframe *frame)));
int	acquire_timer2 __P((int mode));
int	release_timer0 __P((void));
int	release_timer2 __P((void));
int	sysbeep __P((int pitch, int period));

#endif /* KERNEL && !LOCORE */

#endif /* !_MACHINE_CLOCK_H_ */

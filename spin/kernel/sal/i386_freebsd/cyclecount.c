/*
  	cyclecount.c

	x86 cyclecounter implementation of the sal cyclecount.h interface.

	created by David Becker Wed Jun 25 09:34:50 PDT 1997
 */


#include <sal/cyclecount.h>

/*****
 standalone cyclecounter support
 */


int	pentium_mhz=200;

/* 64-bit counter type.  ASSUMES LITTLE ENDIAN.  */
typedef union {
	struct {
		unsigned int lo;  /* least significant word */
		unsigned int hi;  /* most significant word */
	} word;
        unsigned long long value;
	} cyclereg;

/* P5 and better rdtsc insn */
void
rdtsc (cyclereg *cc)
{
	__asm __volatile(".byte 0xf,0x31" : "=A" (*cc));
}


/* 
 hertz returns the cycles per second of the cycle counter (cpu clock rate)
 */
cycle_t
hertz()
{
	return pentium_mhz*1000000;
}

/* 
 cyclecounter return low word (32bits) of free running cycle count register
 */
cycle_t
cyclecounter()
{
	cyclereg cc;
	rdtsc(&cc);
	return cc.word.lo;
}

/* 
 cycledelta returns (now-then) where now and then are 32bits long and
 then possibly overflowed back thru 0.
 */
cycle_t
cycleminus(cycle_t now, cycle_t then)
{
	return now-then;
}

/* 
 cycle_to_microsec returns cycles divided by cpu hertz
 */
long
cycle_to_microsec(cycle_t cycles)
{
	return cycles/(hertz()/1000000);
}



#include <sal/cyclecount.h>

standalone_halt()
{
	halt_cpu();
}

/*****
 standalone cyclecounter support
 */


/* 
 hertz returns the cycles per second of the cycle counter (cpu clock rate)
 */
cycle_t
hertz()
{
	return get_rpb_counter();
}

/* 
 cyclecounter return low word (32bits) of free running cycle count register
 */
cycle_t
cyclecounter()
{
	return rpcc();
}

/* 
 cycledelta returns (now-then) where now and then are 32bits long and
 then possibly overflowed back thru 0.
 */
cycle_t
cycleminus(cycle_t now, cycle_t then)
{
	return (signed)((signed)now-(signed)then);
}

/* 
 cycle_to_microsec returns cycles divided by cpu hertz
 */
long
cycle_to_microsec(cycle_t cycles)
{
	return cycles/(hertz()/1000000);
}

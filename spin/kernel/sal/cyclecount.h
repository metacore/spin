
/*****
 Cycle Counter support
 */

typedef unsigned long cycle_t;


/* 
 cyclecounter return low word (32bits) of free running cycle count register
 */
cycle_t cyclecounter();

/* 
 cycleminus returns (now-then) where now and then are 32bits long and
 then possibly overflowed back thru 0.
 */
cycle_t cycleminus(cycle_t now, cycle_t then);

/* 
 cycle_to_microsec returns cycles divided by cpu Mhz
 */
long cycle_to_microsec(cycle_t cycles);

/* 
 hertz returns the cycles per second of the cycle counter (cpu clock rate)
 */
cycle_t hertz();

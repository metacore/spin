/*
 * HISTORY
 * 22-Sep-97  Yasushi Saito (yasushi) at the University of Washington
 *	Added Pentium support.
 * 23-Jan-97  Yasushi Saito (yasushi) at the University of Washington
 *	Whisted.
 */
#ifndef _SPY_H
#define _SPY_H

/* This module provides precise performance measurements using
   the Alpha/Pentium cycle counter register.  
   Spy is analogous to a module with the same name in m3core. 
*/

#ifdef __cplusplus
extern "C" {
#endif

#include <stdio.h>

typedef struct spy *spy_t;

spy_t spy_create(char *name, int n_samples);
/* Create a spy. N_SAMPLES specifies the maximum number of samples that can
   be stored in spy. Samples are used to calculate the median and
   standard deviation. */

spy_t spy_derive(spy_t parent, int n_samples);


void spy_start(spy_t s);
/* Start the spy timer. */

void spy_stop(spy_t s);
/* Stop the spy timer, and record the time elapsed since the last
   spy_start. Spy_start and spy_stop pair can be repeat any number of times.*/

void spy_dump_all();
/* Display the measurement status of all the spy timers created so far */

void spy_clear(spy_t s);
/* Clear the internal memory of the spy S. */

void spy_clear_all();
/* CLear the internal memory of all the spys created so far. */

long spy_average(spy_t s);
/* Calculate the average cycles spent in spy epochs so far */

long spy_median(spy_t s);
/* Calculate the median cycles spent in spy epochs so far */

#define SPY_DISPLAY_CYCLES 1 /* display cycles, not only microseconds */
#define SPY_DISPLAY_TOTAL 2 /* display total number of microseconds
			       (and cycles) spent in all the spys */
#define SPY_DISPLAY_SAMPLES 4

void spy_set_display_mode(int mode);

void spy_set_output(char *out);
/* Set the file name status is output. By default, status is sent to
   stdout. */

int get_mhz();
double cycle_to_usec(unsigned long cycles);
#ifdef __cplusplus
}
#endif

#endif

#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include "rpcc.h"

/* Calculate the microsecond unid difference between START and END. */
static long 
tv_diff (struct timeval *start, struct timeval *end)
{
    return ((long)end->tv_sec-start->tv_sec) * 1000 * 1000 + 
      end->tv_usec - start->tv_usec;
}

/* One trial to obtain the MHZ of the CPU. MHZ is guessed by comparing
   the cycle counter value and the gettimeofday value. To filter out
   random fluctuations, this proc is called ITR times and the median
   is taken. */
static double
get_mhz_itr ()
{
    struct timeval start, end;
    long elapsed;
    long x, y;
    gettimeofday(&start, 0);
    x = get_rpcc();
    for (;;) {
	gettimeofday(&end, 0);
	y = get_rpcc();
	if (tv_diff(&start, &end) > 100000)
	  break;
    }
    return (double)delta(x, y) / tv_diff(&start, &end);
}

#define ITR 5
static int 
cmp (double *a, double *b)
{
    if (*a < *b) return -1;
    if (*a > *b) return 1;
    return 0;
}

int 
get_mhz ()
{
    int i;
    static int mhz;
    double h[ITR];
    if (mhz != 0) return mhz;
    for (i = 0; i < ITR; i++) {
	h[i] = get_mhz_itr();
    }
    qsort(h, ITR, sizeof(double), cmp);
    mhz = h[ITR/2 + 1];
    return mhz;
}

double
cycle_to_usec (unsigned long cycles) 
{
    return (double)cycles / get_mhz();
}

unsigned int 
delta (unsigned long start,unsigned long stop)
{
    stop &= 0xffffffff;
    start &= 0xffffffff;

    if (stop<start) /* correct for wraparound */
      return 0x100000000-start+stop;
    
    return stop-start;

}	

#define MAX_SPY 128
static struct spy spys[MAX_SPY];
static int nspys;

spy_t
spy_create (char *name, int n_samples)
{
    void *malloc();
    spy_t s;
    if (nspys >= MAX_SPY) {
	printf("spy: number exceeded.\n");
	abort();
    }
    s = &spys[nspys++];
    s->name = name;
    s->active = 0;
    s->start = 0;
    s->cumulative = 0;
    s->max = 0;
    s->min = 0x7fffffff;
    s->n = 0;
    s->n_samples = n_samples;
    if (n_samples > 0) {
	s->samples = malloc(n_samples * sizeof(unsigned int));
	if (s->samples == 0) {
	    printf("spy_create(%s): malloc failed.\n", name);
	    s->n_samples = 0;
	}
    } else {
	s->samples = 0;
    }
    return s;
}

void 
spy_start (spy_t s) 
{
    if (s->active) {
	printf("spy: %s already active.\n", s->name);
    }
    s->active = 1;
    s->start = get_rpcc();
}

void
spy_stop (spy_t s)
{
    unsigned long stop = get_rpcc();
    unsigned d = delta(s->start, stop);
    if (!s->active) {
	printf("spy: %s not active.\n", s->name);
    }
    s->active = 0;
    s->cumulative += d;
    if (s->max < d) s->max = d;
    if (s->min > d) s->min = d;
    if (s->n < s->n_samples) {
	s->samples[s->n] = d;
    }
    s->n++;
}

static int 
compare (void *a, void *b)
{
    unsigned v1 = *(unsigned*)a;
    unsigned v2 = *(unsigned*)b;
    if (v1 == v2) {
	return 0;
    } else if (v1 > v2) {
	return 1;
    } else {
	return -1;
    }
}

long spy_average(spy_t s) {
    return s->cumulative / s->n;
}
long spy_median(spy_t s) {
    long n_samples = s->n > s->n_samples ? s->n_samples : s->n;
    if (n_samples > 0) {
	qsort(s->samples, n_samples, sizeof(unsigned), compare);
	return s->samples[n_samples/2];
    }
}

void
spy_dump (spy_t s)
{
    long average = 0;
    long median = 0;
    long n_samples = s->n > s->n_samples ? s->n_samples : s->n;

    if (s->n == 0) return;

    average = spy_average(s);
    median = spy_median(s);
    printf("%-9s: %5ld(%5.2f), %5ld(%5.2f), %5ld(%5.2f), %5ld(%5.2f), %5ld(%5.2f), %5ld.\n", 
	   s->name, 
	   s->min, cycle_to_usec(s->min),
	   s->max, cycle_to_usec(s->max),
	   average, cycle_to_usec(average),
	   median,  cycle_to_usec(median),
	   s->cumulative, cycle_to_usec(s->cumulative),
	   s->n);
}

void
spy_dump_all ()
{
    int i;
    printf("%-9s: %12s  %12s  %12s  %12s  %5s.\n",
	   "name", "min", "max", "average", "median", "cumulative", "hits");
    for (i = 0; i < nspys; i++) {
	spy_dump(spys+i);
    }
}

void 
spy_clear (spy_t s)
{
    s->active = 0;
    s->start = 0;
    s->cumulative = 0;
    s->n = 0;
}

void 
spy_clear_all ()
{
    int i;
    printf("name: min   max  average cumulative hits.\n");
    for (i = 0; i < nspys; i++) {
	spy_clear(spys+i);
    }
}

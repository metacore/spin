/*
 * HISTORY
 * 22-Sep-97  Yasushi Saito (yasushi) at the University of Washington
 *	whisted, and added Pentium support.
 *	 
 *	 
 */
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include "spy.h"

#ifdef i386
#include "spy_x86.c"
#else
#include "spy_alpha.c"
#endif


struct spy {
    char active;
    char *name;
    cycle_t start;
    unsigned long cumulative;
    unsigned long min, max;
    unsigned int n;
    int n_samples;
    unsigned int *samples;
    struct spy *siblings;
    struct spy *children;
};

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
    cycle_t x, y;
    gettimeofday(&start, 0);
    read_cycle_counter(&x);
    for (;;) {
	gettimeofday(&end, 0);
	read_cycle_counter(&y);
	if (tv_diff(&start, &end) > 100000)
	  break;
    }
    return (double)diff_cycle_counter(x, y) / tv_diff(&start, &end);
}

#define ITR 5
static int 
cmp_double (const void *a_, const void *b_)
{
    const double *a = a_;
    const double *b = b_;
    if (*a < *b) return -1;
    if (*a > *b) return 1;
    return 0;
}


/* Get the CPU clock rate. */
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
    qsort(h, ITR, sizeof(double), cmp_double);
    mhz = h[ITR/2 + 1];
    return mhz;
}

/* Convert clock cycle count into microsecs. 
   The CPU clock rate is assumed to be MHZ. */
double
cycle_to_usec (unsigned long cycles) 
{
    return (double)cycles / get_mhz();
}

#define MAX_SPY 128

static struct spy spys[MAX_SPY];
static int nspys;

extern void *malloc();

spy_t
spy_create (char *name, int n_samples)
{
    spy_t s;
    if (nspys >= MAX_SPY) {
	printf("spy: number exceeded.\n");
	abort();
    }
    s = &spys[nspys++];
    s->name = name;
    s->n_samples = n_samples;
    s->siblings = 0;
    s->cumulative = 0;
    s->children = 0;

    if (n_samples > 0) {
	s->samples = malloc(n_samples * sizeof(unsigned int));
	if (s->samples == 0) {
	    printf("spy_create(%s): malloc failed.\n", name);
	    s->n_samples = 0;
	}
    } else {
	s->samples = 0;
    }
    spy_clear(s);
    return s;
}

spy_t
spy_derive (spy_t parent, int n_samples)
{
    spy_t s = malloc(sizeof(struct spy));
    s->name = parent->name;
    s->n_samples = n_samples;
    s->children = 0;
    s->cumulative = 0;

    s->siblings = parent->children;
    parent->children = s;

    if (n_samples > 0) {
	s->samples = malloc(n_samples * sizeof(unsigned int));
	if (s->samples == 0) {
	    printf("spy_create(%s): malloc failed.\n", parent->name);
	    s->n_samples = 0;
	}
    } else {
	s->samples = 0;
    }
    spy_clear(s);
    return s;
}

void 
spy_start (spy_t s) 
{
    if (s->active) {
	printf("spy: %s already active.\n", s->name);
    }
    s->active = 1;
    read_cycle_counter(&s->start);
}

void
spy_stop (spy_t s)
{
    cycle_t stop;
    unsigned d;

    read_cycle_counter(&stop);
    d = diff_cycle_counter(s->start, stop);

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
cmp_unsigned (const void *a, const void *b)
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
	qsort(s->samples, n_samples, sizeof(unsigned), cmp_unsigned);
	return s->samples[n_samples/2];
    }
}

double spy_std_variation (spy_t s)
{
    int i;
    return 0;
}

static int mode;
static char *log_file;
static FILE *out;

void spy_set_display_mode (int mode_)
{
    mode = mode_;
}

void spy_set_output(char *path)
{
    log_file = malloc(strlen(path)+1);
    strcpy(log_file, path);
}

/* darn dec unix printf doesn't understand %*. */
static void print_fill (char *buf, int len)
{
    int n;
    n = len - strlen(buf);
    while (n > 0) {
	putc(' ', out);
	n--;
    }
    fputs(buf, out);
}

static char *r_dtoa (double x)
{
    static char buf[64];
    sprintf(buf, "%.2f", x);
    return buf;
}
static char *itoa (long x)
{
    static char buf[64];
    sprintf(buf, "%d", x);
    return buf;
}

static void open_output ()
{
    if (log_file) {
	out = fopen(log_file, "a");
	if (out == 0) {
	    perror(log_file);
	    out = stdout;
	}
    } else {
	out = stdout;
    }
}
static void close_output ()
{
    if (out != stdout) {
	fclose(out);
    }
}

static void
spy_accumulate_children_stats(spy_t s)
{
    spy_t child;

    if (!s->children) return;

    for (child = s->children; child; child = child->siblings) {
	int n;
	spy_accumulate_children_stats(child);

	s->cumulative += child->cumulative;
	if (child->max > s->max) s->max = child->max;
	if (child->min < s->min) s->min = child->min;

	/* Copy all the child's samples to parent's */
	for (n = 0; n < child->n; n++) {
	    if (n < child->n_samples && s->n < s->n_samples) {
		s->samples[s->n] = child->samples[n];
	    }
	    s->n++;
	}
	/*spy_clear(child);*/
    }
}

typedef struct tabstop {
  int name, min, max, mean, median, total, hits;
} tabstop;

int max(int a, int b)
{
    if (a > b) return a;
    return b;
}

static void set_tabstop(spy_t s, tabstop *t)
{
    t->name = max(t->name, strlen(s->name) + 3);
    t->min = max(t->min, strlen(r_dtoa(cycle_to_usec(s->min))) + 3);
    t->max = max(t->max, strlen(r_dtoa(cycle_to_usec(s->max))) + 3);
    t->mean = max(t->mean, strlen(r_dtoa(spy_average(s))) + 3);
    t->median = max(t->median, strlen(r_dtoa(cycle_to_usec(spy_median(s)))) + 3);
    t->total = max(t->total, strlen(r_dtoa(cycle_to_usec(s->cumulative))) + 3);
    t->hits = max(t->hits, strlen(itoa(s->n)) + 3);
}

static void init_tabstop (tabstop *t)
{
    memset(t, 0, sizeof (*t));
} 
void dump_header (tabstop *t) 
{
    print_fill("name", t->name);
    print_fill("min(us)", t->min);
    print_fill("max(us)", t->max);
    print_fill("mean(us)", t->mean);
    print_fill("median(us)", t->median);
    print_fill("total(us)", t->total);
    print_fill("hits(us)", t->hits);
    putc('\n', out);
}

void
spy_dump_internal (spy_t s, tabstop *t)
{
    long average = 0;
    long median = 0;
    long std_var;
    long n_samples;

    if (s->n == 0) return;
    n_samples = s->n > s->n_samples ? s->n_samples : s->n;    
    open_output();

    average = spy_average(s);
    median = spy_median(s);
    std_var = spy_std_variation(s);

    if (!(mode & SPY_DISPLAY_CYCLES)) {
	print_fill(s->name, t->name);
	print_fill(r_dtoa(cycle_to_usec(s->min)), t->min);
	print_fill(r_dtoa(cycle_to_usec(s->max)), t->max);
	print_fill(r_dtoa(cycle_to_usec(average)), t->mean);
	print_fill(r_dtoa(cycle_to_usec(median)), t->median);
	print_fill(r_dtoa(cycle_to_usec(s->cumulative)), t->total);
	print_fill(itoa(s->n), t->hits);
	putc('\n', out);
    } else {
	fprintf(out, "%-9s: %5ld(%5.2f), %5ld(%5.2f), %5ld(%5.2f), %5ld(%5.2f), %5ld(%5.2f), %5ld.\n", 
	       s->name, 
	       s->min, cycle_to_usec(s->min),
	       s->max, cycle_to_usec(s->max),
	       average, cycle_to_usec(average),
	       median,  cycle_to_usec(median),
	       s->cumulative, cycle_to_usec(s->cumulative),
	       s->n);
    }
    close_output();
}

static void
display_samples (spy_t s) {
    int i;
    fprintf(out, "%s samples:\n", s->name);
    for (i = 0; i < s->n; i++) {
	if (s->n > s->n_samples) break;
	fprintf(out, "%f\n", cycle_to_usec(s->samples[i]));
    }
    fprintf(out, ".\n");
}

void spy_dump (spy_t s)
{
    tabstop t;
    open_output();
    spy_accumulate_children_stats(s);

    if (mode == SPY_DISPLAY_SAMPLES) {
	display_samples(s);
    } else {
	init_tabstop(&t);
	set_tabstop(s, &t);
	dump_header(&t);
	spy_dump_internal(s, &t);
    }
}

void
spy_dump_all ()
{
    int i;
    unsigned long total = 0;
    tabstop t;
    spy_t s;
    
    open_output();
    init_tabstop(&t);

    for (i = 0; i < nspys; i++) {
	s = &spys[i];
	spy_accumulate_children_stats(s);
	if (s->n == 0) continue;

	if (mode != SPY_DISPLAY_SAMPLES) set_tabstop(s, &t);
	total += spys[i].cumulative;
    }
    if (mode == SPY_DISPLAY_SAMPLES) {
	for (i = 0; i < nspys; i++) {
	    s = &spys[i];
	    if (s->n == 0) continue;
	    display_samples(s);
	}
    } else {
	dump_header(&t);
	for (i = 0; i < nspys; i++) {
	    s = &spys[i];
	    if (s->n == 0) continue;
	    spy_dump_internal(s, &t);
	}
	if (mode & SPY_DISPLAY_TOTAL) {
	    fprintf(out, "total: %s.\n", r_dtoa(cycle_to_usec(total)));
	}
    }
}

void 
spy_clear (spy_t s)
{
    s->active = 0;
    s->start = 0;
    s->cumulative = 0;
    s->max = 0;
    s->min = 0x7fffffff;
    s->n = 0;
}

void 
spy_clear_all ()
{
    int i;
    for (i = 0; i < nspys; i++) {
	spy_clear(spys+i);
    }
}





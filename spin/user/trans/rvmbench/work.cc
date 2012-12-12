/*


     RVM: an Experimental Recoverable Virtual Memory Package
			Release 1.3

       Copyright (c) 1990-1994 Carnegie Mellon University
                      All Rights Reserved.

 Permission  to use, copy, modify and distribute this software and
 its documentation is hereby granted (including for commercial  or
 for-profit use), provided that both the copyright notice and this
 permission  notice  appear  in  all  copies  of   the   software,
 derivative  works or modified versions, and any portions thereof,
 and that both notices appear  in  supporting  documentation,  and
 that  credit  is  given  to  Carnegie  Mellon  University  in all
 publications reporting on direct or indirect use of this code  or
 its derivatives.

 RVM  IS  AN  EXPERIMENTAL  SOFTWARE  PACKAGE AND IS KNOWN TO HAVE
 BUGS, SOME OF WHICH MAY  HAVE  SERIOUS  CONSEQUENCES.    CARNEGIE
 MELLON ALLOWS FREE USE OF THIS SOFTWARE IN ITS "AS IS" CONDITION.
 CARNEGIE MELLON DISCLAIMS ANY  LIABILITY  OF  ANY  KIND  FOR  ANY
 DAMAGES  WHATSOEVER RESULTING DIRECTLY OR INDIRECTLY FROM THE USE
 OF THIS SOFTWARE OR OF ANY DERIVATIVE WORK.

 Carnegie Mellon encourages (but does not require) users  of  this
 software to return any improvements or extensions that they make,
 and to grant Carnegie Mellon the  rights  to  redistribute  these
 changes  without  encumbrance.   Such improvements and extensions
 should be returned to Software.Distribution@cs.cmu.edu.


*/

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <sys/time.h>
#include <setjmp.h>
#include <mach.h>
#include <limits.h>
#include "spy.h"
#include "bench.h"
#include "work.h"
#include "trans.h"

extern "C" int getpagesize();

extern unsigned long write_time; /* this holds the cumulative cycle numbers
				    spent in disk writes. 
				    defined in rvm_io.c */

class iterator {
  public:
    iterator(int maxelements);
    virtual char *name() = 0;
    virtual int next() = 0;
  protected:
    int maxelements;
};


/* There are three kinds of iterators, sequential, random and localized.
   
   Sequential iterator just picks numbers sequentially modulo MAXELEMENTS.
   Random iterator picks numbers between 0 .. MAXELEMENTS-1 equiprobably.
   Localized iterator favors some groups of numbers over others.

*/

class sequential_iterator : public iterator {
  public:
    sequential_iterator(int maxelements);
    char *name() {return "sequential";}
    int next();
  protected:
    int previndex;		/* previndex returned*/
};

#define MAXINTERVALS		10
class localized_iterator : public iterator {
  public:
    char *name() {return "localized";}
    int next();
    localized_iterator(int maxelements, int nintervals, int *tp, int *sp);
  protected:
    int elementsize;			/* how big each element is */
    int elementsperpage;		/* number of elements in a page*/
    int numberofpages;			/* total pages for fitting this array*/
    /* the following is needed only for "Localized" Locality function*/
    int npercents;			/* number of slots valid in array below */
    int trialpercentage[MAXINTERVALS];	/* keeps info about distribution of trials*/
    int spacepercentage[MAXINTERVALS];	/* keeps info about distribution of addresses*/
    /* eg. trialpercentage = [70, 95, 100] and 
       spacepercentage = [5, 20, 100] implies that 
       70% of refs. are for 5% of pages of records
       25% of refs are for 15% of pages of records
       and 5% of refs are for 80% of pages of records */
    int intervalendindex[MAXINTERVALS];
};

class random_iterator : public iterator {
  public:
    char *name() {return "random";}
    int next();
    random_iterator(int maxelements);
};

iterator::iterator(int max) {
    maxelements = max;
}

sequential_iterator::sequential_iterator(int max)
: iterator(max) {
    previndex = -1;
}

int sequential_iterator::next () {
    if (previndex == -1){
	previndex = 0;
	return 0;
    }
    previndex++;
    if (previndex >= maxelements) 
      previndex = 0;
    return(previndex);
}

/* Initialize the random number generator to a known state. 
   This is executed exactly once. */
static void
init_random () {
    static int random_initialized;
#define RANDOMSTATESIZE	256
    static char randomstate[RANDOMSTATESIZE];
    if (!random_initialized) {
	int i;
	unsigned randomseed;
	random_initialized = 1;
	/* set up random number generator */
	randomseed = 0;
	/* We have to use same seed to make the experiment fair. */
	for (i = 0; i < RANDOMSTATESIZE; i++)
	    randomstate[i] = ' ';
	initstate(randomseed, randomstate, RANDOMSTATESIZE);
    }
}


localized_iterator::localized_iterator (int max, int nintervals,
					int *tp, int *sp)
: iterator(max) {
    elementsize = MAXLENGTH;
    npercents = nintervals;
    elementsperpage = getpagesize() / elementsize;
    numberofpages = maxelements / elementsperpage;
    init_random();
    for (int i = 0; i < npercents; i++) {
	double pagesforinterval = (double)(sp[i] * numberofpages)/100;
	int integralpfi = (int) (pagesforinterval + 0.5);
	trialpercentage[i] = tp[i];
	spacepercentage[i] = sp[i];
	intervalendindex[i] = integralpfi * elementsperpage;
	/* hopefully there is atleast 1 page in this interval*/
	if (i > 0) {
	    assert(intervalendindex[i] > intervalendindex[i - 1]);
	}
	else {
	    assert(intervalendindex[i] > 0);
	}
    }
}

int localized_iterator::next () {
    double intervalbegin = 0.0;
    double intervalend;
    int i;
    int index;
    /* get a random number between 0 and 100 
       see which bucket it lies in;
       then offset into spacial_percentages and get range of 
       addresses you can fall in;
       use second random number to figure out which address to choose  */
    double d = (double)random() / INT_MAX * 100;

    for (i = 0; i < npercents; i++) {
	if (d < trialpercentage[i]) 
	  break;
    }
    assert(i < npercents);
    intervalbegin = 0;
    if (i > 0) 
      intervalbegin = (double)intervalendindex[i - 1];
    intervalend = (double)intervalendindex[i];
    index = ((double)random() / INT_MAX) * (intervalend-intervalbegin)
      + intervalbegin;
    return(index);
}

random_iterator::random_iterator (int max): iterator(max) {
    init_random();
}

int 
random_iterator::next () {
    return random() % maxelements;
}

int RANDOM = 1;
int NTRANS = 4000;
int NACCOUNTS = 32768;
int NHISTORY;
int do_barrier;
int do_profile;
int do_silent;
int do_spy;
int do_trace;
int do_pfm;
int do_time;
int trans_mode;
spy_t trans_spy, total_spy, null_spy;

#ifndef spin
#define USyscall_System(a)
#define USyscall_Profile(a)
#define trans_barrier(a)
#endif

static void
run_trial_with_locality (iterator *itr)
{
    int transexecuted;
    long begin_usec, end_usec;
    struct timeval tvb, tve;

    write_time = 0;
    if (do_spy) {
	spy_clear_all();
	USyscall_System("spy reset");
	USyscall_System("spy on");
    }
    if (do_pfm) {
	 USyscall_System("pfm setmux off0 dcache");
	 USyscall_System("pfm clear");
	 USyscall_System("pfm enable"); 
    }
    if (do_barrier) trans_barrier(do_barrier);
    if (do_profile) USyscall_Profile(1);
    
    gettimeofday(&tvb, 0);
    for (int i = 0; i < NTRANS; i++) {
	/* get index of account to modify*/
	int index = itr->next();
	basic_transaction(0, transexecuted, index, 1, NACCOUNTS);
	transexecuted++;
	if (do_trace) { printf("."); fflush(stdout); }
    }
    gettimeofday(&tve, 0);
    if (do_trace) { printf(" done\n"); fflush(stdout); }
    if (do_pfm) USyscall_System("pfm disable");
    if (do_profile) USyscall_Profile(0);
    if (do_spy) {
	USyscall_System("spy off");
	USyscall_System("spy dump");
    }

    if (!do_silent) {
	printf("%s: %lu transactions executed\n", itr->name(), NTRANS);
	begin_usec = (long)tvb.tv_sec * 1000000 + (long)tvb.tv_usec;
	end_usec = (long)tve.tv_sec * 1000000 + (long)tve.tv_usec;
	
	printf("%s: wallclock time = %f secs.\n", itr->name(),
	       (double)(end_usec-begin_usec) / 1000000);
	printf("%s: write time = %f secs.\n", itr->name(),
	       cycle_to_usec(write_time) / 1000000);
	spy_dump_all();
    }
    spy_clear_all();
}

void
run_trial () 
{
    iterator *itr;
    NHISTORY = NACCOUNTS*2;
    assert(NACCOUNTS + NHISTORY < MAXHISTORY);
    trans_spy = spy_create("trans", 0);
    total_spy = spy_create("total", 0);

    printf("This run: accounts = %d maxhistoryrecs = %d\n",
	   NACCOUNTS, NACCOUNTS*2);
    if (RANDOM) {
	itr = new random_iterator(NACCOUNTS);
    } else {
	int trialpercent[3];
	int spacepercent[3];
	trialpercent[0] = 70;
	trialpercent[1] = 95;
	trialpercent[2] = 100;

	spacepercent[0] = 5;
	spacepercent[1] = 20;
	spacepercent[2] = 100;
	/* read:
	   first 5% of the group receives 70% of the hit.
	   next 15% of the group receives 25% of the hit.
	   last 80% of the group receives 5% of the hit. 
	   */
	itr = new localized_iterator(NACCOUNTS, 3, trialpercent, spacepercent);
    }
    // Run the actual transactions 
    printf("%s start: accounts = %d history = %d\n", itr->name(), 
	   NACCOUNTS, NHISTORY);
    for (int i = 0; i < 3; i++) 
      run_trial_with_locality(itr);
    printf("Done\n");
}

void 
fatal(char*msg)
{
    fputs(msg, stderr);
    abort();
}

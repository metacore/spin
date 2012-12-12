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

#ifdef CAMELOT 
#include <camlib/camlib.h>
#include <cam/camelot_prefixed.h>
#include <cam/_setjmp.h>
#include <cam/cam_short_tran_types.h>
#include <cam/camelot.h>
#endif CAMELOT

#ifdef RVM
#include <rvm.h>
#include <rds.h>
#include <assert.h>
#endif RVM
#include "bench.h"

extern void basic_transaction(int, int, int, int);
extern int thisrunmaxhistory;
extern int thisrunmaxaccounts;

#include "work.h"

/* Get a random number between 0 and 1  */
double get_random_percent()
{
  double random_value;
  long random_number = random();
  random_value = (double)random_number / (double)2147483647;
  assert ((random_value >= 0.0) && (random_value <= 1.0));
  return (random_value);
}

/* Get a random integer between min and max */
double get_random_double(double min, double max)
{
  double random_value;
  double diff;
  double random_number;
  diff = max - min;

  random_number = get_random_percent();
  random_value = random_number*diff + min;
  if ((random_value < min) || (random_value > max)) {
    printf("random_number = %g\n", random_number);
    printf("min = %g, max = %g, diff\n", min, max, diff);
    printf("random_value = %g\n", random_value);
    fflush(stdout);
    exit(-1);
  }
  return(random_value);
}
void init(iterator *itr, int naccounts, Locality l, int elsize)  {
    itr->previndex = -1;
    itr->maxelements = naccounts;
    itr->elementsize = elsize;
    itr->locality = l;
    if ((itr->locality == Random) || (itr->locality == Localized)) {
	/* set up random number generator */
	struct timeval tmptv;
	unsigned randomseed;
	int i;
	printf("Initializing random number generator\n");

	gettimeofday(&tmptv, 0);
	randomseed = (unsigned)(tmptv.tv_usec >> 15);
	printf("The seed is %lu\n", randomseed);
	for (i = 0; i < RANDOMSTATESIZE; i++)
	    itr->randomstate[i] = ' ';
	initstate(randomseed, itr->randomstate, RANDOMSTATESIZE);
    }
    itr->pagesize = getpagesize();
    itr->elementsperpage = itr->pagesize / elsize;
    itr->numberofpages = itr->maxelements/itr->elementsperpage;
}

void itr_init(iterator *itr, int naccounts, Locality l, int elsize) {
    init(itr, naccounts, l, elsize);
}

void itr_init2(iterator *itr, 
	       int naccounts, Locality l, int elsize, int nintervals, 
	       int *tp /* trial percentages */, 
	       int *sp /* corresponding space percentages */ ) {
    int i;
    int pagessofar = 0;
    assert (l == Localized);
    assert (nintervals <= MAXINTERVALS);
    init(itr, naccounts, Localized, elsize);
    itr->npercents = nintervals;

    for (i = 0; i < itr->npercents; i++) {
	double pagesforinterval = (sp[i] * itr->numberofpages)/100;
	int integralpfi = (int) (pagesforinterval + 0.5);
	itr->trialpercentage[i] = tp[i];
	itr->spacepercentage[i] = sp[i];
	itr->intervalendindex[i] = integralpfi * itr->elementsperpage;
	/* hopefully there is atleast 1 page in this interval*/
	if (i > 0) {
	    assert(itr->intervalendindex[i] > itr->intervalendindex[i - 1]);
	}
	else {
	    assert(itr->intervalendindex[i] > 0);
	}
    }
    printf("The address ranges are \n");
    for (i = 0; i < itr->npercents; i++) 
	printf("For interval %d until entry %d\n", i, itr->intervalendindex[i]);
    
}



/* return index of next element on a new page depending on the 
locality policy*/
int itr_next(iterator *itr) {
    if (itr->locality == Sequential) {
	if (itr->previndex == -1){
	    itr->previndex = 0;
	    return 0;
	}
	itr->previndex++;
	if (itr->previndex >= itr->maxelements) 
	    itr->previndex = 0;
	return(itr->previndex);
    }
    else if (itr->locality == Random) {
	/* get a random index from the range */
	double r = get_random_percent();
	int i = (int)(r * itr->maxelements);
	return (i);
    }
    else {
	double intervalbegin = 0.0;
	double intervalend;
	int index;
	/*locality is localized
	  get a random number between 0 and 100 
	  see which bucket it lies in;
	  then offset into spacial_percentages and get range of 
	  addresses you can fall in;
	  use second random number to figure out which address to choose  */
	double d = get_random_double(0.0, 100.0);
	int i;
	for (i = 0; i < itr->npercents; i++) {
	    if (d < itr->trialpercentage[i]) 
		break;
	}
	assert(i < itr->npercents);
	intervalbegin = 0;
	if (i > 0) 
	    intervalbegin = (double)itr->intervalendindex[i - 1];
	intervalend = (double)itr->intervalendindex[i];
	index = (int)get_random_double(intervalbegin, intervalend);
	return(index);
    }
}

#define NTRANS 4000

void RunBench(int *ntransactions, struct timeval *tvb, struct timeval *tve, 
	      vm_statistics_data_t *vmb, vm_statistics_data_t *vma, 
	      Locality l) {
    /* set alarm for experiments (duration minutes long) */
    iterator next;
    int trialpercent[3];
    int spacepercent[3];
    int delta = 1;
    int transexecuted = 0;
    int i;

    trialpercent[0] = 70;
    trialpercent[1] = 95;
    trialpercent[2] = 100;

    spacepercent[0] = 5;
    spacepercent[1] = 20;
    spacepercent[2] = 100;

    if (l == Localized) 
      itr_init2(&next, thisrunmaxaccounts, l, MAXLENGTH,
		3, trialpercent, spacepercent);
    else 
      itr_init(&next, thisrunmaxaccounts, l, MAXLENGTH);

    gettimeofday(tvb, 0);

    for (i = 0; i < NTRANS; i++) {
	/* get index of account to modify*/
	int index = itr_next(&next);
	basic_transaction(0, transexecuted, index, delta);
	transexecuted++;
    }
    gettimeofday(tve, 0);
    *ntransactions = NTRANS;
}


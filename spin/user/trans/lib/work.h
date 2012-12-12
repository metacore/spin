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

#define RANDOMSTATESIZE	256
#define MAXINTERVALS		10

typedef struct iterator {
    int maxelements;			/* total number of elements in array */
    Locality locality;			/* kind of locality function */
    int previndex;			/* previndex returned (for sequential only)*/
    int pagesize;			/* pagesize on the system*/
    int elementsize;			/* how big each element is */
    int elementsperpage;		/* number of elements in a page*/
    int numberofpages;			/* total pages for fitting this array*/
    char randomstate[RANDOMSTATESIZE];	/* used for initing the random number generator*/
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
} iterator;

#ifdef __cplusplus
extern "C" {
#endif

void itr_init(iterator*, int, Locality, int);
void itr_init2(iterator*, int, Locality, int, int, int *, int *);
void itr_destroy(iterator*);


extern void RunBench(int *, struct timeval *, struct timeval *, 
		     vm_statistics_data_t *, vm_statistics_data_t *,
                     Locality);
#ifdef __cplusplus
}
#endif

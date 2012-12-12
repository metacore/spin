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
#include <signal.h>
#include <strings.h>
#include <nlist.h>
#include <setjmp.h>
#include <sys/types.h>
#include <sys/resource.h>
#include <sys/time.h>
#include <sys/dk.h>
#include <sys/file.h>

#include "bench.h"
#include "work.h"
#include "trans.h"

static int tstamp = 0;
static int next_hindex = 0;

void
randomly_modify (void *x_, int size)
{
    int i;
    long *x = (long*)x_;
    size /= sizeof(long);
    for (i=0; i < size; i++) {
	x[i]++;
    }
}

void
basic_transaction (int bid, int tranid, int aid, int delta, int nacs) 
{
    history newrecord;
    account ac;
    teller tlr;
    branch br;
    int status = 0;
    account *accounts;
    history *histories;

    spy_start(trans_spy);

    BEGIN_TRANSACTION();
      
    accounts = (account *)(&REC(accountsandhistory)[0]);
    histories = &REC(accountsandhistory)[2 * nacs]; 

    /* first modify the account balance - modifies MAXLENGTH bytes */
    ac = accounts[aid];
    randomly_modify(&ac, sizeof(ac));
    /*ac.acct_balance += delta;*/
    MODIFY_BYTES(&(accounts[aid]), &ac, sizeof(account));

    /* modify the teller next */
    tlr = REC(tellers[tranid]);
    randomly_modify(&tlr, sizeof(tlr));
    /*tlr.teller_balance += delta;*/
    MODIFY_BYTES(&REC(tellers)[tranid], &tlr, sizeof(teller));

    /* modify branch */
    br = REC(branches[0]);
    randomly_modify(&br, sizeof(br));
    /*br.branch_balance += delta;*/
    MODIFY_BYTES(&REC(branches)[0], &br, sizeof(branch));
    
    /* modify the history record */
    newrecord.trans_id = tranid;
    newrecord.branch_id = bid;
    newrecord.acct_id = aid;
    newrecord.delta = delta;
    newrecord.tstamp1 = tstamp++;
    newrecord.tstamp2 = tstamp++;
    MODIFY_BYTES(&(histories[next_hindex]), 
		 &newrecord, sizeof(history));
    next_hindex = (next_hindex + 1);
    if (next_hindex >= NHISTORY) next_hindex = 0;
    END_TRANSACTION();
    spy_stop(trans_spy);
}

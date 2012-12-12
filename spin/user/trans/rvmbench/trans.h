#ifndef __TRANS_H
#define __TRANS_H

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

/* bench.h 
 * This file contains the definitions of the structures used by TPC A benchmark
 * This is used both by the rvm and the camelot version of the benchmark
 */

#define MAXLENGTH	128
#define NBRANCHES	1
#define NTELLERS	10
#define MAXHISTORY	1 << 24
#define MAXDELTA	1 << 8

typedef struct branch{
    int branch_id;
    int branch_balance;
    char filler[MAXLENGTH - sizeof(int) - sizeof(int)];
} branch;

typedef struct teller {
    int teller_id;
    int branch_id;
    int teller_balance;
    char filler[MAXLENGTH - (3 * sizeof(int))];
} teller;

typedef struct account{
    int acct_id;
    int branch_id;
    int acct_balance;
    char filler[MAXLENGTH - (3 * sizeof(int))];
} account;

typedef struct history {
    int trans_id;
    int branch_id;
    int acct_id;
    int delta;
    int tstamp1;
    int tstamp2;
    char filler[(MAXLENGTH/2) - (6 * sizeof(int))];
} history;

typedef struct root_struct {
    int already_init;
    branch branches[NBRANCHES];
    teller tellers[NTELLERS];
    history accountsandhistory[MAXHISTORY];
} root_struct;

void basic_transaction(int bid, int tranid, int aid, int delta, int nacs);
#endif


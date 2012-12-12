/*
 * HISTORY
 * 02-May-97  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 */
#ifndef __WORK_H
#define __WORK_H

#include <spy.h>

extern int RANDOM;
extern int NTRANS; /* # of transactions to be executed in total. */
extern int NACCOUNTS; /* # of accounts in the database */
extern int NHISTORY; /* max # of history records in the database. */
extern int do_spy, do_pfm, do_profile, do_barrier; /* cmd line flags. */
extern int do_silent, do_trace;
extern spy_t trans_spy, total_spy, null_spy;
extern int trans_mode;
void run_trial();

#endif

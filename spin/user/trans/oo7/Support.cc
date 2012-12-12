#include "macrodef.h"
extern void *DesignLib;

#include "GenList.h"
#include "GenVHSet.h"
#include "GenVHBag.h"
#include "GenBBag.h"
#include "GenAVLIndex.h"

#include "OO7.h"
#include "BenchParams.h"
#include "GenParams.h"
#include "VarParams.h"

extern int debugMode;

void JoinDoNothing(int id1, int id2)
{
    if (debugMode) {
        cout << "==> DoNothing4(id1 = " << id1 << "id2 = " << id2 << "\n";
    }
}

static double 
normalize (double seconds, double useconds)
{
    if (useconds < 0.0) {
        useconds = 1000000.0 + useconds;
        seconds--;
    }
    return (seconds + useconds/1000000.0);
}

double ComputeWallClockTime(struct timeval *startWallTime, 
			    struct timeval *endWallTime)
{
    double seconds, useconds;

    seconds = (double) (endWallTime->tv_sec - startWallTime->tv_sec);
    useconds = (double) (endWallTime->tv_usec - startWallTime->tv_usec);
    return normalize(seconds, useconds);
}


double ComputeUserTime(struct rusage *start, struct rusage *end)
{
    double seconds, useconds;

    seconds = (double)(end->ru_utime.tv_sec - start->ru_utime.tv_sec);
    useconds = (double)(end->ru_utime.tv_usec - start->ru_utime.tv_usec);
    return normalize(seconds, useconds);
}

double ComputeSystemTime(struct rusage *start, struct rusage *end)
{
    double seconds, useconds;

    seconds = (double) (end->ru_stime.tv_sec - start->ru_stime.tv_sec);
    useconds = (double) (end->ru_stime.tv_usec - start->ru_stime.tv_usec);
    return normalize(seconds, useconds);
}



// compute number of page faults
long
ComputeNumPageFaults(struct rusage *start,
                     struct rusage *end)
{
  return (end->ru_majflt - start->ru_majflt);
}

// compute number of signals received
long
ComputeNumSignals(struct rusage *start,
                     struct rusage *end)
{
  return (end->ru_nsignals - start->ru_nsignals);
}

// compute number of swaps
long
ComputeNumSwaps(struct rusage *start,
                     struct rusage *end)
{
  return (end->ru_nswap - start->ru_nswap);
}


void PrintOp(BenchmarkOp op)
{
    switch (op) {
        case Trav1:
            cout << "Trav1" ;
	    break;
        case Trav2a:
            cout << "Trav2a" ;
	    break;
        case Trav2b:
            cout << "Trav2b" ;
	    break;
        case Trav2c:
            cout << "Trav2c" ;
	    break;
        case Trav3a:
            cout << "Trav3a" ;
	    break;
        case Trav3b:
            cout << "Trav3b" ;
	    break;
        case Trav3c:
            cout << "Trav3c" ;
	    break;
        case Trav4:
            cout << "Trav4" ;
	    break;
        case Trav5do:
            cout << "Trav5do" ;
	    break;
        case Trav5undo:
            cout << "Trav5undo" ;
	    break;
        case Trav6:
            cout << "Trav6" ;
	    break;
        case Trav7:
            cout << "Trav7" ;
	    break;
        case Query1:
            cout << "Query1" ;
	    break;
        case Query2:
            cout << "Query2" ;
	    break;
        case Query3:
            cout << "Query3" ;
	    break;
        case Query4:
            cout << "Query4" ;
	    break;
        case Query5:
            cout << "Query5" ;
	    break;
        case Query6:
            cout << "Query6" ;
	    break;
        case Query7:
            cout << "Query7" ;
	    break;
        case Query8:
            cout << "Query8" ;
	    break;
        case Insert:
            cout << "Insert" ;
	    break;
        case Delete:
            cout << "Delete" ;
	    break;
        case Reorg1:
            cout << "Reorg1" ;
	    break;
        case Reorg2:
            cout << "Reorg2" ;
	    break;
        case MultiTrav1:
            cout << "MultiTrav1" ;
	    break;
        case MultiTrav2:
            cout << "MultiTrav2" ;
	    break;
        case MultiTrav3:
            cout << "MultiTrav3" ;
	    break;
        case MultiTrav4:
            cout << "MultiTrav4" ;
	    break;
        case MultiTrav5:
            cout << "MultiTrav5" ;
	    break;
        case MultiTrav6:
            cout << "MultiTrav6" ;
	    break;
    }
}

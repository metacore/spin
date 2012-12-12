/*
 *
 * Copyright 1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 */


/*
 * HISTORY
 * 08-Mar-97  Wilson Hsieh (whsieh) at the University of Washington
 *	added new variables
 *
 * 27-Jan-97  Wilson Hsieh (whsieh) at the University of Washington
 *      created
 *
 */


void **TransactionQueue;
unsigned long NilOld, NilNew, OldEqualNew,
              AllocationClock,
              HeapClock, HeapTotal, HeapTotal2, HeapCount,
              LocalClock, LocalTotal, LocalTotal2, LocalCount,
              GlobalClock, GlobalTotal, GlobalTotal2, GlobalCount,
              PossibleHeap, PossibleGlobal, PossibleLocal;


/*
 * these functions need to be fixed to be atomic!
 */


void *TraceRefHeap (void **addr, void *rhs, unsigned long tc,
		    unsigned long definite)
{
  void *old = *addr;
  *addr = rhs;
  return old;
}


void *TraceRefGlobal (void **addr, void *rhs, unsigned long tc,
		      unsigned long definite)
{
  void *old = *addr;
  *addr = rhs;
  return old;
}


void TraceCount (unsigned long tc, unsigned long definite)
{
}

void AssignTracedToGlobal (void **addr, void *rhs)
{
  *addr = rhs;
}

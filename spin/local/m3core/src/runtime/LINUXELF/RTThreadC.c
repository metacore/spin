/* Copyright (C) 1990, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */
/* Last modified on Mon Dec  6 16:14:35 PST 1993 by kalsow     */
/*      modified on Tue Jan 19 15:20:48 PST 1993 by burrows    */

/* This file implements the coroutine transfer: RTThread.Transfer */

/* HISTORY
 * 28-Jan-97  Marc Fiuczynski (mef) at the University of Washington
 *	Added RT0u__types and RT0u__RunTimeEpoch so we can compile the
 *	file using the SPIN M3 compiler.
 *
 */

#include <setjmp.h>

RTThread__Transfer (from, to)
jmp_buf *from, *to;
{
  if (__setjmp(*from) == 0) __longjmp (*to, 1);
}


/* global thread ID used by 'etp' */
int ThreadF__myId = 1;

/* low-level runtime lock */
int RT0u__inCritical = 0;

/* global, per-thread linked list of exception handlers */
void* RTThread__handlerStack = 0;

/* an array of pointers to typecells, kept as an EXTERNAL to 
   save one load (through interface) on each NARROW */
long *RT0u__types = 0;

/* */
long RT0u__RuntimeEpoch = 0;


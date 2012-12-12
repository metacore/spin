/* Copyright (C) 1990, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */
/* Last modified on Mon Dec  6 16:10:27 PST 1993 by kalsow     */
/*      modified on Tue Jan 19 15:20:48 PST 1993 by burrows    */

/* This file implements the coroutine transfer: RTThread.Transfer */

/*
 * HISTORY
 * 15-Nov-95  Przemek Pardyak (pardy) at the University of Washington
 *	Added a counter of reconfigurations.  Used to distinguish
 *	incarnations of the runtime after each dynamic linking.
 *
 */

#include <setjmp.h>

RTThread__Transfer (from, to)
jmp_buf *from, *to;
{
  if (_setjmp(*from) == 0) _longjmp (*to, 1);
}


/* global thread ID used by 'etp' */
int ThreadF__myId = 1;

/* low-level runtime lock */
int RT0u__inCritical = 0;

/* global, per-thread linked list of exception handlers */
void* RTThread__handlerStack = 0;

/* reinitialization counter */
int RT0u__RuntimeEpoch = 0;

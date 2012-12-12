(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 31-May-97  David Becker at the University of Washington
 *	Renamed ThreadForSAL to SalSync.
 *	This module provides sync services for underlying Sal code.
 *
 * 05-May-96  Przemek Pardyak (pardy) at the University of Washington
 *	Made SalThread return result, this was expected in kern_malloc.c
 *
 * 15-Feb-96  Emin Gun Sirer (egs) at the University of Washington
 *	Added WakeupOne. Changed Wakeup to awaken all waiting threads.
 *
 * 04-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	Changed KThread to Thread and ThreadExtra.
 *
 * 25-Oct-95  Emin Gun Sirer (egs) at the University of Washington
 *	Added interface for C-land to do a voluntary heap check.
 *      Changed the mutex operations to be GC-safe, by encapsulating
 *      a traced strongrefed mutex in a wrapper structure. The strongref
 *      handle is kept in the structure and is used to deallocate it.
 *
 * 05-Oct-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Interface to support clients who want to see a Mach/OSF
 *      thread interface.
 *)
INTERFACE SalSync;
IMPORT ThreadExtra, Thread, Word;

TYPE EventT = INTEGER;

TYPE CSideMutex <: REFANY;

(*
 * Support for device drivers that want to see a Mach interface
 *)
PROCEDURE CurrentThread() : Thread.T;

PROCEDURE KernelThread(cfunc: PROCEDURE(carg: ADDRESS); carg: ADDRESS)
                       : Thread.T;

PROCEDURE BlockWithContinuation(continuation: ThreadExtra.FuncT;
                                arg: ThreadExtra.ArgT);

PROCEDURE Sleep();

PROCEDURE SetTimeout(thread: Thread.T; timeout: Word.T);

PROCEDURE AssertWait(eventhandle: EventT);

PROCEDURE ThreadWakeup(eventhandle: EventT);

PROCEDURE ThreadWakeupOne(eventhandle: EventT);

PROCEDURE ClearWait(thread: Thread.T);

PROCEDURE AllocateMutex() : CSideMutex;

PROCEDURE DeallocateMutex(mu : CSideMutex);

PROCEDURE AcquireMutex(mu: CSideMutex);

PROCEDURE ReleaseMutex(mu: CSideMutex);

PROCEDURE Init(verbose: BOOLEAN);

END SalSync.



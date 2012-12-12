(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 *
 * 24-Aug-97  Przemek Pardyak (pardy) at the University of Washington
 *	Cleaned-up.  Renamed Mutex to MutexSanity.
 *
 * 03-Dec-97  Robert Grimm (rgrimm) at the University of Washington
 *      Added Security and SecurityStatistics.
 *
 * 16-Oct-97  Tian Fung Lim (tian) at the University of Washington
 *	Added MeasureIdle
 *
 * 28-Sep-97  Yasushi Saito (yasushi) at the University of Washington
 *	Added Mutex, deleted debugSPIN.
 *
 * 20-Dec-96  Yasushi Saito (yasushi) at the University of Washington
 *	Deleted Space.
 *
 * 24-Feb-97  Emin Gun Sirer (egs) at the University of Washington
 *	Added DoCleanUntraced, VerboseInit, DoStrongrefTiming.
 *
 * 23-Feb-97  Marc Fiuczynski (mef) at the University of Washington
 *	Added option to turn of TrackStrand conditionally.
 *
 * 15-May-96  Marc Fiuczynski (mef) at the University of Washington
 *    Clean up.
 *
 * 12-Mar-96  Emin Gun Sirer (egs) at the University of Washington
 *    Obsoleted Print. It should be deleted by spin-19.
 *
 * 28-Aug-95  Emin Gun Sirer (egs) at the University of Washington
 *	Made variables into consts to trigger dead-code elimination.
 *
 * 09-May-95  Stefan Savage (savage) at the University of Washington
 *	Created. Added Debug booleans for conditional debug messages
 *)
INTERFACE DebugOption;

CONST
  Sched                 = FALSE;  (* Scheduler debugging into log.       *)
  Cswtch                = FALSE;  (* Make a log entry upon each cswtch.  *)
  DoTimings             = FALSE;  (* Create Spy timers for threads etc.  *)
  TrapFrames            = FALSE;  (* Trap frame management.              *)
  DoRedZones            = FALSE;  (* Put RedZones after kernel stacks.  *)
  DoStrandDebug         = TRUE;   (* Track active strands for the debugger. *)
  MutexSanity           = FALSE;  (* mutex & condition sanity checks *)
  DebuggerDebug         = FALSE;  (* Debug the debugger support. *)
  DoCleanUntracedStacks = FALSE;  (* Register stacks as clean regions *)
  VerboseInit           = TRUE;   (* print info on boot *)
  DoStrongrefTiming     = FALSE;  (* timing of strongref operations  *) 
  DoTrackStrand         = FALSE;  (* track resources used by strands *)
  Translation           = FALSE;  (* Pmap sanity checking *)
  PhysAddr              = FALSE;  (* Page frame sanity checking *)
  MeasureIdle           = FALSE;  (* Measure idle time *)
  Security              = TRUE;   (* Enable security management *) 
  SecurityStatistics    = FALSE;  (* Spy counts for access operations *)
  DoThreadForSAL        = FALSE;

END DebugOption.

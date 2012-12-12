(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 31-May-97  David Becker at the University of Washington
 *      Replace SAL with Kernel, Clock and CPU interfaces
 *
 * 20-May-96  David Dion (ddion) at the University of Washington
 *	Created for time-related system calls.
 *
 *)
MODULE TimeHandlers;

IMPORT Strand, CPU;
IMPORT Sema, Clock, UserSpaceThread, Word, Textify, Fmt;
IMPORT HandlerUtils;
FROM HandlerUtils IMPORT Print;

(* The MaintainTime procedure fakes mapped time in the server.  Every
   "ticks" ticks it overwrites the global time variable in the server
   with the updated time.  No locking is needed on the global time 
   variable since interrupts are disabled during a Space.Write.
   MaintainTime works by setting a timeout for "ticks" ticks.  The
   timeout function is a wrapper function which NARROWs its argument
   to a semaphore and kicks it.  The MaintainTime procedure is waiting
   on this semaphore.  When kicked, it obtains the new time from the
   kernel and overwrites it in the server.  It then sets a new timeout
   and waits.  MaintainTime never returns. *)
PROCEDURE TimeSemaWrapper(args: REFANY) =
  BEGIN
    WITH sema = NARROW(args, Sema.T) DO
      Sema.V(sema);
    END;
  END TimeSemaWrapper;

PROCEDURE MaintainTime(strand: Strand.T; VAR ms: CPU.SavedState) =
  VAR 
    timeSema: Sema.T;
    t: Clock.TimeVal;
    ticks: CARDINAL := 200; (* ??? *)
  BEGIN
    timeSema := Sema.Alloc(0);
    WITH space = UserSpaceThread.GetSpace(NARROW(strand, UserSpaceThread.T)) DO
      LOOP
        Clock.TimeOfDay(t);
        WITH timeArray = VIEW(t, ARRAY [0..7] OF CHAR) DO
          IF NOT HandlerUtils.CopyOut(space, timeArray, ms.a0, 8) THEN
            HandlerUtils.PrintError("MaintainTime: CopyOut failed!  " & 
              "Continuing anyway.\n");
          END;
        END;
        Clock.SetAlarm(ticks, TimeSemaWrapper, timeSema);
        Sema.P(timeSema);
      END;
    END;
    (* This procedure never returns *)
  END MaintainTime;

VAR
  SoftClockSema: Sema.T;
  SoftClockInit: BOOLEAN := FALSE;
  SoftClockWaitingForAlarm: BOOLEAN := FALSE;
  SoftClockMu: MUTEX;
  SoftClockGotPoked: BOOLEAN := FALSE;

PROCEDURE SetSoftclock(<* UNUSED *>strand: Strand.T; 
                       VAR ms: CPU.SavedState) =
  VAR
    timeoutmsecs: Word.T;
    timeoutTicks: Word.T;
    semaIsReset: BOOLEAN := TRUE;
  BEGIN
    IF NOT SoftClockInit THEN
      (* initialize the synchronization mechanisms *)
      SoftClockSema := Sema.Alloc(0);
      SoftClockMu := NEW(MUTEX);
      SoftClockInit := TRUE;
    ELSE
      (* reset the synchronization mechanisms *)
      REPEAT
        semaIsReset := TRUE;
        TRY
          Sema.Reset(SoftClockSema);
        EXCEPT
        | Sema.InUse =>
          Sema.V(SoftClockSema);
          semaIsReset := FALSE;
        END;
      UNTIL semaIsReset;
    END;

    timeoutmsecs := ms.a0;
    IF timeoutmsecs = 0 THEN (* wait forever until "poked" *)
      SoftClockWaitingForAlarm := FALSE;
      Sema.P(SoftClockSema);
      ms.v0 := 1; (* return 1 if "poked" *)
    ELSE (* wait specified number of clock ticks *)
      timeoutTicks := (timeoutmsecs * Clock.InterruptRate()) DIV 1000 ;
      LOCK SoftClockMu DO
        Clock.SetAlarm(timeoutTicks, TimeSemaWrapper, SoftClockSema);
        SoftClockWaitingForAlarm := TRUE;
      END;
      Sema.P(SoftClockSema);
      LOCK SoftClockMu DO
        SoftClockWaitingForAlarm := FALSE;
        IF SoftClockGotPoked THEN
          SoftClockGotPoked := FALSE;
          ms.v0 := 1;
          RETURN;
        END;
      END;
      ms.v0 := 0;
    END;
  END SetSoftclock;

PROCEDURE PokeSoftclock(<* UNUSED *>strand: Strand.T; 
                        <* UNUSED *>VAR ms: CPU.SavedState) =
  BEGIN
    (* Don't try to lock the mutex if it hasn't been initialized. *)
    REPEAT
      Strand.Yield();
    UNTIL SoftClockInit;

    LOCK SoftClockMu DO
      IF SoftClockWaitingForAlarm THEN
        SoftClockGotPoked := TRUE;
        EVAL Clock.CancelAlarm(TimeSemaWrapper, SoftClockSema);
      END;
      Sema.V(SoftClockSema); (* OK if signaled twice since it gets reset *)
    END;
  END PokeSoftclock;

TYPE Sample = RECORD
  cycles: Word.T;
  id: Strand.T;
END;

CONST
  NUMTIMERS = 1; (* 9; *)
  MAXSAMPLES = 1; (* 200; *)
VAR
  data: ARRAY[0..NUMTIMERS-1] OF ARRAY[0..MAXSAMPLES-1] OF Sample;
  samplenumber: ARRAY[0..NUMTIMERS-1] OF INTEGER;

PROCEDURE TimerInit(timer: INTEGER) =
  BEGIN
    IF timer < 0 THEN
      FOR i := 0 TO NUMTIMERS-1 DO
        FOR j := 0 TO MAXSAMPLES-1 DO
          data[i][j].cycles := 0;
          data[i][j].id := NIL;
        END;
        samplenumber[i] := 0;
      END;
    ELSE
      FOR j := 0 TO MAXSAMPLES-1 DO
        data[timer][j].cycles := 0;
        data[timer][j].id := NIL;
      END;
      samplenumber[timer] := 0;
    END;
  END TimerInit;

PROCEDURE TimerSample(timer: INTEGER; id: Strand.T) =
  VAR sample: Word.T;
  BEGIN
    sample := CPU.CycleCounter();
    sample := Word.And(sample, 16_00000000ffffffff);

    IF (samplenumber[timer] < MAXSAMPLES) THEN
      data[timer][samplenumber[timer]].cycles := sample;
      data[timer][samplenumber[timer]].id := id;
      INC(samplenumber[timer]);
    ELSE
      HandlerUtils.PrintError("timer filled up: " & Fmt.Int(timer) & "\n");
    END;
  END TimerSample;

PROCEDURE TimerPrint(timer: INTEGER) =
  BEGIN
    Print("Timer #" & Fmt.Int(timer) & "\n");
    FOR j := 0 TO samplenumber[timer]-1 DO
      Print("id " & Textify.Ref(data[timer][j].id) & "\n" & 
        Fmt.Unsigned(data[timer][j].cycles) & "\n");
    END;
  END TimerPrint;


BEGIN
END TimeHandlers.


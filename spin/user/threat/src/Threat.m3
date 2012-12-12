(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 31-May-97  David Becker at the University of Washington
 *      Replace SAL with Kernel and MachineCPU interfaces
 *
 * 10-May-97  Tian Fung Lim (tian) at the University of Washington
 *	Added time mode.
 *
 * 14-Nov-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added quiet and verbose modes.
 *
 * 29-Apr-96  Emin Gun Sirer (egs) at the University of Washington
 *	Changed to figure out and print elapsed time.
 *
 * 27-Mar-96  Brian Bershad (bershad) at the University of Washington
 *	Created.
 *
 *)
UNSAFE
MODULE Threat;

IMPORT Thread, IO, ParseParams, ThreadExtra, Rd, Fmt, ThreatCmd;
IMPORT Clock;

(*
 * threat:  beat up on cthreads.
 *)

CONST MAXTHREADS = 20;
CONST MAX_SPIN_DELAY = 10;

TYPE SlotObj = RECORD
	thread: Thread.T;
	occupied : BOOLEAN;
END;

TYPE Seconds = INTEGER;

VAR slots : ARRAY [0..MAXTHREADS-1] OF SlotObj;
    slotmu := NEW(MUTEX);
    testing: BOOLEAN;	 (* testing still in progress *)
    numThreads: CARDINAL := 0;		(* number of threads alive *)
    actThreads: CARDINAL := 0;          (* number of threads active *)

VAR
  verbose: BOOLEAN := FALSE;
  (* for time limits *)
  TimeLimit : BOOLEAN := FALSE;
  TimeLimitSeconds : INTEGER := 0;

PROCEDURE Assert (cond: BOOLEAN; msg: TEXT) =
  BEGIN
    IF NOT cond THEN
      IO.Put("Assertion failed: " & msg);
      <* ASSERT FALSE *>
    END;
  END Assert;

PROCEDURE Spin (loops: INTEGER) =
  BEGIN 
    FOR i := 0 TO loops DO ThreadExtra.Yield(); 
    END;
 END Spin;


PROCEDURE AwaitChar () =
  VAR rd: Rd.T := ThreadExtra.GetRdSelf();
  BEGIN
    LOOP
      TRY
        IF Rd.CharsReady(rd) > 0 THEN
          EVAL Rd.GetChar(rd);
          RETURN;
        ELSE
          ThreadExtra.Yield();
        END;
      EXCEPT
        Rd.EndOfFile, Rd.Failure, Thread.Alerted => (* do nothing *)
      END;
    END;
  END AwaitChar;

(******************************)
(* Terminal Printout Routines *)
(******************************)
PROCEDURE Time() : Seconds =
  VAR t: Clock.TimeVal;
  BEGIN
    Clock.TimeOfDay(t);
    RETURN t.tv_sec;
  END Time;

VAR printmu := NEW(MUTEX);
    tcCount : CARDINAL;
    startTime : Seconds;
    elapsedTime: Seconds;

PROCEDURE PrintThreadCount (tc: CARDINAL) =
  BEGIN
    IF tcCount = 20 THEN
      tcCount := 0;
      IO.Put("\n");
      elapsedTime := Time() - startTime;
      IO.PutInt(elapsedTime DIV 3600,2);
      IO.Put(":");
      elapsedTime := elapsedTime MOD 3600;
      IO.PutInt(elapsedTime DIV 60,2);
      IO.Put(":");
      elapsedTime := elapsedTime MOD 60;
      IO.PutInt(elapsedTime,2);
      IO.Put(">");
    END;
    IO.Put(Fmt.Int(tc));
    INC(tcCount);
  END PrintThreadCount;

PROCEDURE PrintOpCount () =
  BEGIN
    IF tcCount MOD 1000 = 0 THEN
      IO.Put(Fmt.Int(tcCount DIV 1000) & " ");
    END;
    INC(tcCount);
  END PrintOpCount; 

(*****************************)
(*  Random Number Generator  *)
(*****************************)

CONST AA = 38737;
CONST CC = 131071;
CONST MM = 43691;

VAR seed : CARDINAL;

PROCEDURE Random(minval: INTEGER; maxval: INTEGER) : INTEGER =
  VAR span: INTEGER;
 BEGIN
	IF minval > maxval THEN
          RETURN minval;
        ELSE
  	  span := (maxval - minval) + 1;
	  seed := (AA*seed + CC) MOD MM;
	  RETURN (minval + (seed MOD span));
        END;
 END Random;

(****************)
(*  Mutex Test  *)
(****************)

VAR mCountMu := NEW(MUTEX);
    mTestMu := NEW(MUTEX);
    mCount : CARDINAL;
    holder: Thread.T;

PROCEDURE HitMutex () =
  VAR
    maxThreshold: INTEGER;
    threshold   : INTEGER;
  BEGIN
    maxThreshold := MAXTHREADS DIV 4;
    LOCK mCountMu DO
      INC(mCount); 
    END;

    LOCK mTestMu DO
      holder := Thread.Self();
      LOOP
        Spin(MAX_SPIN_DELAY);
        threshold := Random(0, maxThreshold);
        IF threshold < mCount THEN EXIT; END;
      END;

      IF verbose THEN
        LOCK printmu DO
          IO.Put("[");
          PrintThreadCount(mCount);
          IO.Put("]");
        END;
      END;
    END;
    LOCK mCountMu DO
      DEC(mCount);
    END;
  END HitMutex;

(*****************************)
(*  Condition Variable Test  *)
(*****************************)

VAR cv := NEW(Thread.Condition);
    cvCount : INTEGER := 0;

PROCEDURE HitCV () =
  VAR
    maxThreshold : INTEGER;
    threshold    : INTEGER;
  BEGIN
    maxThreshold := MAXTHREADS DIV 2;
    LOCK slotmu DO
      threshold := Random(0, MIN(maxThreshold, numThreads - 1));
      IF testing AND cvCount < threshold THEN
        INC(cvCount);
        DEC(actThreads);
        IF actThreads = 0 THEN
          LOCK printmu DO
            IO.Put("[ZERO ACTIVE: " & Fmt.Int(actThreads) & " " & 
              Fmt.Int(cvCount) & " " & 
              Fmt.Int(numThreads) & " ]");
          END;
        END;
        Thread.Wait(slotmu, cv);
        INC(actThreads);
      ELSE
        BroadCastCV();
      END;
    END;
  END HitCV;

PROCEDURE BroadCastCV () =
  BEGIN
    Thread.Broadcast(cv);
    IF verbose THEN
      LOCK printmu DO
        IO.Put("{");
        PrintThreadCount(cvCount);
        IO.Put("}");
      END;
    END;
    cvCount := 0;
  END BroadCastCV;

PROCEDURE PokeCV () =
  BEGIN
    BroadCastCV();
  END PokeCV;

(*********************************)
(*  Spawning and Slot machinery  *)
(*********************************)

TYPE Cl = Thread.Closure OBJECT id: INTEGER;  OVERRIDES apply := Root; END;

PROCEDURE Spawn (slotNum: INTEGER) =
  VAR cl: Cl;
  BEGIN
    Assert(NOT slots[slotNum].occupied, "Spawn: occupied slot");
    cl := NEW(Cl);
    cl.id := slotNum;
    slots[slotNum].occupied := TRUE;
    (* slots[slotNum].thread := Thread.Fork(cl);*)
    slots[slotNum].thread := ThreadExtra.PFork(Root, cl);
    INC(numThreads);
    INC(actThreads);
  END Spawn;

PROCEDURE Die (slotNum: INTEGER) =
  BEGIN
    PokeCV();                    (* in case cv waiters were depending on
                                    this thread*)
    slots[slotNum].occupied := FALSE;
    slots[slotNum].thread := NIL;
    DEC(numThreads);
    DEC(actThreads);
  END Die;

PROCEDURE SpawnOrDie (me: INTEGER): BOOLEAN =
  VAR
    slotNum: INTEGER;
    breed  : BOOLEAN;
  BEGIN
    LOCK slotmu DO
      IF verbose THEN
        LOCK printmu DO
          PrintThreadCount(numThreads);
        END;
      END;

      LOOP
        slotNum := Random(0, MAXTHREADS - 1);
        IF slotNum # me THEN EXIT; END;
      END;

      breed := testing AND NOT slots[slotNum].occupied;
      IF breed THEN Spawn(slotNum); ELSE Die(me); END;

      IF numThreads = 0 THEN
        LOCK printmu DO
          IO.Put("[ZERO THREADS: " & Fmt.Int(numThreads) & " ]");
        END;
      END;

      IF verbose THEN
        LOCK printmu DO
          IF breed THEN IO.Put("+"); ELSE IO.Put("-"); END;
        END;
      END;
    END;
    RETURN breed;
  END SpawnOrDie;

PROCEDURE InitSlots () =
  BEGIN
    FOR slotNum := 0 TO MAXTHREADS - 1 DO
      slots[slotNum].occupied := FALSE;
      slots[slotNum].thread := NIL;
    END;
  END InitSlots;

(***********************************)
(*  Root procedure for all threads *)
(***********************************)

PROCEDURE Root (self: REFANY) : REFANY =
  VAR
    alive : BOOLEAN;
    action: INTEGER;
    me: INTEGER;
  BEGIN
    alive := TRUE;
    me := NARROW(self, Cl).id;

    LOCK slotmu DO
      Assert(slots[me].occupied AND (slots[me].thread = Thread.Self()),
             "Root: slot not mine");
    END;

    WHILE alive DO
      IF NOT verbose THEN
        LOCK printmu DO 
          PrintOpCount();
        END;
      END;

      (* check if time limit exceeded *)
      IF TimeLimit THEN
        elapsedTime := Time() - startTime;
        IF elapsedTime > TimeLimitSeconds THEN
          TimeLimit := FALSE;
          testing := FALSE;
          LOCK printmu DO
            IO.Put("\n\nOperations performed = " & Fmt.Int(tcCount) &"\n\n");
          END;
        END;
      END;

      action := Random(1, 8);
      CASE action OF
      | 1 => alive := SpawnOrDie(me);
      | 2 => HitMutex();
      | 3, 4, 5, 6, 7, 8 => HitCV();
      ELSE
      END;
      Spin(Random(1, MAX_SPIN_DELAY));
    END;

    RETURN NIL;
  END Root;

(****************)
(* Main Program *)
(****************)

PROCEDURE DoIt () =
  BEGIN
    IF numThreads # 0 THEN
      IO.PutError ("previous run not finished yet\n");
      RETURN;
    END;

    IO.Put(
      "Begin cthread test.  Max threads = " & Fmt.Int(MAXTHREADS) & "\n");
    IO.Put("  [n]  - n threads waiting for the mutex (incl holder)\n");
    IO.Put("  {n}  - n threads waiting on the condition variable\n");
    IO.Put("  n    - total number of threads\n");
    IO.Put("  +    - thread spawned\n");
    IO.Put("  -    - thread died\n");
    IO.Put("  (h:mm:ss)  - elapsed time\n");
    IO.Put("Press RETURN to terminate.\n\n");

    startTime := Time();
    testing := TRUE;
    numThreads := 0;
    actThreads := 0;
    tcCount := 0;
    mCount := 0;
    cvCount := 0;
    seed := 23311;
    InitSlots();

    LOCK slotmu DO
      Spawn(0);
    END;
    
    IF NOT TimeLimit THEN
      AwaitChar();
    ELSE
      WHILE testing DO 
        ThreadExtra.Yield();
      END;
    END;

    LOCK printmu DO
      IO.Put("\nThreat shutting down...\n");
    END;
    testing := FALSE;
    LOCK slotmu DO
      PokeCV();
    END;
  END DoIt;

PROCEDURE Zap() =
  BEGIN
    ThreatCmd.Uninstall();
    IO.Put("Threat is uninstalled\n");
  END Zap;

PROCEDURE Run (pp: ParseParams.T): BOOLEAN =
  BEGIN
    pp.reset();
    TRY
      pp.skipNext();             (* skip arg0 *)
      IF pp.testNext("zap") THEN
        Zap();
      ELSE 
        IF pp.keywordPresent("verbose") THEN
          verbose := TRUE;
        END;
        
        IF pp.keywordPresent("quiet") THEN
          verbose := FALSE;
        END;
        
        IF pp.keywordPresent("time") THEN
          TimeLimit := TRUE;
          TimeLimitSeconds := pp.getNextInt();
          IO.Put("Will quit after "&Fmt.Int(TimeLimitSeconds)&" seconds\n");
        END;

        DoIt();
      END;
      RETURN TRUE;
    EXCEPT
    | ParseParams.Error =>
        IO.Put(CommandName & CommandHelp & "\n");
       RETURN FALSE;
    END;
  END Run;

BEGIN
END Threat.

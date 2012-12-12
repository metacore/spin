(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 31-May-97  David Becker at the University of Washington
 *      Replace SAL with Kernel and MachineCPU interfaces
 *
 * 10-Feb-97  Robert Grimm (rgrimm) at the University of Washington
 *      Removed Identity
 *
 * 14-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	New interface to interrupt management.
 *
 * 07-May-96  Stefan Savage (savage) at the University of Washington
 *	Re-enabled PageFaultTest
 *
 * 04-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	Changed KThread to Thread.
 *
 * 12-Aug-95  Stefan Savage (savage) at the University of Washington
 *	Added appel VM tests
 *
 * 30-Jul-95  Stefan Savage (savage) at the University of Washington
 *	Replaced Kthread.CurrentIdentity with Identity.GetCurrent
 *	Updated Space tests    
 *
 * sometime-94  Emin Gun Sirer (egs) at the University of Washington
 *	Created.
 *
 *)
UNSAFE
MODULE Benchmark;
IMPORT CPU, Space, Translation;
IMPORT Thread, ThreadExtra, ThreadRep, ThreadPrivate;
IMPORT SalSync, UserSpaceThread;
IMPORT Strand, SpinLock, Sema, Measure;
IMPORT Ctypes; 
IMPORT Fmt, Word, ParseParams, DispTime;
IMPORT PageFaultTest, VirtAddr;
IMPORT VMError;
IMPORT IO;

VAR 
    array: ARRAY [0..8193] OF CHAR;

CONST
    NLoop = 10000;
    BigLoop = 100000;

PROCEDURE CSwitchTime(<*UNUSED*>arg: ThreadExtra.ArgT) : ThreadExtra.ResultT =
  VAR
    timer: Measure.T;
  BEGIN
    timer := (NEW(Measure.T).init("CSwitchTime",NLoop)); 
    FOR i := 1 TO NLoop DO
      timer.start(); 
      Strand.Yield();
      timer.stop();
    END;
    Measure.PrintStats(timer);
    RETURN NIL;
  END CSwitchTime;


(**********************************************************************)
(*
PROCEDURE Loop(arg: ThreadExtra.ArgT) : ThreadExtra.ResultT =
VAR b: REF INTEGER;
BEGIN
  b := NARROW(arg, REF INTEGER);
  LOOP
    IO.Put("loop(" & Fmt.Int(b^) & ")\n");
    FOR I := 1 TO 5000000 DO
    END;
  END;
END Loop;
*)

PROCEDURE StopNGo(<*UNUSED*>arg: ThreadExtra.ArgT) : ThreadExtra.ResultT =
  VAR 
    me: Thread.T;
  BEGIN
    me := Strand.GetCurrent();
    SalSync.AssertWait(666);
    SalSync.SetTimeout(me, 5);
    SalSync.BlockWithContinuation(StopNGo, NIL);
    Strand.Block(me);
    (* Not reached *)
    RETURN NIL;
  END StopNGo;

PROCEDURE SpaceCreate() =
  VAR
    timer: Measure.T;
  BEGIN
    (* warm the cache *)
    FOR i := 1 TO 3 DO
      EVAL Space.Create();
    END;
    timer := (NEW(Measure.T).init("SpaceCreate",200)); 
    (* do it *)
    FOR i := 1 TO 200 DO
      timer.start(); 
      EVAL Space.Create();
      timer.stop();
    END;
    Measure.PrintStats(timer);
  END SpaceCreate;

PROCEDURE SpaceAllocate () =
  VAR
    s    : Space.T;
    timer: Measure.T;
    address: VirtAddr.Address;
  BEGIN
    s := Space.Create();

    timer := (NEW(Measure.T).init("SpaceAllocate", 200));
    FOR i := 200 TO 399 DO
      timer.start();
      TRY
        address := 8192 * i;
	Space.Allocate(s, address, 8192);
        Translation.Write(s, SUBARRAY(array, 0, 8192), 8192 * i);
      EXCEPT
      | VMError.E(ec) =>
	IO.Put("SpaceAllocate: " & VMError.Message(ec) & ".\n");
      END;
      timer.stop();
    END;
    Measure.PrintStats(timer);

  END SpaceAllocate;


(*
 * KERNEL THREADS
 *)

CONST
  CreateIterations: INTEGER = 200;
  ForkIterations: INTEGER = 200;
  ForkJoinIterations: INTEGER = 200;
  TerminateIterations: INTEGER = 200;
  PingPongIterations: INTEGER = 200;

VAR 
  Sema1, Sema2, SyncStarted, SyncFinished: Sema.T;

PROCEDURE ThreadSync1(<*UNUSED*>arg: ThreadExtra.ArgT) : ThreadExtra.ResultT =
  BEGIN
    Sema.V(SyncStarted);
    FOR i := 1 TO PingPongIterations DO
      Sema.V(Sema2);
      Sema.P(Sema1);
    END;
    RETURN NIL;
  END ThreadSync1;

PROCEDURE ThreadSync2(<*UNUSED*>arg: ThreadExtra.ArgT) : ThreadExtra.ResultT =
  VAR
    timer: Measure.T;
  BEGIN
    timer := (NEW(Measure.T).init("PingPong",
                                  PingPongIterations)); 
    Sema.P(SyncStarted);
    FOR i := 1 TO PingPongIterations DO
      timer.start(); 
      Sema.P(Sema2);
      Sema.V(Sema1);
      timer.stop();
    END;
    Measure.PrintStats(timer);
    Sema.V(SyncFinished);
    RETURN NIL;
  END ThreadSync2;

PROCEDURE EmptyThreadBody(<*UNUSED*>arg: ThreadExtra.ArgT) : ThreadExtra.ResultT =
  BEGIN
    RETURN NIL;
  END EmptyThreadBody;

PROCEDURE Fork() =
  VAR
    timer: Measure.T;
    threads: REF ARRAY OF Thread.T;
  BEGIN
    threads := NEW(REF ARRAY OF Thread.T, ForkIterations);
    timer := NEW(Measure.T).init("Fork",ForkIterations); 
    ThreadPrivate.Preallocate(ForkIterations);
    FOR i := 0 TO ForkIterations-1 DO
      timer.start(); 
      threads[i] := ThreadExtra.PFork(EmptyThreadBody, NIL);
      timer.stop();
    END;
    Measure.PrintStats(timer);
  END Fork;

PROCEDURE Create() =
  VAR
    timer: Measure.T;
    threads: REF ARRAY OF Thread.T;
  BEGIN
    threads := NEW(REF ARRAY OF Thread.T, CreateIterations);
    timer := NEW(Measure.T).init("Create",CreateIterations); 
    ThreadPrivate.Preallocate(CreateIterations);
    FOR i := 0 TO CreateIterations-1 DO
      timer.start(); 
      threads[i] := ThreadPrivate.Create(EmptyThreadBody, NIL);
      timer.stop();
    END;
    FOR i := 0 TO CreateIterations-1 DO
      ThreadPrivate.Kill(threads[i], NIL);
    END;
    Measure.PrintStats(timer);
  END Create;

PROCEDURE Terminate() =
  VAR
    timer: Measure.T;
    th: Thread.T;
  BEGIN
    timer := NEW(Measure.T).init("Terminate",TerminateIterations); 
    FOR i := 0 TO TerminateIterations-1 DO
      th := ThreadExtra.PFork(EmptyThreadBody, NIL);
      timer.start(); 
      ThreadPrivate.Kill(th, NIL);
      timer.stop();
    END;
    Measure.PrintStats(timer);
  END Terminate;


PROCEDURE PingPong() =
  BEGIN
    SyncStarted := Sema.Alloc(0);
    SyncFinished := Sema.Alloc(0);
    Sema1 := Sema.Alloc(0);
    Sema2 := Sema.Alloc(0);

    EVAL ThreadExtra.PFork(ThreadSync1, NIL);
    EVAL ThreadExtra.PFork(ThreadSync2, NIL);
    Sema.P(SyncFinished);

  END PingPong;

PROCEDURE ForkJoin() =
  VAR
    timer: Measure.T;
  BEGIN
    timer := NEW(Measure.T).init("ForkJoin",
                                 ForkJoinIterations); 
    ThreadPrivate.Preallocate(ForkJoinIterations);
    FOR i := 0 TO ForkJoinIterations-1 DO
      timer.start(); 
      EVAL Thread.Join(ThreadExtra.PFork(EmptyThreadBody, NIL));
      timer.stop();
    END;
    Measure.PrintStats(timer);
  END ForkJoin;

PROCEDURE AllocTCB() =
  VAR
    timer: Measure.T;
    threads: REF ARRAY OF Thread.T;
    start, end: Word.T;
  BEGIN
    threads := NEW(REF ARRAY OF Thread.T, ForkIterations);
    timer := NEW(Measure.T).init("AllocTCB",ForkIterations); 
    FOR i := 0 TO ForkIterations-1 DO
      timer.start();
      start := CPU.CycleCounter();
      threads[i] := NEW(Thread.T);
      end := CPU.CycleCounter();
      timer.stop();
      IO.Put("alloc: " & Fmt.Int(Word.Minus(end, start)) & " cycles\n");
    END;
    Measure.PrintStats(timer);
  END AllocTCB; 

PROCEDURE AllocArray(size: INTEGER) =
  VAR
    timer: Measure.T;
    start, end: Word.T;
  BEGIN
    timer := NEW(Measure.T).init("AllocArray "& Fmt.Int(size),ForkIterations); 
    FOR i := 0 TO ForkIterations-1 DO
      timer.start();
      start := CPU.CycleCounter();
      EVAL NEW(REF ARRAY OF CHAR, size);
      end := CPU.CycleCounter();
      timer.stop();
      IO.Put("alloc: " & Fmt.Int(Word.Minus(end, start)) & " cycles\n");
    END;
    Measure.PrintStats(timer);
  END AllocArray;

TYPE WORD = Ctypes.int;

TYPE container = RECORD
  a: INTEGER;
  b: Strand.T;
  c: Strand.T := NIL;
  d: INTEGER := 0;
  e: INTEGER := 32; (* offset 32 *)
  f: INTEGER := 33; (* offset 40 *)
  g: INTEGER := 34; (* offset 48 *)
  h: INTEGER := 35; (* offset 56 *)
  i: WORD := 36; (* 64 *)
  j: WORD := 37; (* 68 *)
  k: WORD := 38; (* 72 *)
  l: WORD;
  m: WORD := 39; (* 80 *)
  n: WORD;
  o: WORD := 40; (* 88 *)
  p: WORD;       (* 92 *)
  q: WORD;       (* 96 *)
  r: WORD := 41; (*100 *)
  s: CHAR;       (*104 *)
  t: CHAR := 'A';(* 105 *)
  u: CHAR := 'B';
  v: CHAR := 'C';
  w: CHAR;
  x: CHAR := 'D';
END;

PROCEDURE AllocObjTest(size, num: INTEGER) =
  VAR
    timer: Measure.T;
    a: REF ARRAY OF container;
    b: REF container;
    c: container;
  BEGIN
    IF c.v # 'C' THEN
      IO.Put("NOT correctly initialized!!!\n");
    ELSE
      IO.Put("correctly initialized\n");
    END;
    (* Traced *)
    timer := (NEW(Measure.T).init("Allocate Traced Open Array",num)); 
    b := NEW(REF container);
    IO.Put("the container has an r field of " & Fmt.Int(b.r) & "\n");
    IO.Put("the container has an a field of " & Fmt.Int(b.a) & "\n");
    IF c.c = NIL THEN
      IO.Put("the container c field is nil as it should be.\n");
    ELSE 
      IO.Put("the container c field is not NIL!!! Error.\n");
    END;
    IF c.b = NIL THEN
      IO.Put("the container b field is nil as it should be.\n");
    ELSE 
      IO.Put("the container b field is not NIL!!! Error.\n");
    END;
    FOR i := 1 TO num DO
      timer.start(); 
      a := NEW(REF ARRAY OF container, size);
      timer.stop();
      IO.Put("the container has an r field of " & Fmt.Int(a[0].r) & "\n");
    END;
    Measure.PrintStats(timer);
  END AllocObjTest; 

PROCEDURE KThreadStop() =
  VAR
    timer: Measure.T;
  BEGIN
    timer := (NEW(Measure.T).init("KThreadStop",NLoop)); 
    FOR i := 1 TO NLoop DO
      timer.start(); 
      EVAL Strand.Stop(Strand.GetCurrent());
      timer.stop();
    END;
    Measure.PrintStats(timer);
  END KThreadStop;

PROCEDURE TimeSpl() =
  VAR
    timer: Measure.T;
    spl: CPU.InterruptLevel;
    begin, end: Word.T;
  BEGIN
    timer := (NEW(Measure.T).init("Spl",BigLoop)); 
    FOR i := 1 TO BigLoop DO
      (* timer.start();  *)
      begin := CPU.CycleCounter();
      (* spl := CPUPrivate.SetInterruptMask(CPUPrivate.InterruptClass.High);
         CPUPrivate.RestoreInterruptMask(spl); *)
      end := CPU.CycleCounter();
      end := CPU.CycleCounter();

      begin := Word.And(begin,16_ffffffff);
      end := Word.And(end,16_ffffffff);

      IF Word.GT(end,begin) THEN
        timer.hit(Word.Minus(end, begin));
      ELSE
        timer.hit(Word.Plus(Word.Minus(16_ffffffff,begin), end));
      END;

      (* timer.stop(); *)
    END;
    Measure.PrintStats(timer);
  END TimeSpl;

PROCEDURE TimeSpinLock() =
  VAR
    l: INTEGER := 0;
    timer: Measure.T;
  BEGIN
    timer := (NEW(Measure.T).init("Spinlock",BigLoop)); 
    FOR i := 1 TO BigLoop DO
      timer.start(); 
      SpinLock.Lock(l);
      SpinLock.Unlock(l);
      timer.stop();
    END;
    Measure.PrintStats(timer);

  END TimeSpinLock;

PROCEDURE TimeMutex() =
  VAR
      sm: MUTEX;
      timer: Measure.T;
  BEGIN
    timer := (NEW(Measure.T).init("Mutex",BigLoop)); 
    sm := NEW(MUTEX);
    FOR i := 0 TO BigLoop-1 DO
      timer.start(); 
      LOCK sm DO
      END;
      timer.stop();
    END;
    Measure.PrintStats(timer);
  END TimeMutex;

PROCEDURE TimeSema() =
  VAR
    sema: Sema.T;
    timer: Measure.T;
  BEGIN
    sema := Sema.Alloc(1);
    timer := (NEW(Measure.T).init("TimeSema",BigLoop)); 
    FOR i := 1 TO BigLoop DO
      timer.start(); 
      Sema.P(sema);
      Sema.V(sema);
      timer.stop();
    END;
    Measure.PrintStats(timer);
  END TimeSema; 
    
PROCEDURE Run (pp: ParseParams.T): BOOLEAN =
  VAR
    s    : Space.T;
    t    : UserSpaceThread.T;
    x    : UserSpaceThread.State;
    timer: Measure.T;
  BEGIN
    TRY
      pp.reset();
      pp.skipNext();

      IF pp.testNext("thread") THEN
        IF pp.testNext("u") THEN
          (*
            * User thread time
          *)
          s := Space.Create();
          x := NEW(UserSpaceThread.State);
          x.cpustate := NEW(REF CPU.GeneralRegs);
          (* warm the cache *)
          FOR i := 1 TO 3 DO EVAL UserSpaceThread.Create(s); END;
          (* do it *)
          timer := (NEW(Measure.T).init("Ucreate", 20));
          FOR i := 1 TO 20 DO
            timer.start();
            t := UserSpaceThread.Create(s);
            UserSpaceThread.SetState(t, x);
            UserSpaceThread.Resume(t);
            timer.stop();
          END;
          Measure.PrintStats(timer);
        ELSIF pp.testNext("c") THEN
          s := Space.Create();
          t := UserSpaceThread.Create(s);
          timer := (NEW(Measure.T).init("UCCreate", 20));
          FOR i := 1 TO 20 DO
            timer.start();
            (* EVAL CThread_Strands.Fork(t, 0, 0, 0, 0, 0); *)
            timer.stop();
          END;
          Measure.PrintStats(timer);
        ELSIF pp.testNext("ce") THEN
          s := Space.Create();
          t := UserSpaceThread.Create(s);
          timer := (NEW(Measure.T).init("UCECreate", 20));
          FOR i := 1 TO 20 DO
            timer.start();
            (* ct := CThread_Strands.Fork(t, 0, 0, 0, 0, 0); *)
            (* EVAL UserSpaceThread.Externalize(t, ct); *)
            timer.stop();
          END;
          Measure.PrintStats(timer);
        ELSE
          IO.Put("Unknown timing.\n");
        END;
      ELSIF pp.testNext("spacecreate") THEN
        SpaceCreate();
      ELSIF pp.testNext("spaceallocate") THEN
        SpaceAllocate();
      ELSIF pp.testNext("alloctcb") THEN
        AllocTCB();
      ELSIF pp.testNext("allocarray") THEN
        AllocArray(pp.getNextInt());
      ELSIF pp.testNext("allocobj") THEN
        AllocObjTest(pp.getNextInt(), pp.getNextInt());
      ELSIF pp.testNext("create") THEN
        Create();
      ELSIF pp.testNext("fork") THEN
        Fork();
      ELSIF pp.testNext("forkjoin") THEN
        ForkJoin();
      ELSIF pp.testNext("terminate") THEN
        Terminate();
      ELSIF pp.testNext("synch") THEN
        PingPong();
      ELSIF pp.testNext("pagefault") THEN
        PageFaultTest.FaultTest();
      ELSIF pp.testNext("spagefault") THEN
        PageFaultTest.SFaultTest();
      ELSIF pp.testNext("prot1") THEN
        PageFaultTest.Prot1();
      ELSIF pp.testNext("prot100") THEN
        PageFaultTest.Prot100();
      ELSIF pp.testNext("unprot100") THEN
        PageFaultTest.Unprot100();
      ELSIF pp.testNext("pagetrap") THEN
        PageFaultTest.TrapTest();
      ELSIF pp.testNext("pagetrapc") THEN
        PageFaultTest.TrapClosure();
      ELSIF pp.testNext("appel1") THEN
        PageFaultTest.Appel1();
      ELSIF pp.testNext("appel2") THEN
        PageFaultTest.Appel2();

      ELSIF pp.testNext("getcurrent") THEN
        timer := (NEW(Measure.T).init("GetCurrent", NLoop));
        FOR i := 1 TO NLoop DO
          timer.start();
          EVAL Strand.GetCurrent();
          timer.stop();
        END;
        Measure.PrintStats(timer);
      ELSIF pp.testNext("stop") THEN
        KThreadStop();
      ELSIF pp.testNext("cswtch") THEN
        EVAL ThreadExtra.PFork(CSwitchTime, NIL);
      ELSIF pp.testNext("spl") THEN
        TimeSpl();
      ELSIF pp.testNext("spinlock") THEN
        TimeSpinLock();
      ELSIF pp.testNext("mutex") THEN
        TimeMutex();
      ELSIF pp.testNext("sema") THEN
        TimeSema();

        (* dispatcher *)
      ELSIF pp.testNext("disp") THEN
        EVAL DispTime.Run(pp);

      ELSIF pp.testNext("all") THEN
        TimeSpl();
        TimeSpinLock();
        TimeMutex();
        TimeSema();

        KThreadStop();

        SpaceCreate();
        SpaceAllocate();
      ELSE
        RAISE ParseParams.Error;
      END;
    EXCEPT
      ParseParams.Error =>
        IO.Put("Unknown timing\n");
        IO.Put(CommandName & " " & CommandHelp & "\n");
    END;

    RETURN TRUE;
  END Run;

BEGIN
  IO.Put("Benchmarks installed...\n");
END Benchmark.

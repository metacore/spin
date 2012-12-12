(*
 * Copyright 1994, 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
 * HISTORY
 * 18-Nov-97  Yasushi Saito (yasushi) at the University of Washington
 *      Removed double CycleToMicrosec conversion
 *
 * 17-Oct-97  Tian Fung Lim (tian) at the University of Washington
 *	Added idle time printing and resetting.
 *
 * 29-Sep-97  Yasushi Saito (yasushi) at the University of Washington
 *	Added stddev, tabstop formatting.
 *
 * 15-Jul-97  Tian Fung Lim (tian) at the University of Washington
 *	Fixed elapsed time computation and added GetElapsed.
 *
 * 07-Jul-97  Yasushi Saito (yasushi) at the University of Washington
 *	Removed RecEnter and RecExit.
 *
 * 31-May-97  David Becker at the University of Washington
 *	Replace SAL with Kernel, Clock and CPU interfaces
 *
 * 10-May-97  Tian Fung Lim (tian) at the University of Washington
 *	Fixed percentage calculation.
 *
 * 14-Nov-96  Przemek Pardyak (pardy) at the University of Washington
 *	Made Spy-s untraced to be able to measure the collector.
 *
 * 14-Nov-96  Przemek Pardyak (pardy) at the University of Washington
 *	The current interface is too rigid.  Added alwaysActive and
 *	nested flags, and Move() and Reset() operations which where needed
 *	for measuring the collector. More flexibility and clean-up to come.
 *
 * 10-Oct-96  Przemek Pardyak (pardy) at the University of Washington
 *      Do not print error messages until initialized to avoid crashing
 *	if IO is not initialized yet.
 *
 * 03-May-96  David Becker (becker) at the University of Washington
 * 	Removed Limits() from init to quiet it down
 *
 * 02-Apr-96  Marc Fiuczynski (mef) at the University of Washington
 *	The Chain function produces a machine check when a NIL timer is
 *	passed in.  It now checks for NIL, but the semantics of the
 *	procedure might not be right.
 * 
 *      Increased the "MaxTimers" constant from 256 to 1024.
 *
 * 09-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	This file is initialized before the console writer, so it cannot use
 *	IO.Put.
 *
 * 27-Oct-95  Brian Bershad (bershad) at the University of Washington
 *	Fixed Up.
 *
 * 02-Mar-95  David Becker (becker) at the University of Washington
 *	Created.
 *
 *)

(*
  	Spy.m3
  
   Spin system monitor

   Note for all the people who want to hack this module: All the procedures
   in this module must be VERY FAST. So don't insert any
   codes that slow down the execution unnecessarily. -yas.

   And also don't complicate the code. I don't understand what Move means.:(

   THIS MODULE SHOULD NOT MAKE TRACED ALLOCATIONS before sampling and
   reporting Spy info.  In addition, the tabbing is broken making it
   impossible to parse the output automatically.  - tian
   
 *)

UNSAFE (* to call spl in CPUPrivate *)
MODULE Spy;	
IMPORT RTIO, Fmt;
IMPORT Text;
IMPORT Debugger;
IMPORT CPU;
IMPORT Strand, Thread, Clock;
IMPORT Word;
IMPORT Spy;
(* needed for idle time measurements *)
IMPORT ThreadPrivate, DebugOption;

CONST
  MaxTimers = 1024;
  CheckNested = FALSE;
  
VAR
  showCycles := FALSE;
  
REVEAL
  T = UNTRACED (* XXX contains a TRACED REF *) BRANDED REF RECORD
    label: TEXT;
    hits := 0; 
    time := 0; (* cycles *)
    start := 0; (* cycles *)
    min := LAST(INTEGER);
    max := FIRST(INTEGER);
    moved: T := NIL;
    nested: BOOLEAN;
    active: BOOLEAN := TRUE;
    samples: UNTRACED REF ARRAY OF INTEGER;
  END;

VAR
  bootTime: Clock.TimeVal;
  table : ARRAY [1..MaxTimers] OF T;
  current : T;
  total : T;
  firsttimer : T;
  usertimer : T;
  clock : INTEGER;
  overhead: CARDINAL;
  
PROCEDURE Create (label:TEXT; 
		  nested: BOOLEAN := FALSE;
		  nSamples: INTEGER := 0): T =
  VAR
    newt : T;
  BEGIN
    newt := NEW(T);
    newt.label := label;
    newt.nested := nested;
    IF nSamples > 0 THEN
      newt.samples := NEW(UNTRACED REF ARRAY OF INTEGER, nSamples);
    END;
    
    FOR i := FIRST(table) TO LAST(table) DO
      IF table[i] = NIL THEN
	table[i] := newt;
	RETURN newt;
      END;
    END;
    RTIO.PutText("Spy.Create: table full.\n");
    Debugger.Enter();
    RETURN NIL;
  END Create;

PROCEDURE Delete (t: T) =
  BEGIN
    FOR i := FIRST(table) TO LAST(table) DO
      IF table[i] = t THEN
	table[i] := NIL;
	RETURN;
      END;
    END;
    RTIO.PutText("Spy.Delete: timer not found");
    Debugger.Enter();
  END Delete;
  
PROCEDURE Lookup (name: TEXT): T = 
  BEGIN
    FOR i := FIRST(table) TO LAST(table) DO
      IF Text.Equal(table[i].label, name) THEN RETURN table[i] END;
    END;
    RETURN NIL;
  END Lookup;


PROCEDURE SetName (timer:T; name: TEXT) = 
  BEGIN
    IF timer # NIL THEN timer.label := name END;
  END SetName;

PROCEDURE Enter (timer:T) = 
  BEGIN
    IF timer = NIL THEN RETURN; END;
    
    IF NOT timer.active THEN RETURN; END;
    
    IF CheckNested AND timer.start # 0 THEN
      IF timer.nested THEN
        IF initDone THEN
          RTIO.PutText("Spy.Enter nested: ");
          RTIO.PutText(timer.label);
          RTIO.PutText("\n");
        END;
        timer.nested := FALSE; (* to avoid cascaded messages *)
        Debugger.Enter();
      END;
    END;

    timer.start := CPU.CycleCounter();
    IF timer.start = 0 THEN
      timer.start := 1;
    END;
  END Enter;
  
PROCEDURE Hit (timer:T; start, stop: INTEGER) =
  VAR
    delta:= CPU.CycleMinus(stop,start);
  BEGIN
    IF timer.samples # NIL AND timer.hits < NUMBER(timer.samples^) THEN
      timer.samples[timer.hits] := delta;
    END;
    INC(timer.hits);
    INC(timer.time, delta);

    IF timer.min > delta THEN timer.min := delta; END;
    IF timer.max < delta THEN timer.max := delta; END;
  END Hit;

PROCEDURE ExitMoved (timer: T; stop: INTEGER) =
  BEGIN
    IF NOT timer.active THEN RETURN; END;
    IF timer.moved # NIL THEN
      ExitMoved(timer.moved, stop);
      timer.moved := NIL;
    ELSE
      Hit(timer, timer.start, stop);
      timer.start := 0;
    END;
  END ExitMoved;
  
PROCEDURE Exit (timer:T) =
  VAR
    stop := CPU.CycleCounter();
  BEGIN
    IF timer = NIL THEN 
      RETURN;
    END;
  
    IF NOT timer.active THEN RETURN; END;
    IF timer.moved # NIL THEN
      ExitMoved(timer.moved, stop);
      timer.moved := NIL;
    ELSE
      Hit(timer, timer.start, stop);
      timer.start := 0;
    END;
  END Exit;

PROCEDURE ContextSwitch (nextTimer:T) =
  VAR
    now := CPU.CycleCounter();
  BEGIN
    IF current = nextTimer THEN RETURN END;

    Hit(total, clock, now);
    Hit(current, clock, now);

    (* update global state *)
    clock := now;
    current := nextTimer;
  END ContextSwitch;

PROCEDURE Move (thisTimer, newTimer: T) =
  BEGIN
    IF thisTimer = NIL OR newTimer = NIL THEN
      IF initDone THEN
        RTIO.PutText("Spy.Move: NIL Spy timer\n");
      END;
      RETURN;
    END;

    IF NOT newTimer.active THEN RETURN; END;

    IF newTimer.start # 0 THEN
      IF newTimer.nested THEN
        IF initDone THEN
          RTIO.PutText("Spy.Enter nested: ");
          RTIO.PutText(newTimer.label);
          RTIO.PutText("\n");
        END;
        newTimer.nested := FALSE; (* to avoid cascaded messages *)
        Debugger.Enter();
      END;
    END;

    newTimer.start := thisTimer.start;
    IF newTimer.start = 0 THEN
      newTimer.start := 1;
    END;

    thisTimer.moved := newTimer;
    thisTimer.start := 0;
  END Move;

PROCEDURE ExitInterrupt () =
  VAR
    nextTimer: T;
    s := Strand.GetCurrent();
  BEGIN
    IF TYPECODE(s) # TYPECODE(Thread.T) THEN
      nextTimer := usertimer;
    END;
    ContextSwitch(nextTimer);
  END ExitInterrupt;

PROCEDURE On () =
  BEGIN
    Reset();
  END On;

PROCEDURE Off () =
  BEGIN
  END Off;

PROCEDURE GetInfo (timer: T; VAR info: Info) =
  BEGIN
    info.label := timer.label;
    info.hits := timer.hits;
    info.total := timer.time;
    info.min := timer.min;
    info.max := timer.max;
    IF timer.samples # NIL AND timer.hits > 0 THEN
      WITH n = MIN(NUMBER(timer.samples^), timer.hits) DO
	info.samples := NEW(REF ARRAY OF INTEGER, n);
	info.samples^ := SUBARRAY(timer.samples^, 0, n);
      END;
    ELSE
      timer.samples := NIL;
    END;
  END GetInfo;

PROCEDURE Activate (timer: T; onoff: BOOLEAN) =
  BEGIN
    timer.active := onoff;
  END Activate;

PROCEDURE Clear (timer: T) =
  BEGIN
    timer.hits := 0;
    timer.time := 0;
    timer.start:= 0;
    timer.min  := LAST(INTEGER);
    timer.max  := 0;
  END Clear;

PROCEDURE FmtMicrosec (c: INTEGER): TEXT =
  VAR
    usec100 := CPU.CycleToMicrosec(c*100);
    s: TEXT;
  BEGIN
    s := Fmt.Int(usec100 DIV 100) & ".";
    WITH mod = usec100 MOD 100 DO
      IF mod > 9 THEN
	s := s & Fmt.Int(mod);
      ELSE
	s := s & "0" & Fmt.Int(mod);
      END;
    END;
    RETURN s;
  END FmtMicrosec;

(* Print the microsec number in 1/100 precision *)
PROCEDURE FmtTime (c: INTEGER): TEXT =
  BEGIN
    IF showCycles THEN
      RETURN Fmt.F("%s(%s)",
		   FmtMicrosec(c),
		   Fmt.Int(c));
    ELSE
      RETURN FmtMicrosec(c);
    END;
  END FmtTime;

(* Get the median cycle value of all the samples recorded in "t".
   Only works when "t" is created with "nSamples" > 0. *)
PROCEDURE Median (t: T): INTEGER =
  VAR
    x, tmp: INTEGER;
    n: CARDINAL;  (* # of samples collected. *)
  BEGIN
    IF t.samples = NIL THEN RETURN 0; END;
    n := MIN(t.hits, NUMBER(t.samples^)); (* # of samples collected. *)

    (* Do bubble sort on "samples". Bubble sort is the only algorithm I
       know, sorry. *)
    FOR i := 1 TO n-1 DO 
      x := t.samples[i];
      FOR j := 0 TO i-1 DO
	IF x < t.samples[j] THEN
	  (* swap i and j. *)
	  tmp := t.samples[j];
	  t.samples[j] := x;
	  t.samples[i] := tmp;
	  EXIT;
	END;
      END;
    END;
    RETURN t.samples[n DIV 2 + 1] - overhead;
  END Median;

PROCEDURE Variance (t: T): INTEGER =
  VAR
    mean, sigma, sum: CARDINAL := 0;
    n: CARDINAL;  (* # of samples collected. *)
  BEGIN
    IF t.samples = NIL THEN RETURN 0; END;
    n := MIN(t.hits, NUMBER(t.samples^));
    IF n = 0 THEN RETURN 0; END;
    
    (* First, compute the mean of samples. *)
    FOR i := 0 TO n-1 DO
      sum := sum + t.samples[i];
    END;
    mean := sum DIV n;

    FOR i := 0 TO n-1 DO 
      sigma := sigma + (t.samples[i]-mean)*(t.samples[i]-mean);
    END;
    RETURN sigma DIV n;
  END Variance;

TYPE Stat = RECORD
  percent, min, max, mean, median, stddev: CARDINAL;
END;
PROCEDURE ComputeStats (t: T; elTime: INTEGER; VAR stat: Stat) =
  BEGIN
    IF elTime # 0 THEN
      stat.percent := CPU.CycleToMicrosec(t.time) * 100 DIV elTime;
    END;

    IF t.hits # 0 THEN
      WITH mean = t.time DIV t.hits - overhead DO
	stat.mean := MAX(mean, 0);
      END;
    END;
    IF t.max # FIRST(INTEGER) THEN
      stat.max := MAX(t.max - overhead, 0);
    END;
    IF t.min # LAST(INTEGER) THEN
      stat.min := MAX(t.min - overhead, 0);
    END;
    stat.median := Median(t);
    stat.stddev := CeilingSqrt(Variance(t));
    IF t.label = NIL THEN t.label := "<no-label>"; END; (* XXX *)
  END ComputeStats;
  
PROCEDURE DumpTimeWithElapsed (entry: T; elTime: INTEGER;
			       READONLY tabstop: Tabstop) =
  VAR
    stat: Stat;
  BEGIN
    ComputeStats(entry, elTime, stat);
    RTIO.PutText(Fmt.Pad(entry.label, tabstop.name+1));
    IF stat.percent >= 0 AND stat.percent <= 100 THEN
      RTIO.PutText(Fmt.Pad(Fmt.Int(stat.percent), tabstop.percent+1));
    ELSE
      RTIO.PutText(Fmt.Pad("?" & Fmt.Int(stat.percent) & "?", tabstop.percent+1));
    END;
    RTIO.PutText(Fmt.Pad(FmtTime(stat.min), tabstop.min+1));
    RTIO.PutText(Fmt.Pad(FmtTime(stat.max), tabstop.max+1));
    RTIO.PutText(Fmt.Pad(FmtTime(stat.mean), tabstop.mean+1));
    RTIO.PutText(Fmt.Pad(FmtTime(stat.median), tabstop.median+1));
    RTIO.PutText(Fmt.Pad(FmtTime(stat.stddev), tabstop.stddev+1));
    RTIO.PutText(Fmt.Pad(Fmt.Int(entry.hits), tabstop.hits+1));
    RTIO.PutText(Fmt.Pad(FmtTime(entry.time), tabstop.total+1));
    RTIO.PutText("\n");
  END DumpTimeWithElapsed;
  

TYPE Tabstop = RECORD
  name, percent, min, max, mean, median, stddev, hits, total: CARDINAL;
END;

PROCEDURE ComputeTabStop (t: T; elTime: INTEGER; VAR tabstop: Tabstop) =
  VAR
    stat: Stat;
  BEGIN
    ComputeStats(t, elTime, stat);
    tabstop.name := MAX(tabstop.name, Text.Length(t.label));
    IF stat.percent >= 0 AND stat.percent <= 100 THEN
      tabstop.percent := MAX(tabstop.percent, 2);
    ELSE
      tabstop.percent := MAX(tabstop.percent, 2 + Text.Length(Fmt.Int(stat.percent)));
    END;
    tabstop.min := MAX(tabstop.min, Text.Length(FmtTime(stat.min)));
    tabstop.max := MAX(tabstop.max, Text.Length(FmtTime(stat.max)));
    tabstop.mean := MAX(tabstop.mean, Text.Length(FmtTime(stat.mean)));
    tabstop.median := MAX(tabstop.median, Text.Length(FmtTime(stat.median)));
    tabstop.stddev := MAX(tabstop.stddev, Text.Length(FmtTime(stat.stddev)));
    tabstop.hits := MAX(tabstop.hits, Text.Length(Fmt.Int(t.hits)));
    tabstop.total := MAX(tabstop.total, Text.Length(FmtTime(t.time)));
  END ComputeTabStop;

PROCEDURE DisplayHeader (READONLY tabstop: Tabstop) =
  BEGIN
    RTIO.PutText(Fmt.Pad("Timer", tabstop.name+1));
    RTIO.PutText(Fmt.Pad("%", tabstop.percent+1));
    RTIO.PutText(Fmt.Pad("Min", tabstop.min+1));
    RTIO.PutText(Fmt.Pad("Max", tabstop.max+1));
    RTIO.PutText(Fmt.Pad("Mean", tabstop.mean+1));
    RTIO.PutText(Fmt.Pad("Median", tabstop.median+1));
    RTIO.PutText(Fmt.Pad("Stddev", tabstop.stddev+1));
    RTIO.PutText(Fmt.Pad("Hits", tabstop.hits+1));
    RTIO.PutText("Total\n");
  END DisplayHeader;
  
PROCEDURE DumpTime (entry: T) =
  VAR
    tabstop: Tabstop;  
  BEGIN
    ComputeTabStop(entry, 0, tabstop);    
    DisplayHeader(tabstop);
    DumpTimeWithElapsed(entry, 0, tabstop);
  END DumpTime;

PROCEDURE Dump() = (* Pretty stats table *)
  VAR
    (* overall:INTEGER:= total.time + firsttimer.time;*)
    dumpTime: Clock.TimeVal;
    elTime : INTEGER;
    tabstop: Tabstop;
  BEGIN
    Clock.TimeOfDay(dumpTime);
    
    elTime := (dumpTime.tv_sec - bootTime.tv_sec) * 1000000;
    elTime := elTime + dumpTime.tv_usec - bootTime.tv_usec;

    (* This marker is required by the GC benchmarking scripts.  Do not
       move or delete it *)
    RTIO.PutText("### BEGIN OVERHEAD\n ");
    RTIO.PutText("boot: "); RTIO.PutInt(bootTime.tv_sec);
    RTIO.PutText(" "); RTIO.PutInt(bootTime.tv_usec); RTIO.PutText("\n");
    RTIO.PutText("dump: "); RTIO.PutInt(dumpTime.tv_sec);
    RTIO.PutText(" "); RTIO.PutInt(dumpTime.tv_usec); RTIO.PutText("\n");
    RTIO.PutText("elapsed : "); RTIO.PutInt(elTime); RTIO.PutText("\n");
    IF DebugOption.MeasureIdle THEN
      RTIO.PutText("idle    : "); 
      RTIO.PutInt(CPU.CycleToMicrosec(ThreadPrivate.GetIdleTime())); 
      RTIO.PutText("\n");
    END;
    IF total.time = 0 THEN
      RTIO.PutText("context timers disabled\n");
    END;


    FOR i := FIRST(table) TO LAST(table) DO
      IF table[i] # NIL AND table[i].hits # 0 THEN
	ComputeTabStop(table[i], elTime, tabstop);
      END;
    END;
    
    DisplayHeader(tabstop);
    
    FOR i := FIRST(table) TO LAST(table) DO
      IF table[i] # NIL AND table[i].hits # 0 THEN
	DumpTimeWithElapsed(table[i], elTime, tabstop);
      END;
    END;
    RTIO.PutText("Total: ");
    RTIO.PutInt(CPU.CycleToMicrosec(total.time) DIV 1000000);
    RTIO.PutText(" secs\n");
    DumpTime(total);

    (* This marker is required by the GC benchmarking scripts.  Do not
       move or delete it *)
    RTIO.PutText("### END OVERHEAD\n"); 
  END Dump;

PROCEDURE Limits () =
  BEGIN
    RTIO.PutText("\tTick rate\t"& Fmt.Int(CPU.Hertz()) &
		 " Hz\t("
		 & Fmt.Int(1000000000 DIV CPU.Hertz()) &
		 " nanoseconds per cycle)\n");
  END Limits;

VAR
  initDone := FALSE;

PROCEDURE Reset () = (* resets all stats to zero *)
  BEGIN
    FOR i := FIRST(table) TO LAST(table) DO
      IF table[i] # NIL THEN Clear(table[i]); END;
    END;
    total.time := 0;
    firsttimer.time := 0;
    usertimer.time := 0;
    clock := CPU.CycleCounter();
    Clock.TimeOfDay(bootTime);
    current := firsttimer;
    initDone := TRUE;
    ThreadPrivate.SetIdleTime(0);
  END Reset; 

(* return number of microseconds since last Spy.Reset *)
PROCEDURE GetElapsed () :INTEGER=
  VAR
    dumpTime: Clock.TimeVal;
    elTime : INTEGER;
  BEGIN
    Clock.TimeOfDay(dumpTime);

    (* TimeOfDay is only updated at clockticks, so if spl is high,
       this routine will not reflect actual elapsed time if a tick
       hasn't yet occured *)
    elTime := (dumpTime.tv_sec - bootTime.tv_sec) * 1000000;
    elTime := elTime + dumpTime.tv_usec - bootTime.tv_usec;
    RETURN elTime;
  END GetElapsed;    


PROCEDURE FloorSqrt(x: INTEGER): INTEGER =
  VAR
    root := 0;
  BEGIN
    IF x < 0 THEN
      RTIO.PutText("Spy.FloorSqrt: x < 0");
      Debugger.Enter();
    END;

    (* x is between 0 and 2^(BITSIZE(INTEGER) - 1) - 1, i.e. 2^63 - 1 *)
    (* so its square root is between 0 and 2^(BITSIZE(INTEGER)/2) - 1, 
       i.e. 2^32 - 1 *)

    FOR bit := 31 TO 0 BY -1 DO
      WITH larger = Word.Or(root, Word.Shift(1, bit)) DO
	WITH LargeSq = larger * larger DO
	  IF LargeSq > 0 AND
	    LargeSq < x THEN
	    root := larger;
	  END;
	END;
      END;
    END;
    RETURN root;
  END FloorSqrt;

PROCEDURE CeilingSqrt (x: INTEGER): INTEGER =
  BEGIN
    RETURN FloorSqrt(x) + 1;
  END CeilingSqrt;

PROCEDURE Init () =
  VAR spy: T;
  BEGIN
    IF NOT initDone THEN
      initDone := TRUE;
      total := NEW(T);
      firsttimer := NEW(T);
      firsttimer.label := "First timed context";
      usertimer := Create("All user code");

      (* Measure the null spy time and calibrate other spys. *)
      spy := Create("<temp>", FALSE, 400);
      FOR i := 0 TO 399 DO
	(* Make qualified proc calls to measure overhead for
	   calls from an outside module *)
	Spy.Enter(spy);
	Spy.Exit(spy);
      END;
      overhead := Median(spy);
      Delete(spy);
      
      Clock.TimeOfDay(bootTime);
    END;
  END Init;
  
BEGIN
  (* mainbody moved to Init() which is called directly from RTLinker *)
END Spy. 

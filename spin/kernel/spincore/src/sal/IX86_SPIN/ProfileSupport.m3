(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 29-Jul-96  Charles Garrett (garrett) at the University of Washington
 *	Changed to accomodate cycle counting profiling.
 *
 * 29-May-96  Charles Garrett (garrett) at the University of Washington
 *	Added the ability to accumulate profile data over multiple calls
 *	 to On() and Off(), with a new Flush() command to wipe out
 *	 existing profile data. Tried to minimize the amount of time
 *	 spent in this module when profiling is actually turned on.
 *
 * 20-May-96  Przemek Pardyak (pardy) at the University of Washington
 *    Changed the dispatcher interface.
 *
 * 05-May-96  Charles Garrett (garrett) at the University of Washington
 *	Extended to handle both PC sampling and full call graph
 *	 profiling. If the kernel is compiled with a
 *	 machine/alpha/Profile.s file that performs call graph profiling,
 *	 then you will get call graph information, otherwise you can only
 *	 get PC samples.
 *
 * 17-Apr-96  Charles Garrett (garrett) at the University of Washington
 *	A PC sampling profiler. Type "profile on", then run some tests
 *	and type "profile off". The profile information will reside in
 *	memory until the next time you type "profile on". While the 
 *	information is in memory, you can attach with m3gdbttd and 
 *	display it.
 *
 *)
UNSAFE MODULE ProfileSupport EXPORTS ProfileSupport, ProfileExtern;
IMPORT Dispatcher, DispatcherPrivate, ClockPrivate, CPU, IO, Fmt;
IMPORT SortedIntIntTbl, Word;

(* This module implements two kinds of profiling. First, when the 
   regular ALPHA_SPIN target is built, you get a form of PC sampling
   profiling which installs a handler on the ClockTick event and 
   records the PC at which the interrupt occured. This is of some use,
   but it is not very accurate and it ignores all of the system activity
   that occurs when the clock interrupts are masked.

   On the other hand, if you compile the ALPHA_SPIN_PROF target, you 
   get full call graph profiling with both frequency and cycle count
   information. This requires quite a bit of coordination to pull of
   which is where most of the code in the module comes in. The profiling
   is done by two low level routines (_mcount and _mcount_epilogue),
   which are automatically called at the entry and exit of every 
   procedure. No information is collected until the StartGprof procedure
   is called. This procedure allocates buffers for the mcount routines to
   use and set the flag to TRUE. When StopGprof is called, the flag is
   set to false and the profile data is read out of the buffer where mcount
   wrote it and stored in a simpler form in a new buffer where m3gdbttd can
   read it. In order to read the profile information and resolve the 
   addresses within it, the user connects with m3gdbttd which slurps the
   data out of that new buffer.
*)


TYPE
  PCcount = RECORD
    pc: Word.T := 0;
    count: CARDINAL := 0;
  END;

  (* Keep this record synchronized with the structure in gprof.c *)
  ArcInfo = RECORD
    link   : Word.T := 0;
    count  : Word.T := 0;
    time   : Word.T := 0;
    selfPC : Word.T := 0;
  END;

  (* Keep this record synchronized with the structure in gprof.c *)
  CallerPC = RECORD
    proc   : Word.T := 0;
    next   : Word.T := 0;
    pc     : Word.T := 0;
  END;


  GdbArcInfo = RECORD
    caller : Word.T;
    proc   : Word.T;
    count  : Word.T;
    time   : Word.T;
  END;

CONST
  Debug = TRUE;

TYPE
  ArcRange = [1000..65536];
  CallerRange = [1000..65536];

VAR
  NumberOfArcs : ArcRange := 10000;
  NumberOfCallers : CallerRange := 10000;

  binding : Dispatcher.Binding := NIL;
  sampleTbl : SortedIntIntTbl.T := NIL;

  (* The forGdb array is in untraced memory so that you can tell
     gdb where it is and it won't move. *)
  forGdb : UNTRACED REF ARRAY OF PCcount := NIL;
  gdbGraphProfile : UNTRACED REF ARRAY OF GdbArcInfo := NIL;

  callerArray : UNTRACED REF ARRAY OF CallerPC := NIL;
  procArray : UNTRACED REF ARRAY OF ArcInfo := NIL;

  stubRoutines : UNTRACED REF ARRAY OF DispatcherPrivate.StubEvent;

PROCEDURE Sampler(VAR ss: CPU.SavedState) =
  VAR
    count : INTEGER;
    pc : Word.T;
  BEGIN
    (* IF ss.pc = ClockPrivate.pcForDelayedTicks THEN
       pc := Word.Or(ss.ra, 1);
       ELSE *)
    pc := ss.pc;
    (* END; *)
    IF sampleTbl.get(pc, count) THEN
      EVAL sampleTbl.put(pc, count + 1);
    ELSE
      EVAL sampleTbl.put(pc, 1);
    END;
  END Sampler;


PROCEDURE Flush() =
  BEGIN
    (* Clean up any memory we used before *)
    IF callerArray # NIL THEN DISPOSE(callerArray); END;
    IF procArray # NIL THEN DISPOSE(procArray); END;
  END Flush;

PROCEDURE SetNumArcs(n: CARDINAL) =
  VAR
    newArcs : UNTRACED REF ARRAY OF ArcInfo := NIL;
  BEGIN
    IF n < FIRST(ArcRange) THEN
      IO.Put("The minimum number of profiling arcs is " & 
        Fmt.Int(FIRST(ArcRange)) & "\n");
    ELSIF n > LAST(ArcRange) THEN
      IO.Put("The maximum number of profiling arcs is " & 
        Fmt.Int(LAST(ArcRange)) & "\n");
    ELSE
      IF n < NumberOfArcs THEN
        Flush();
        NumberOfArcs := n;
      ELSIF n > NumberOfArcs THEN
        IF procArray # NIL THEN
          newArcs := NEW(UNTRACED REF ARRAY OF ArcInfo, n);
          SUBARRAY(newArcs^, 0, NumberOfArcs) := procArray^;
          DISPOSE(procArray);
          procArray := newArcs;
        END;
        NumberOfArcs := n;
      END;
      arcsLen := n;
    END;
  END SetNumArcs;

PROCEDURE SetNumCallers(n: CARDINAL) =
  VAR
    newCallers : UNTRACED REF ARRAY OF CallerPC := NIL;
  BEGIN
    IF n < FIRST(CallerRange) THEN
      IO.Put("The minimum number of profiling callers is " & 
        Fmt.Int(FIRST(CallerRange)) & "\n");
    ELSIF n > LAST(CallerRange) THEN
      IO.Put("The maximum number of profiling callers is " & 
        Fmt.Int(LAST(CallerRange)) & "\n");
    ELSE
      IF n < NumberOfCallers THEN
        Flush();
        NumberOfCallers := n;
      ELSIF n > NumberOfCallers THEN
        IF callerArray # NIL THEN
          newCallers := NEW(UNTRACED REF ARRAY OF CallerPC, n);
          SUBARRAY(newCallers^, 0, NumberOfCallers) := callerArray^;
          DISPOSE(callerArray);
          callerArray := newCallers;
        END;
        NumberOfCallers := n;
      END;
      callerLen := n;
    END;
  END SetNumCallers;

PROCEDURE StartGprof(keep: BOOLEAN := FALSE): BOOLEAN =
  VAR
    event : PROCANY;
    sampler : PROCANY;
  BEGIN
    IF flag = Flag.ProfOn THEN
      RETURN FALSE;
    END;
    LOCK mu DO
      (* When not compiled with full profiling support,
         don't allocate the arrays of arc count info. *)
      IF NOT callGraph THEN 
        (* Install clock tick handler *)
        IF sampleTbl = NIL OR NOT keep THEN
          sampleTbl := NEW(SortedIntIntTbl.Default).init();
        END;
        event := ClockPrivate.ClockTick;
        sampler := Sampler;
        TRY
          binding := Dispatcher.InstallHandler(event, NIL, sampler);
        EXCEPT
        | Dispatcher.Error => 
          IO.Put("Profile command failed\n");
          RETURN FALSE;
        END;

        flag := Flag.ProfOn;
        IO.Put("PC sampling enabled\n");
        RETURN TRUE; 
      END;

      IF NOT keep THEN
        Flush();
      END;

      callerLen := NumberOfCallers;
      arcsLen := NumberOfArcs;

      (* callerArray could be NIL before any profiling has happened *)
      IF NOT keep OR callerArray = NIL THEN
        callerArray := NEW(UNTRACED REF ARRAY OF CallerPC, callerLen);
        procArray := NEW(UNTRACED REF ARRAY OF ArcInfo, arcsLen);
      END;

      IF Debug THEN
        IO.Put("CallerPC table size = " & Fmt.Int(callerLen) & " x " &
          Fmt.Int(BYTESIZE(callerArray[0])) & "\n");
        IO.Put("Arcs table size = " & Fmt.Int(arcsLen) & " x " &
          Fmt.Int(BYTESIZE(procArray[0])) & "\n");
        IO.Put("Call graph profiling enabled\n");
      END;

      (* Initialize the pointers in the C code. *)
      caller := LOOPHOLE(ADR(callerArray[0]), Word.T);
      arcs := LOOPHOLE(ADR(procArray[0]), Word.T);

      (* Be sure that the flag is not reset until the end *) 
      flag := Flag.ProfOn;
    END;
    RETURN TRUE;
  END StartGprof;


PROCEDURE StopGprof(): BOOLEAN =
  BEGIN
    IF flag = Flag.ProfOff THEN
      RETURN FALSE;
    END;
    LOCK mu DO
      IF NOT callGraph THEN
        TRY
          Dispatcher.Uninstall(binding);
        EXCEPT
        | Dispatcher.Error => 
        END;
        binding := NIL;
      END;

      (* Be sure that the flag is set at the beginning *)
      IF flag = Flag.ProfOverflow1 THEN
        IO.Put("WARNING: Ran out of space for new callers\n");
      ELSIF flag = Flag.ProfOverflow2 THEN
        IO.Put("WARNING: Ran out of space for new arcs\n");
      END;
      flag := Flag.ProfOff; 
      IF Debug THEN
        IO.Put("Profiling stopped\n");
      END;
    END;
    IF callGraph THEN
      StoreArcs();
    ELSE
      StoreSamples();
    END;
    RETURN TRUE;
  END StopGprof;



(* Sort the entries by decreasing frequency count.
   Move the sample data from the table to a flat array
   which can be displayed by gdb. *)
PROCEDURE StoreSamples() =
  VAR
    pc : Word.T;
    count : INTEGER;
    iter := sampleTbl.iterateOrdered();
  BEGIN
    LOCK mu DO
      IF forGdb # NIL THEN
        DISPOSE(forGdb);
      END;
      forGdb := NEW(UNTRACED REF ARRAY OF PCcount, sampleTbl.size());
      
      numSamples := 0;
      WHILE iter.next(pc, count) DO
        forGdb[numSamples] := PCcount {pc, count};
        INC(numSamples);
      END;
      
      flatProfile := LOOPHOLE(ADR(forGdb[0]), Word.T);

      (* Find out what the current stub routines are *)
      IF stubRoutines # NIL THEN DISPOSE(stubRoutines); END;
      stubRoutines := DispatcherPrivate.RecordStubs();
      numStubs := NUMBER(stubRoutines^);
      stubInfo := LOOPHOLE(ADR(stubRoutines[0]), Word.T);
    END;
  END StoreSamples;

PROCEDURE StoreArcs() =
  VAR
    arcIndex : CARDINAL;
  BEGIN
    LOCK mu DO
      numArcs := 0;
      IF callerArray # NIL AND procArray # NIL THEN 
        (* The total number of distinct arcs is stored in the link
           field of the first entry in the procArray. *)
        numArcs := procArray[0].link;
        (* We decipher the call-arc data structures in SPIN. gdb
           will just be responsible from reading out of the flat 
           array that we fill in. *)
        IF gdbGraphProfile # NIL THEN
          DISPOSE(gdbGraphProfile);
        END;
        gdbGraphProfile := NEW(UNTRACED REF ARRAY OF GdbArcInfo, numArcs);
        arcIndex := 0;
      
        (* IF Debug THEN
          IO.Put("Caller Array\n");
          FOR i := 0 TO LAST(callerArray^) DO
            WITH proc = callerArray[i] DO
              IF proc.pc = 0 AND proc.next = 0 THEN
                EXIT;
              END;
              IO.Put(Fmt.Int(i) & ": " & Fmt.Unsigned(proc.pc) & " ");
              IO.Put(Fmt.Int(proc.proc) & " ");
              IO.Put(Fmt.Int(proc.next) & "\n");
            END;
          END;

          IO.Put("Arcs Array\n");
          FOR i := 0 TO LAST(procArray^) DO
            WITH proc = procArray[i] DO
              IF proc.selfPC = 0 AND proc.link = 0 THEN
                EXIT;
              END;
              IO.Put(Fmt.Int(i) & ": " & Fmt.Unsigned(proc.selfPC) & " ");
              IO.Put(Fmt.Int(proc.count) & " ");
              IO.Put(Fmt.Int(proc.time) & " ");
              IO.Put(Fmt.Int(proc.link) & "\n");
            END;
          END;
        END; *)

        (* Walk over the array of CallerPC records and for each one
           walk over its list of callees. The first entry in the 
           callerArray is a dummy. *)
        FOR i := 1 TO LAST(callerArray^) DO
          WITH pc = callerArray[i].pc DO
            IF pc = 0 THEN
              EXIT;
            END;

            VAR
              proc := callerArray[i].proc;
            BEGIN
              WHILE proc # 0 DO
                WITH entry = procArray[proc] DO
                  gdbGraphProfile[arcIndex] := GdbArcInfo {caller := pc,
                                                           proc := entry.selfPC,
                                                           count := entry.count,
                                                           time := entry.time};
                  proc := entry.link;
                  INC(arcIndex);
                END;
              END;
            END;
          END;
        END;

        IF numArcs > 0 THEN
          graphProfile := LOOPHOLE(ADR(gdbGraphProfile[0]), Word.T);
        ELSE
          graphProfile := 0;
        END;
      END;

      (* Find out what the current stub routines are *)
      IF stubRoutines # NIL THEN DISPOSE(stubRoutines); END;
      stubRoutines := DispatcherPrivate.RecordStubs();
      numStubs := NUMBER(stubRoutines^);
      stubInfo := LOOPHOLE(ADR(stubRoutines[0]), Word.T);
    END;
    IF Debug THEN
      IO.Put("Gdb storage = " & Fmt.Int(numArcs) & " x " &
        Fmt.Int(BYTESIZE(GdbArcInfo)) & "\n");
      IO.Put("Spin profile count = " & Fmt.Int(count) & "\n");
    END;
  END StoreArcs;

PROCEDURE PrintSamples() =
  VAR
    info : GdbArcInfo;
  BEGIN
    LOCK mu DO
      IF NOT callGraph THEN
        IO.Put("PC\t\t\tFrequency\n");
        IF forGdb # NIL THEN
          FOR i := FIRST(forGdb^) TO LAST(forGdb^) DO
            IO.Put(Fmt.Unsigned(forGdb[i].pc) & "\t" & 
              Fmt.Int(forGdb[i].count) & "\n");
          END;
        END;
      ELSE
        IO.Put("Call graph stuff\n");
        FOR i := FIRST(gdbGraphProfile^) TO LAST(gdbGraphProfile^) DO
          info := gdbGraphProfile[i];
          IO.Put("CallerPC = " & 
            Fmt.Unsigned(info.caller) & 
            "\tCalleePC = " & 
            Fmt.Unsigned(info.proc) &
            "\tCount = " & Fmt.Unsigned(info.count) & 
            "\tCycles = " & Fmt.Unsigned(info.time) & "\n");
        END;
      END;
    END;
  END PrintSamples;

  (* Exported procedures *)
PROCEDURE On(keep: BOOLEAN): BOOLEAN =
  BEGIN
    RETURN StartGprof(keep);
  END On;

PROCEDURE Off(): BOOLEAN =
  BEGIN
    RETURN StopGprof();
  END Off;

PROCEDURE Enabled(): BOOLEAN =
  BEGIN
    RETURN flag = Flag.ProfOn;
  END Enabled;

VAR
  mu: MUTEX;
BEGIN
  mu := NEW(MUTEX);
  clockRate := CPU.Hertz();
END ProfileSupport.

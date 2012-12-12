(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 4-oct-96  becker at the University of Washington
 *	Added /proc files
 *
 * 08-Jul-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added printing of events.
 *
 * 15-Apr-95  Przemek Pardyak (pardy) at the University of Washington
 *	Created
 *
 *)

UNSAFE (* to display type info *)
MODULE Disp;
IMPORT ParseParams, IO, DispatcherPrivate, Dispatcher, Fmt;
IMPORT RTProcedureSRC, Text, SafeConvert, Binding;
IMPORT InfoFile, Wr, ThreadExtra, Error;
IMPORT RTLinker, RT0, RTType, UnsafeConvert;
IMPORT Log, DomainPrivate;

<* FATAL Dispatcher.Error *>
<* FATAL ParseParams.Error *>

PROCEDURE Usage () =
  BEGIN
    IO.Put("Disp usage:\n");
    IO.Put("\tdisp opt     - fully optimized dispatch routines\n");
    IO.Put("\tdisp debug   - unoptimized debug dispatch routines\n");
    IO.Put("\tdisp set <n> - set optimization level\n");
    IO.Put("\tdisp show    - print current optimization level\n");
    IO.Put("\tdisp event all            - list all events\n");
    IO.Put("\tdisp event active         - list all overridden events\n");
    IO.Put("\tdisp event <unit>.<event> - print info about event\n");
    IO.Put("\tdisp event <unit>.<event> <n> - set optimization level for an event\n");
  END Usage;

PROCEDURE PrintProc (proc: PROCANY) =
  VAR
    desc: REF RTProcedureSRC.ProcDesc;
  BEGIN
    IF proc = NIL THEN
      IO.Put("NIL");
    ELSE
      IO.Put("0x" & Fmt.Unsigned(SafeConvert.AdrToWord(proc), 16) & " ");
      desc    := RTProcedureSRC.GetByPC(proc);
      IF desc # NIL AND desc.proc = proc THEN 
        IO.Put(desc.file & "." & desc.name);
      ELSE
        IO.Put("<UNKNOWN>");
      END;
    END;
  END PrintProc;

PROCEDURE PrintEvent (VAR event: RTProcedureSRC.ProcDesc; 
                      n: INTEGER; short: BOOLEAN := FALSE) =
  VAR
    handlers: REF ARRAY OF Dispatcher.Binding;
    guard: Dispatcher.ImposedGuard;
  BEGIN
    IF NOT short THEN
      IO.Put(" " & Fmt.Int(n) & " event   : ");
    END;
    PrintProc(event.proc);
    IO.Put("; opt: " & Fmt.Int(DispatcherPrivate.GetOptLevel(event.proc)));
    IO.Put("; domain: " & DomainName(event.proc) & "\n");
    IF short THEN RETURN; END;
    TRY
      handlers := Dispatcher.GetHandlers(event.proc, NIL);
    EXCEPT
    ELSE
      IO.Put("could not get handlers\n");
    END;
    IF handlers = NIL THEN
      IO.Put("\tonly the original handler installed\n"); 
    ELSE
      FOR i := FIRST(handlers^) TO LAST(handlers^) DO
        IO.Put("    " & Fmt.Int(i+1) & "\thandler : ");
        PrintProc(handlers[i].handler);
        IF handlers[i].useHandlerClosure THEN
          IO.Put(" (cl: " & 
            Fmt.Unsigned(
                SafeConvert.RefAnyToWord(handlers[i].handlerClosure)) & 
            ")");
        END;
        IO.Put("\n\tguard   : "); 
        PrintProc(handlers[i].guard); 
        IF handlers[i].useGuardClosure THEN
          IO.Put(" (cl: " & 
            Fmt.Unsigned(
                SafeConvert.RefAnyToWord(handlers[i].guardClosure)) & 
            ")");
        END;
        IO.Put("\n");
        guard := handlers[i].imposedGuards;
        IF guard # NIL THEN
          IO.Put("\timposed guards  : ");
          WHILE guard # NIL DO
            IO.Put("\t\t");
            PrintProc(guard.guard);
            guard := guard.next;
            IO.Put("\n");
          END;
        END;
      END;
    END;
  END PrintEvent;

PROCEDURE AllEvents () =
  VAR
    list: REF ARRAY OF RTProcedureSRC.ProcDesc;
  BEGIN
    list := RTProcedureSRC.GetByName();
    FOR i := FIRST(list^) TO LAST(list^) DO
      IO.Put(Fmt.Int(i) & " : ");
      PrintProc(list[i].proc);
      IO.Put("; domain: " & DomainName(list[i].proc) & "\n");
    END;
  END AllEvents;

PROCEDURE ActiveEvents (short: BOOLEAN) =
  VAR
    list: REF ARRAY OF RTProcedureSRC.ProcDesc;
    handlers: REF ARRAY OF Dispatcher.Binding;
    n: INTEGER;
  BEGIN
    list := RTProcedureSRC.GetByName();
    n := 0;
    FOR i := FIRST(list^) TO LAST(list^) DO
      TRY
        handlers := Dispatcher.GetHandlers(list[i].proc, NIL);
      EXCEPT
      ELSE
        IO.Put("could not get handlers\n");
      END;
      IF handlers # NIL THEN
        INC(n);
        PrintEvent(list[i], n, short);
        IF NOT short THEN
          IO.Put("\n");
        END;
      END;
    END;
  END ActiveEvents;

PROCEDURE DomainName (proc: PROCANY): TEXT =
  VAR
    name: TEXT;
    trusted, dynamic: BOOLEAN;
  BEGIN
    TRY
      DomainPrivate.GetState(DomainPrivate.AddressToDomain(proc),
                             name, trusted, dynamic);
    EXCEPT
    | DomainPrivate.NonDomainAddress => 
      name := "NO-DOMAIN";
    END;
    RETURN name;
  END DomainName;

(* find the event name *)
PROCEDURE EventName (arg:TEXT; VAR unit, name: TEXT) : BOOLEAN =
  BEGIN
    (* "dot" cannot be the first or last character *)
    unit := NIL;
    FOR i := 1 TO Text.Length(arg) - 2 DO
      IF Text.GetChar(arg, i) = '.' THEN
        unit := Text.Sub(arg, 0, i);
        name := Text.Sub(arg, i+1, Text.Length(arg) - i - 1);
      END;
    END;
    IF unit = NIL THEN
      IO.Put("event name format: <module-name>.<procedure-name>\n");
      RETURN FALSE;
    END;
    RETURN TRUE;
  END EventName;
    
PROCEDURE AnEvent (arg:TEXT; pp: ParseParams.T) : BOOLEAN =
  VAR
    unit, name: TEXT;
    list: REF ARRAY OF RTProcedureSRC.ProcDesc;
    setOne: BOOLEAN := FALSE;
    optLevel: INTEGER;
  BEGIN
    (* get the name of the event *)
    IF NOT EventName(arg, unit, name) THEN RETURN FALSE; END;

    (* see whether we are setting the opt level for the event *)
    TRY
      optLevel := pp.getNextInt();
      setOne := TRUE;
    EXCEPT
    | ParseParams.Error =>
    END;
    IF setOne THEN
      IF optLevel < 0 OR optLevel > DispatcherPrivate.MaxOptLevel THEN
        IO.Put("specify optimization level in the range: 0 - " &
          Fmt.Int(DispatcherPrivate.MaxOptLevel) & "\n");
        RETURN FALSE;
      END;
    END;
    
    (* print or set what we found *)
    list := RTProcedureSRC.GetByName(unit, name);
    IF list = NIL THEN
      IO.Put("event not found\n");
    ELSE
      FOR i := FIRST(list^) TO LAST(list^) DO
        IF setOne THEN
          DispatcherPrivate.SetOptLevel(list[i].proc, optLevel);
        ELSE
          PrintEvent(list[i], i+1);
        END;
      END;
    END;
    RETURN TRUE;
  END AnEvent;

PROCEDURE FindOn(pp: ParseParams.T): BOOLEAN RAISES { ParseParams.Error } =
  BEGIN
    IF pp.keywordPresent("on") THEN
      RETURN TRUE;
    ELSIF pp.keywordPresent("off") THEN
      RETURN FALSE;
    ELSE
      RAISE ParseParams.Error;
    END;
  END FindOn;

PROCEDURE Trace (pp: ParseParams.T) : BOOLEAN =
  VAR
    list: REF ARRAY OF RTProcedureSRC.ProcDesc;
    unit, name: TEXT;
    all: BOOLEAN := FALSE;
    on : BOOLEAN;
    arg: TEXT;
  BEGIN
    arg := pp.getNext();
    IO.Put("[" & arg & "]\n");
    IF Text.Equal(arg, "dump") THEN
      IO.Put("dump\n");
      IO.Put("Dump tracing information\n");
      DispatcherPrivate.DumpTrace();
      RETURN TRUE;
    ELSIF Text.Equal(arg, "reset") THEN
      IO.Put("Reseting tracing information\n");
      DispatcherPrivate.ResetTrace();
      RETURN TRUE;
    ELSIF Text.Equal(arg, "all") THEN
      IO.Put("all\n");
      all := TRUE;
    ELSE
      IO.Put("one\n");
      IF NOT EventName(arg, unit, name) THEN RETURN FALSE; END;
    END;
    on := FindOn (pp);
    IF all THEN
      IO.Put("Tracing all events");
      IF on THEN IO.Put(" on\n"); ELSE IO.Put(" off\n"); END;
      DispatcherPrivate.TraceAll(on);
    ELSE
      list := RTProcedureSRC.GetByName(unit, name);
      IF list = NIL THEN
        IO.Put("event not found\n");
        RETURN FALSE;
      ELSE
        IF NUMBER(list^) # 1 THEN
          IO.Put("More than one event found\n");
          FOR i := FIRST(list^) TO LAST(list^) DO
            PrintEvent(list[i], i+1);
          END;
          RETURN FALSE;
        ELSE
          IO.Put("Tracing " & unit & "." & name);
          IF on THEN IO.Put(" on\n"); ELSE IO.Put(" off\n"); END;
          DispatcherPrivate.Trace(list[0].proc, on);
        END;
      END;
    END;    
    RETURN TRUE;
  END Trace;

PROCEDURE Run(pp: ParseParams.T): BOOLEAN =
  VAR
    optLevel: INTEGER;
    arg: TEXT;
  BEGIN
    pp.reset();
    TRY
      pp.skipNext();
      IF pp.keywordPresent("opt") THEN
        DispatcherPrivate.SetDefaultOptLevel(DispatcherPrivate.MaxOptLevel);

      ELSIF pp.keywordPresent("log") THEN
        IO.Put("\n-- log ------------\n");
        Log.Dumplog();
        IO.Put("\n-- end ------------\n");

      ELSIF pp.keywordPresent("debug") THEN
        DispatcherPrivate.SetDefaultOptLevel(0);
        (*
      ELSIF pp.keywordPresent("trace") THEN
        (*        DispatcherPrivate.SetTrace();*)
        *)
      ELSIF pp.keywordPresent("set") THEN
        optLevel := pp.getNextInt();
        IF optLevel < 0 OR optLevel > DispatcherPrivate.MaxOptLevel THEN
          IO.Put("specify optimization level in the range: 0 - " &
            Fmt.Int(DispatcherPrivate.MaxOptLevel) & "\n");
          RETURN FALSE;
        END;
        DispatcherPrivate.SetDefaultOptLevel(optLevel);
      ELSIF pp.keywordPresent("show") THEN
        IO.Put("current default level of optimization: " &
          Fmt.Int(DispatcherPrivate.DefaultOptLevel) & "\n");
      ELSIF pp.keywordPresent("event") THEN
        arg := pp.getNext();
        IF Text.Equal(arg, "all") THEN
          AllEvents();
        ELSIF Text.Equal(arg, "active") THEN
          ActiveEvents(pp.keywordPresent("short"));
        ELSE
          IF NOT AnEvent(arg,pp) THEN RETURN FALSE END;
	END;
      ELSIF pp.keywordPresent("trace") THEN
        IF NOT Trace(pp) THEN
          Usage();
          RETURN FALSE;
        END;
      ELSE
        Usage();
        RETURN FALSE;
      END;
    EXCEPT
    | ParseParams.Error => 
      Usage();
      RETURN FALSE;
    | Dispatcher.Error => 
      IO.PutError("dispatcher exception\n");
      RETURN FALSE;
    END;
    RETURN TRUE;
  END Run;

PROCEDURE Events (wr: Wr.T) =
  VAR
    realWr := ThreadExtra.SetWrSelf(wr);
  BEGIN
    AllEvents();
    EVAL ThreadExtra.SetWrSelf(realWr);
  END Events;

PROCEDURE Types (wr: Wr.T) =
  VAR t: RT0.TypeDefn;
  BEGIN
    Wr.PutText(wr,"Here are the types: nTypes = " &
      Fmt.Int  (RTLinker.type_info.nTypes) & "\n");
    FOR i := 0 TO RTLinker.type_info.nTypes-1 DO
      t := RTType.Get (i);
      Wr.PutText(wr, " typecode= 0x" & Fmt.Unsigned(t.typecode) &
        " subtypes= [ " & Fmt.Int(t.subTypeCode) &
        " .. " & Fmt.Int(t.lastSubTypeCode) & " ] " &
        UnsafeConvert.StoT(LOOPHOLE(t.name,UNTRACED REF [-128..127])) & "\n");
    END;
  END Types;

PROCEDURE Handlers (wr: Wr.T) =
  VAR
    realWr := ThreadExtra.SetWrSelf(wr);
  BEGIN
    ActiveEvents(FALSE);
    EVAL ThreadExtra.SetWrSelf(realWr);
  END Handlers;

PROCEDURE DispOpt (wr: Wr.T) =
  BEGIN
    Wr.PutText(wr,"current default level of optimization: " &
          Fmt.Int(DispatcherPrivate.DefaultOptLevel) & "\n");
  END DispOpt;

BEGIN
  TRY
    InfoFile.Create("/proc/types",Types);
    InfoFile.Create("/proc/events",Events);
    InfoFile.Create("/proc/handlers",Handlers);
    InfoFile.Create("/proc/disp_opt",DispOpt);
  EXCEPT
  | Error.E(e) =>
    IO.PutError("disp procfs files:" & e.message() & "\n");
  END;
END Disp. 

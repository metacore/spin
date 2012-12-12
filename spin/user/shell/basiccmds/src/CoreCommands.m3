(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 31-May-97  David Becker at the University of Washington
 *      Replace SAL with Kernel and CPU interfaces
 *
 *)

(* Core commands *)
MODULE CoreCommands;

IMPORT Clock, Debugger, Fmt, IO, Log, ParseParams, RTOS, Sal,
       SchedPrivate, Shell, Strand, Commands;

IMPORT InfoFile, Wr, Error;

PROCEDURE PREEMPTIVE (
    <*UNUSED*> closure : REFANY;
    pp      : ParseParams.T): BOOLEAN =
  CONST preemptive = ARRAY BOOLEAN OF TEXT {"disabled","enabled"};
  BEGIN
    pp.reset();
    IF pp.keywordPresent("user") THEN
      SchedPrivate.userPreemptive := NOT SchedPrivate.userPreemptive;
    ELSIF pp.keywordPresent("kernel") THEN
      SchedPrivate.kernelPreemptive := NOT SchedPrivate.kernelPreemptive;
    END;
    IO.Put("User preemption "&preemptive[SchedPrivate.userPreemptive]&"\n");
    IO.Put("Kernel preemption "&preemptive[SchedPrivate.kernelPreemptive]&"\n");
    RETURN TRUE;
  END PREEMPTIVE;

PROCEDURE YIELD (
    <*UNUSED*> closure : REFANY;
    <*UNUSED*> pp      : ParseParams.T): BOOLEAN =
  BEGIN
    Strand.Yield();
    RETURN TRUE;
  END YIELD; 

PROCEDURE TICKS (
    <*UNUSED*> closure : REFANY;
    <*UNUSED*> pp      : ParseParams.T): BOOLEAN =
  BEGIN
    IO.Put("There have been " & Fmt.Int(Clock.ReadTicks()) & " ticks\n");
    RETURN TRUE;
  END TICKS; 

PROCEDURE MALLOCSTATS (
    <*UNUSED*> closure : REFANY;
    <*UNUSED*> pp      : ParseParams.T): BOOLEAN =
  BEGIN
    Sal.MallocStats();
    RETURN TRUE;
  END MALLOCSTATS; 

PROCEDURE Help(closure : REFANY; pp: ParseParams.T) : BOOLEAN =
  VAR
    detailed := FALSE;
    name: TEXT := NIL;
  BEGIN
    IO.Put("Help.Run running\n");
    pp.reset();
    TRY
      pp.skipNext();
      IF pp.keywordPresent("-l") THEN
	detailed := TRUE;
      END;
      IF pp.next <= LAST(pp.arg^) THEN
	name := pp.getNext();
      END;
      Shell.Help(name, detailed);
      RETURN TRUE;
    EXCEPT
    | ParseParams.Error => 
      Commands.ParseError(closure);
    END;
    RETURN TRUE;
  END Help; 

PROCEDURE HALT (
    <*UNUSED*> closure : REFANY;
    <*UNUSED*> pp      : ParseParams.T) : BOOLEAN =
  BEGIN
    Sal.Halt();
    RETURN TRUE;
  END HALT; 

PROCEDURE DUMP (
    <*UNUSED*> closure : REFANY;
    <*UNUSED*> pp      : ParseParams.T): BOOLEAN =
  BEGIN
    Log.Dumplog();
    RETURN TRUE;
  END DUMP; 

PROCEDURE CRASH(
    <*UNUSED*> closure : REFANY;
    <*UNUSED*> pp      : ParseParams.T) : BOOLEAN =
  BEGIN
    RTOS.Crash();
    RETURN TRUE;
  END CRASH; 

PROCEDURE DEBUG (
    <*UNUSED*> closure : REFANY;
    <*UNUSED*> pp      : ParseParams.T) : BOOLEAN =
  BEGIN
    Debugger.Enter();
    RETURN TRUE;
  END DEBUG; 

PROCEDURE Init(<*UNUSED*> verbose: BOOLEAN) = 
  VAR
  BEGIN
    EVAL Commands.Install(YIELD, "yield", " relinquish the processor");
    EVAL Commands.Install(TICKS, "ticks", " show number of ticks since boot");
    EVAL Commands.Install(MALLOCSTATS, "mallocstats",
			  " print information on the untraced heap");
    EVAL Commands.Install(Help,"help", "[-l] [command]",
			  "Show help for COMMAND. ",
			  "With -l, long help is displayed.");
    EVAL Commands.Install(HALT,"halt", " halt to the prom");
    EVAL Commands.Install(DUMP,"dump", " show debug log");
    EVAL Commands.Install(CRASH,"crash", " hard crash through RTOS");
    EVAL Commands.Install(DEBUG, "debug", " go to the debugger");
    EVAL Commands.Install(PREEMPTIVE, "preemptive",
			  " [user|kernel] en/disable preemption of named space");
  END Init;

PROCEDURE TheTick (wr: Wr.T) =
  BEGIN
    Wr.PutText(wr,
	    Fmt.Int(Clock.ReadTicks()) & "\n" &
	    "Clock ticks since boot.\n");
  END TheTick;

BEGIN
  Init(TRUE);
  TRY
     InfoFile.Create("/proc/ticks",TheTick);
  EXCEPT
  | Error.E(e) =>
    IO.PutError("gc procfs files:" & e.message() & "\n");
  END;
END CoreCommands.

(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 17-Feb-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
UNSAFE MODULE Main;
IMPORT IO, Fmt, Scan;
IMPORT Text;
IMPORT Thread;
IMPORT Params;
IMPORT TransCommands;
IMPORT TransDaemon;
IMPORT TransOS;
IMPORT Extern;
IMPORT PhysAddr;
IMPORT TransUtils;
IMPORT Uresource;
IMPORT UNIXUtils;

PROCEDURE ExecServer (<*UNUSED*>t : Thread.Closure) : REFANY =
  BEGIN
    TransDaemon.Loop(0);
    RETURN NIL;
  END ExecServer;

TYPE
  Test = {None, OO7, RDS, RVM, Micro};
  
VAR
  i : INTEGER;
  arg : TEXT;
  test: Test := Test.None;
  serverMode := FALSE;
  clientserverMode := FALSE;
  suppressPrompt := FALSE;
  argc, argv: INTEGER;
  argvVector: ARRAY [0 .. 100] OF INTEGER;
  argvTexts: ARRAY [0 .. 100] OF REF ARRAY OF CHAR;

PROCEDURE CommandLoop (suppressPrompt: BOOLEAN) =
  VAR
    argv: REF ARRAY OF TEXT;
    line: TEXT;
  BEGIN
    TRY
      LOOP
	IF NOT suppressPrompt THEN IO.Put("trans>"); END;
	line := IO.GetLine();
	argv := UNIXUtils.Tokenize(line);
	IF NUMBER(argv^) = 0 THEN
	  (* empty line *)
	ELSIF Text.GetChar(argv[0], 0) = '#' THEN
	  (* Comment line *)
	ELSE
	  TransCommands.Execute(argv^);
	END;
      END;
    EXCEPT
    | IO.Error =>
      IO.Put("main.commandloop: IO error.\n");
      TransOS.Exit(1);
    END;
  END CommandLoop;

  
(* Convert Params into C style argc/argv pair. *)  
PROCEDURE ConstructArgv() =
  VAR
    x: CARDINAL := 1;
    arg: TEXT;
  BEGIN
    argc := 0;
    argvTexts[0] := NEW(REF ARRAY OF CHAR, 2);
    Text.SetChars(argvTexts[0]^, "db");
    
    FOR j := i TO Params.Count-1 DO
      arg := Params.Get(j);
      argvTexts[x] := NEW(REF ARRAY OF CHAR, Text.Length(arg));
      Text.SetChars(argvTexts[x]^, arg);
      INC(x);
    END;

    FOR j := 0 TO x-1 DO 
      argvVector[j] := LOOPHOLE(ADR(argvTexts[j][0]), INTEGER);
    END;

    argc := x;
    argv := LOOPHOLE(ADR(argvVector[0]), INTEGER);
  END ConstructArgv;
  
PROCEDURE PrintUsageAndDie () =
  BEGIN
    IO.Put("db [-n] [-s] [-a] [-bufsize PAGENUM] [-q]");
    IO.Put("[-bench|-rdstest|-oo7] args...\n");
    (* IO.Put("  -n: turn off debug messages.\n"); *)
    IO.Put("  -s: server mode.\n");
    IO.Put("  -a: both server and client mode.\n");
    IO.Put("  -bufsize: specify max # of pages held in buffer. \n");
    IO.Put("            default: " & Fmt.Int(PhysAddr.MaxPages) & " pages.\n");
    IO.Put("  -q: don't output prompt string.\n");
    IO.Put("  other options starts the specific benchmark program.\n");
    TransOS.Exit(1);
  END PrintUsageAndDie;

BEGIN


  Extern.cancel_memory_limits();
  
  i := 1; (* skip argv[0]. *)
  WHILE i < Params.Count DO 
    arg := Params.Get(i);
    IF Text.GetChar(arg, 0) # '-' THEN EXIT; END;

    IF Text.Equal(arg, "-bench") THEN
      test := Test.RVM;
      INC(i);
      EXIT;
    ELSIF Text.Equal(arg, "-rdstest") THEN
      test := Test.RDS;
      INC(i);
      EXIT;
    ELSIF Text.Equal(arg, "-oo7") THEN
      test := Test.OO7;
      INC(i);
      EXIT;
    ELSIF Text.Equal(arg, "-micro") THEN
      test := Test.Micro;
      INC(i);
      EXIT;
    ELSIF Text.Equal(arg, "-bufsize") THEN
      INC(i);
      TRY
	PhysAddr.MaxPages := Scan.Unsigned(Params.Get(i));
      EXCEPT
      ELSE
	PrintUsageAndDie();
      END;
    ELSIF Text.Equal(arg, "-s") THEN
      (* Server mode *)
      serverMode := TRUE;
    ELSIF Text.Equal(arg, "-q") THEN
      suppressPrompt := TRUE;
    ELSIF Text.Equal(arg, "-a") THEN
      clientserverMode := TRUE;
    ELSIF Text.Equal(arg, "-n") THEN
      (* TransUtils.Debug := FALSE; *)
    ELSE
      PrintUsageAndDie();
    END;
    INC(i);
  END;

  CASE test OF
  | Test.RVM =>
    ConstructArgv();
    Extern.bench_start(argc, argv);
  | Test.OO7 =>
    ConstructArgv();
    Extern.oo7_main(argc, argv);
  | Test.RDS =>
    ConstructArgv();
    Extern.rds_test_main(argc, argv);
  | Test.Micro =>
    ConstructArgv();
    Extern.micro_start(argc, argv);
  ELSE
    IF serverMode THEN
      TransDaemon.Loop(0);
    ELSIF clientserverMode THEN 
      IO.Put("Transaction server&client started.\n");
      EVAL Thread.Fork(NEW(Thread.Closure, apply := ExecServer));
      CommandLoop(suppressPrompt);
    ELSE
      IO.Put("Standalone transaction manager started.\n");
      CommandLoop(suppressPrompt);
    END;
  END;
  
END Main.

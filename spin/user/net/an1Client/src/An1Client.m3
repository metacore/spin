(* 
 * HISTORY
 * 28-Jun-95  Marc Fiuczynski (mef) at the University of Washington
 *	Initialization module called started by the dynamic linker after
 *	the M3 code has been initialized.
 *)

MODULE An1Client;
IMPORT IO;
(* IMPORT An1AM; *)
IMPORT An1Arp;
IMPORT An1Classification;
IMPORT An1Default;
IMPORT An1PktFormat;
IMPORT Net;
IMPORT NetDb;

(* DynShell support *)
IMPORT Text, SafeConvert;
IMPORT Dispatcher;
IMPORT SpinShell;

<* UNUSED *> VAR shell := Dispatcher.Install(SpinShell.RunCommand,EC_Guard,RunCommand);

PROCEDURE EC_Guard(argc: INTEGER; READONLY argv: ARRAY OF TEXT): BOOLEAN =
  BEGIN
    RETURN argc >= 1 AND Text.Equal(argv[0], "An1Client");
  END EC_Guard;

PROCEDURE RunCommand(argc: INTEGER; READONLY argv: ARRAY OF TEXT): BOOLEAN =
  VAR
  BEGIN
    IF argc > 1 AND Text.Equal(argv[1], "debug") THEN
      debug_level := Net.MapDebug(SafeConvert.Atoi(argv[2]));
    ELSIF argc > 2 AND Text.Equal(argv[1], "install") THEN
      IF Text.Equal(argv[2], "default") THEN
        An1Default.Init();
      ELSIF Text.Equal(argv[2], "am") THEN
(*        An1AM.Init(); *)
      ELSIF Text.Equal(argv[2], "all") THEN
(*        An1AM.Init(); *)
        An1Classification.Init();
        An1Default.Init();
      END;
    ELSE
      IO.Put("An1Client.RunCommand: no such command ");
      FOR i := 0 TO argc DO
        IO.Put(argv[i]);
      END;
      IO.Put("\n");
    END;
    RETURN TRUE;
  END RunCommand;


VAR
  debug_level : Net.Level := Net.oLevel.NODEBUG;


PROCEDURE Init () = 
  BEGIN
  END Init;

BEGIN
    IO.Put("starting An1Client()\n");
    An1Arp.Init();
    An1Classification.Init();
    An1Default.Init();
END An1Client.

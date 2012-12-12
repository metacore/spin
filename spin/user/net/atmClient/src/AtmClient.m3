(* 
 * HISTORY
 *
 * 18-Dec-95  Przemek Pardyak (pardy) at the University of Washington
 *	Added Help procedure to conform to the new shell interface.
 *
 * 28-Jun-95  Marc Fiuczynski (mef) at the University of Washington
 *	Initialization module called started by the dynamic linker after
 *	the M3 code has been initialized.
 *)

MODULE AtmClient;
IMPORT Net;
IMPORT IO;
IMPORT AtmClassification;

(* DynShell support *)
IMPORT Text, SafeConvert;
IMPORT SpinShell;

<* UNUSED *> VAR shell := SpinShell.InstallCommand(EC_Guard,RunCommand,Help);

PROCEDURE EC_Guard(<* UNUSED *> argc: CARDINAL; READONLY argv: ARRAY OF TEXT): BOOLEAN =
  BEGIN
    RETURN Text.Equal(argv[0], "AtmClient");
  END EC_Guard;

PROCEDURE RunCommand(argc: CARDINAL; READONLY argv: ARRAY OF TEXT): BOOLEAN =
  VAR
  BEGIN
    IF Text.Equal(argv[1], "debug") THEN
      debug_level := Net.MapDebug(SafeConvert.Atoi(argv[2]));
    ELSIF Text.Equal(argv[1], "install") THEN
      IF Text.Equal(argv[2], "classification") THEN
        AtmClassification.Init();
      ELSIF Text.Equal(argv[2], "all") THEN
        AtmClassification.Init();
      END;
    ELSE
      IO.Put("AtmClient.RunCommand: no such command ");
      FOR i := 0 TO argc DO
        IO.Put(argv[i]);
      END;
      IO.Put("\n");
    END;
    RETURN TRUE;
  END RunCommand;

PROCEDURE Help () =
  BEGIN
  END Help;


VAR
  debug_level : Net.Level := Net.oLevel.NODEBUG;

PROCEDURE Init () = 
  BEGIN
    IO.Put("AtmClient() initialized\n");
  END Init;

BEGIN
END AtmClient.

(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 16-Jul-97  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
MODULE BSDtty;
IMPORT Word;
IMPORT SalDep;
IMPORT Debugger;
IMPORT IO;

REVEAL T = Public BRANDED OBJECT
OVERRIDES
  charsAvail := CharsAvail;
  sigHandler := SigHandler;
  ioctl := Ioctl;
  selectproc := Selectproc;
  descriptor := Descriptor;
END;

PROCEDURE CharsAvail(<*UNUSED*>t: T): CARDINAL =
  BEGIN
    RETURN 0;
  END CharsAvail;

PROCEDURE SigHandler(<*UNUSED*>t: T;
		     <*UNUSED*>handler: SignalHandler) =
  BEGIN
  END SigHandler;

PROCEDURE Ioctl(<*UNUSED*>t: T;
		<*UNUSED*>cmd: INTEGER; 
		<*UNUSED*>VAR arg: ARRAY OF CHAR;
		<*UNUSED*>flags: INTEGER) =
  BEGIN
  END Ioctl;

PROCEDURE Selectproc (t: T): SalDep.SelectProc =
  BEGIN
    IO.PutError(t.name() & " : selectproc unimplemented\n");
    Debugger.Enter();
    RETURN NIL;
  END Selectproc;
  
PROCEDURE Descriptor (t: T): Word.T =
  BEGIN
    IO.PutError(t.name() & " : descriptor unimplemented\n");
    Debugger.Enter();
    RETURN 0;
  END Descriptor;

BEGIN
END BSDtty.

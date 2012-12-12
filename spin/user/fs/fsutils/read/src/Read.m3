(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 5-Jun-96  Eric Christoffersen (ericc) at the University of Washington
 *	Created.
 *
 *)
(* Program for testing read *)
MODULE Read;

IMPORT IO, Fmt,ParseParams;
IMPORT FileSystem, File;
IMPORT Error;
IMPORT LFSTypes;
IMPORT Text;
IMPORT SafeConvert;

CONST DEBUG = FALSE;


PROCEDURE Run(pp: ParseParams.T):BOOLEAN =
VAR
        from    : CARDINAL := 0;
	thePath : TEXT;
        howMuch : CARDINAL;
        thePlace: INTEGER;
        fp: File.T;
        file : REF ARRAY OF CHAR;
BEGIN

  IF DEBUG = TRUE THEN
    IO.Put("Read called.\n");
  END;
  
  pp.reset();
  TRY
    pp.skipNext();
    thePath := pp.getNext();

    thePlace := SafeConvert.Atoi(pp.getNext());
        (* this is how they fetched numbers in fork *)

    howMuch := SafeConvert.Atoi(pp.getNext());

  EXCEPT
  | ParseParams.Error => IO.Put("usage: " & CommandHelp & "\n");
    RETURN FALSE;
  END;
  
  IF DEBUG = TRUE THEN
    IO.Put("Attempting to read "&Fmt.Int(howMuch)&
      " bytes from file:"&thePath&
      " at byte:"&Fmt.Int(thePlace)&".\n");
  END;

  TRY
    (*fp := FileSystem.Open(LFSTypes.ORDONLY,thePath);*)
    fp := FileSystem.Open(thePath);
    TRY
      fp.readRef(thePlace,howMuch, file,from);

      IF DEBUG = TRUE THEN
        IO.Put("Read " & Fmt.Int(BYTESIZE(file^)) & " bytes from file " & thePath & ":\n");
      END;

      IO.Put(Text.FromChars(file^)&"\n");

    FINALLY
      fp.close();
      file:=NIL;
    END;
  EXCEPT
  | Error.E => IO.PutError("Could not read " & thePath & ".\n");
  END;

  RETURN TRUE;
END Run;


BEGIN
END Read.




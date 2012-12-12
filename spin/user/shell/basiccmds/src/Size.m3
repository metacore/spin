(* 
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * Show functions.
 *
 * HISTORY
 * 20-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	Whisted. ParseParams.
 *)
MODULE Size;

IMPORT Error, File, IO, Fmt, FileSystem, ParseParams;

CONST BufferSize = 8 * 1024;

PROCEDURE Run (pp: ParseParams.T): BOOLEAN =
  VAR
    file    : REF ARRAY OF CHAR;
    filename: TEXT;
    f       : File.T;
    bufsize : CARDINAL          := BufferSize;
    size    : CARDINAL          := 0;
    from: CARDINAL;
  BEGIN
    pp.reset();
    TRY
      pp.skipNext();
      filename := pp.getNext();
    EXCEPT
      ParseParams.Error => IO.Put("size <file>\n"); RETURN FALSE;
    END;

    TRY
      IO.Put("File " & filename & "\n");
      TRY
        f := FileSystem.Open(filename);
        WHILE bufsize = BufferSize DO
	  f.readRef(size, bufsize, file, from);
          size := size + bufsize;
        END;
        IO.Put("Read " & Fmt.Int(size) & " from file " & filename & "\n");
      EXCEPT
      | Error.E => IO.PutError("Could not cat " & filename & "\n");
      END;
    FINALLY
      TRY
        IF f # NIL THEN f.close(); END;
      EXCEPT
      | Error.E => IO.PutError("Could not close " & filename & "\n");
      END;
    END;

    RETURN TRUE;
  END Run;


BEGIN
END Size.


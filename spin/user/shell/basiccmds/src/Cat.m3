(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 26-Mar-96  Charles Garrett (garrett) at the University of Washington
 *	Changed from printing one character to printing the whole 
 *      array at once.
 *
 * 05-Mar-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed to print the number of bytes read from file.
 *
 * 08-Jan-96  Yasushi Saito (yasushi) at the University of Washington
 *	Updated to use new file system interface. 
 *
 * 20-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	Whisted.  Parseparams
 *
 *)
MODULE Cat;
IMPORT ShellCommand;
IMPORT File, Error, IO, FileSystem, Fmt, ParseParams, Text;
IMPORT FileStat;

PROCEDURE Run (pp: ParseParams.T): BOOLEAN =
  VAR
    bytes    : CARDINAL;  
    entry    : REFANY;
    f        : File.T;
    buf      : REF ARRAY OF CHAR;
    filename : TEXT;
    from     : CARDINAL;  
    stat     : FileStat.T;

  BEGIN
    pp.reset();
    TRY
      pp.skipNext();
      filename := pp.getNext();
      IO.Put("File " & filename & "\n");
      entry := FileSystem.Open(filename);
      TYPECASE entry OF
      | NULL =>
        IO.Put("Not found.\n");
      | File.T(file) =>
        TRY
          f := file.open(0 (* XXX need real mode *));
          f.stat(stat);
          bytes := stat.size;
          f.readRef(0, bytes, buf, from);
          IO.Put("Read " & Fmt.Int(bytes) & " from file " & filename & "\n");
          IO.Put(Text.FromChars(SUBARRAY(buf^, from, bytes)));
        FINALLY
          f.close();
        END;
      ELSE
        IO.Put("Not a file.\n");
      END;
    EXCEPT
    | Error.E(e) => IO.PutError(filename & ":" & e.message() & "\n");
    | ParseParams.Error =>
      IF filename = NIL THEN IO.PutError("No filename\n"); END;
    END;
    RETURN TRUE;
  END Run;

BEGIN
  ShellCommand.Install("cat", "filename", Run);
END Cat.


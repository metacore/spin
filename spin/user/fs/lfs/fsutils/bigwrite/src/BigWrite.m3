(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 5-Jun-96  Eric Christoffersen (ericc) at the University of Washington
 *	Shell command for testing/using file system write, Created.
 *
 *)
MODULE BigWrite;

IMPORT IO, Fmt,ParseParams;
IMPORT FileSystem, File;
(* IMPORT LFSTypes; *)
IMPORT Text;
IMPORT SafeConvert;
(*IMPORT Error;*)
    
CONST DEBUG = FALSE;
CONST WRITESIZE = 524288;
          
PROCEDURE Run(pp: ParseParams.T):BOOLEAN =
  VAR
    thePath : TEXT;
    number,bytes  : INTEGER;
    theText : TEXT;
    thePlace: CARDINAL;
    textLength:CARDINAL;
    fp: File.T;
    bytesWritten:CARDINAL;
    buffer,interBuf  : REF ARRAY OF CHAR;
  BEGIN

    IF DEBUG = TRUE THEN
      IO.Put("BigWrite called.\n");
    END;
    
    pp.reset();
    TRY
      pp.skipNext();
      thePath := pp.getNext();
      bytes := SafeConvert.Atoi(pp.getNext()); (* this is how they fetched numbers in fork *)
      number := bytes DIV WRITESIZE;
      theText := pp.getNext();
    EXCEPT
    | ParseParams.Error => IO.Put("usage: " & CommandHelp & "\n");
      RETURN FALSE;
    END;
    
    IF DEBUG = TRUE THEN
      IO.Put("Attempting to write "&Fmt.Int(number)&" instances of '"&theText&"' to file:"&thePath&".\n");
    END;
    
    (*fp := FileSystem.Open(LFSTypes.OEXIST,thePath);*)

    fp := FileSystem.Open(thePath);

    TYPECASE fp OF
    | NULL=>
      IO.Put("File Not Found.\n");
    | File.T(file) =>
      TRY
        textLength := Text.Length(theText);
        
        interBuf := NEW(REF ARRAY OF CHAR,textLength);
        Text.SetChars(interBuf^,theText);

        buffer := NEW(REF ARRAY OF CHAR, WRITESIZE);

        (* got to fill up the write array *)
        bytesWritten := 0;
        WHILE bytesWritten+textLength < WRITESIZE DO
          SUBARRAY(buffer^,bytesWritten,textLength) := interBuf^;
          bytesWritten:= bytesWritten + textLength;
        END;

        IF bytesWritten < WRITESIZE THEN
          SUBARRAY(buffer^,bytesWritten,WRITESIZE-bytesWritten):= SUBARRAY(interBuf^,0,WRITESIZE-bytesWritten);
        END;

        bytesWritten := 0;

        thePlace := 0;

        IF number < 1 THEN
          bytesWritten := fp.write(SUBARRAY(buffer^,0,bytes), thePlace);
        ELSE
          FOR i := 1 TO number DO
            bytesWritten:=fp.write(buffer^, thePlace);
            thePlace := thePlace + bytesWritten;
          END;
          bytesWritten := fp.write(SUBARRAY(buffer^,0,bytes-thePlace), thePlace);
        END;
        thePlace := thePlace + bytesWritten;
      FINALLY
        fp.close();
      END;
    ELSE
      IO.Put("Not a file.\n");
    END;
    IF DEBUG = TRUE THEN
      IO.Put("Close called, file closed.\n");
    END;

    buffer := NIL;

    RETURN TRUE;
  END Run;
  

BEGIN
END BigWrite.




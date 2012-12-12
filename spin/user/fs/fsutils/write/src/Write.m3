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
MODULE Write;

IMPORT IO, Fmt,ParseParams;
IMPORT FileSystem, File;
IMPORT LFSTypes;
IMPORT Text;
IMPORT SafeConvert;
(*IMPORT Error;*)
    
CONST DEBUG = FALSE;
          
PROCEDURE Run(pp: ParseParams.T):BOOLEAN =
  VAR
    thePath : TEXT;
    theText : TEXT;
    textLength : CARDINAL;
    thePlace: INTEGER;
    textArg : REF ARRAY OF CHAR;
    truncateFlag:CARDINAL;
    fp: File.T;
    bytesWritten:CARDINAL;
  BEGIN

    IF DEBUG = TRUE THEN
      IO.Put("Write called.\n");
    END;
    
    truncateFlag := LFSTypes.OEXIST;
    
    pp.reset();
    TRY
      pp.skipNext();
      thePath := pp.getNext();
      
      IF Text.Equal(thePath,"-t") THEN
        truncateFlag := LFSTypes.OTRUNC;
        thePath:= pp.getNext();
      END;

      thePlace := SafeConvert.Atoi(pp.getNext()); (* this is how they fetched numbers in fork *)
      theText := pp.getNext();
    EXCEPT
    | ParseParams.Error => IO.Put("usage: " & CommandHelp & "\n");
      RETURN FALSE;
    END;
    
    IF DEBUG = TRUE THEN
      IO.Put("Attempting to write to file:"&thePath&
        " at byte:"&Fmt.Int(thePlace)&".\n");
    END;
    
    IF DEBUG = TRUE THEN
      IF truncateFlag = LFSTypes.OTRUNC THEN
        IO.Put("Opening for truncation.\n");
      END;
    END;
    
    (* try to open it as existing, otherwise create it *)
    (*TRY
    *)
    
    (*fp := FileSystem.Open(truncateFlag,thePath);*)

    fp:=FileSystem.Open(thePath);
    
    (*EXCEPT
      | Error.E =>
      
      IO.Put("Creating new file.\n");
      IF fp = NIL THEN
      IO.Put("FP was NIL on non-existant file.\n");
      END;
      fp := FileSystem.Open(LFSTypes.OCREATE,thePath);
      END;
    *)
    
    textLength := Text.Length(theText);
    textArg := NEW(REF ARRAY OF CHAR, textLength);
    Text.SetChars(textArg^,theText);
    
    bytesWritten:=fp.write(thePlace,textArg^);
    
    IF DEBUG = TRUE THEN
      IO.Put("Write exited, thinks it sent "&Fmt.Int(textLength)&
        " chars to file "&thePath & "\n");
    END;
    
    fp.close();
    
    IF DEBUG = TRUE THEN
      IO.Put("Close called, file closed.\n");
    END;
    
    RETURN TRUE;
  END Run;
  

BEGIN
END Write.




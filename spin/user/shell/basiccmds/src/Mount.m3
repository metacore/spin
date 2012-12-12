(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 08-Jan-96  Yasushi Saito (yasushi) at the University of Washington
 *	Updated to use new file system interface.
 *	
 * 25-Dec-95  Emin Gun Sirer (egs) at the University of Washington
 *	Whisted. Mount a filesystem.
 *)
MODULE Mount;

IMPORT IO, Error, FileSystem, ParseParams;

PROCEDURE Run (pp: ParseParams.T): BOOLEAN =
  VAR
    dev     : TEXT;
    fs      : TEXT;
    path    : TEXT;
  BEGIN
    pp.reset();
    TRY
      pp.skipNext(); (* Command *)
      IF pp.testNext("-u") THEN
        path := pp.getNext();
        FileSystem.Unmount(path);
      ELSE
        dev  := pp.getNext();
        fs   := pp.getNext(); 
        path := pp.getNext();
        FileSystem.Mount(dev, fs, path);
      END
    EXCEPT
      Error.E(e) =>
        IO.PutError("mount failed, error code= "& e.message() & "\n");
    | ParseParams.Error => IO.PutError("Usage: mount dev fstype path\n");
    END;
    RETURN TRUE;
  END Run;

BEGIN
END Mount.



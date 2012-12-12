(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 06-Jun-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
MODULE EFSCmd;
IMPORT EFS;
IMPORT ParseParams;
IMPORT FileSystem, File;
IMPORT Text, Lex, Error, Scan, IO;

PROCEDURE CreateCmd (pp : ParseParams.T) =
  VAR
    dirName, name : TEXT;
    fh : File.T;
    size : CARDINAL;
  BEGIN
    TRY 
      dirName := pp.getNext();
      name := pp.getNext();
      size := Scan.Int(pp.getNext());
      fh := FileSystem.Open(0, dirName);
      
      IF NOT(ISTYPE(fh, EFS.T)) THEN
	IO.Put("efscreate : the path is not efs partition.\n");
	RETURN;
      END;

      EVAL EFS.Create(fh, name, size);
    EXCEPT
    | Lex.Error, ParseParams.Error =>
      IO.Put(CommandName & " : " & CommandHelp & ".\n");
      RETURN;
    | Error.E(e) =>
      IO.Put(CommandName & " : " & e.message() & ".\n");
      RETURN;
    END;
    
  END CreateCmd;

PROCEDURE NukeCmd(pp : ParseParams.T) =
  VAR
    fh : File.T;
  BEGIN
    TRY 
      fh := FileSystem.Open(0, pp.getNext());
    
      IF NOT ISTYPE(fh, EFS.T) THEN
	IO.Put("efs init : the path is not efs partition.\n");
	RETURN;
      END;
      EFS.Nuke(fh);
    EXCEPT
    | ParseParams.Error =>
      IO.Put(CommandName & " : " & CommandHelp & ".\n");
      RETURN;
    | Error.E(e) =>
      IO.Put(CommandName & " : " & e.message() & ".\n");
      RETURN;
    END;
  END NukeCmd;

PROCEDURE ZeroCmd(pp : ParseParams.T) =
  VAR
    file : TEXT;
    fh : File.T;
  BEGIN
    TRY
      file := pp.getNext();
      fh := FileSystem.Open(0, file);
      IF (NOT ISTYPE(fh, EFS.T)) OR ISTYPE(fh, EFS.T) THEN 
	IO.Put("efs init : the path is not efs file.\n");
	RETURN;
      END;
      
      EFS.Zero(fh);
    EXCEPT
    | ParseParams.Error =>
      IO.Put(CommandName & " : " & CommandHelp & ".\n");
      RETURN;
    | Error.E(e) =>
      IO.Put(file & " : " & e.message() & ".\n");
      RETURN;
    END;
    
  END ZeroCmd;

PROCEDURE LsCmd(pp : ParseParams.T) =
  VAR
    file : TEXT;
    fh : File.T;
  BEGIN
    TRY
      file := pp.getNext();
      fh := FileSystem.Open(0, file);
      IF NOT ISTYPE(fh, EFS.T) THEN 
	IO.Put("efs init : the path is not efs file.\n");
	RETURN;
      END;
      
      EFS.Ls(fh);
    EXCEPT
    | ParseParams.Error =>
      IO.Put(CommandName & " : " & CommandHelp & ".\n");
      RETURN;
    | Error.E(e) =>
      IO.Put(file & " : " & e.message() & ".\n");
      RETURN;
    END;
  END LsCmd;

PROCEDURE Run (pp : ParseParams.T) : BOOLEAN =
  VAR
    cmd : TEXT;
  BEGIN
    TRY
      pp.reset();
      pp.skipNext();
      cmd := pp.getNext();
      IF Text.Equal(cmd, "nuke") THEN 
	NukeCmd(pp);
      ELSIF Text.Equal(cmd, "create") THEN 
	CreateCmd(pp);
      ELSIF Text.Equal(cmd, "zero") THEN
	ZeroCmd(pp);
      ELSIF Text.Equal(cmd, "ls") THEN
	LsCmd(pp);
      ELSE
	IO.Put("efs : unknown subcmd " & cmd & ".\n");
      END;
    EXCEPT
    | ParseParams.Error =>
      IO.Put(CommandName & " : " & CommandHelp & ".\n");
    END;
    RETURN TRUE;
  END Run;

BEGIN
  
END EFSCmd.

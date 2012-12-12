(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 01-Apr-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

UNSAFE MODULE TransOS EXPORTS TransOS;
IMPORT TransUtils;
IMPORT Ctypes, Text, SID, M3toC, Fmt, IO;
IMPORT Unix, Udir, Process;
IMPORT HostID;

PROCEDURE FileNameToSID (fileName: TEXT): SID.T =
  VAR lid := Text.Hash(fileName) MOD (LAST(SID.LID)+1);
  BEGIN
    RETURN SID.T{hid := HostID.myID, lid := lid};
  END FileNameToSID;

PROCEDURE SIDToFileName (sid: SID.T): TEXT =
  VAR dh := Udir.opendir(M3toC.TtoS("."));
    de: Udir.direct_star;
    name: TEXT;
  BEGIN
    LOOP
      de := Udir.readdir(dh);
      IF de = NIL THEN
	EXIT;
      END;
      name := M3toC.StoT(LOOPHOLE(ADR(de.d_name), Ctypes.char_star));
      IF FileNameToSID(name) = sid THEN
	EVAL Udir.closedir(dh);
	RETURN name;
      END;
    END;
    EVAL Udir.closedir(dh);
    IO.Put("Hey! I can't find file sid=" & Fmt.Int(sid.lid) & ".\n");
    RETURN NIL;
  END SIDToFileName;

PROCEDURE Exit(x: INTEGER) =
  BEGIN
    Process.Exit(x);
  END Exit;
  
PROCEDURE GetDefaultLogFileName (): TEXT =
  VAR logName := "trans_log";
  BEGIN
    IF Unix.access(M3toC.TtoS("/dev/rrz2f"), Unix.W_OK) = 0 THEN
      (* On TAZ *)
      logName := "/dev/rrz2f";
    ELSIF Unix.access(M3toC.TtoS("/dev/rrz0f"), Unix.W_OK) = 0 THEN
      (* On SAM *)
      logName := "/dev/rrz0f";
    END;
    IO.Put("log device name is " & logName & ".\n");
    RETURN logName;
  END GetDefaultLogFileName;

BEGIN
END TransOS.

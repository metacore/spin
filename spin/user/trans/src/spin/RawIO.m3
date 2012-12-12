(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 05-Jun-96  Yasushi Saito (yasushi) at the University of Washington
 *  Created.
 *)
MODULE RawIO;
IMPORT IO;
IMPORT Fmt;
IMPORT Error;
IMPORT NameServer;
IMPORT File, FileSystem;
IMPORT FileStat AS SpinFileStat;
IMPORT Proc, ProcRep;
IMPORT Text;
IMPORT Debugger;
IMPORT UserSpaceThread;
IMPORT TransError;
IMPORT Spy;
FROM TransUtils IMPORT Msg, SpyTime;

VAR
  ioTimer: Spy.T;
  bytesWritten, bytesRead: CARDINAL;
  
REVEAL T = BRANDED REF RECORD
  h: File.T;
END;
PROCEDURE Stat (f: T): FileStat =
  VAR buf: SpinFileStat.T;
  BEGIN
    TRY
      f.h.stat(buf);
    EXCEPT
    | Error.E(e) =>
      Msg("rawio.stat:", e.message(), ".\n");
      buf.size := 0;
    END;
    RETURN FileStat{size := buf.size};
  END Stat;

PROCEDURE PosIsAligned (pos: CARDINAL): BOOLEAN =
  BEGIN
    IF pos MOD BlockSize # 0 THEN
      Debugger.Enter();
    END;
    RETURN TRUE;
  END PosIsAligned;
  
PROCEDURE Write (f: T; READONLY log: ARRAY OF CHAR; pos: CARDINAL) =
  VAR len: CARDINAL;
  BEGIN
    <*ASSERT PosIsAligned(pos)*>
    IF SpyTime THEN Spy.Enter(ioTimer); END;	  
    TRY
      len := f.h.write(log, pos);
      <*ASSERT len = NUMBER(log)*>
      INC(bytesWritten, len);
    EXCEPT
    | Error.E(e) =>
      IO.Put("rawwrite:" & e.message() & ".\n");
    END;
    IF SpyTime THEN Spy.Exit(ioTimer); END;
  END Write;
  
PROCEDURE Read (f: T; VAR log: ARRAY OF CHAR; pos: CARDINAL) : INTEGER =
  VAR len: CARDINAL;
  BEGIN
    <*ASSERT PosIsAligned(pos)*>
    IF SpyTime THEN Spy.Enter(ioTimer); END;
    TRY
      len := f.h.read(log, pos);
      <*ASSERT len = NUMBER(log)*>
      INC(bytesRead, len);
    EXCEPT
    | Error.E(e) =>
      IO.Put("	rawread:" & e.message() & ".\n");
      RETURN -1;
    END;
    IF SpyTime THEN Spy.Exit(ioTimer); END;
    RETURN len;
  END Read;

PROCEDURE Close (<*UNUSED*>f: T) =
  BEGIN
    (* f.t.close();*)
  END Close;

PROCEDURE Open (file: TEXT; <*UNUSED*>size: CARDINAL): T RAISES {Error.E} =
  VAR
    dir: NameServer.T;
    fh : File.T;
    proc: Proc.T;
  BEGIN
    TRY
      IF Text.GetChar(file, 0) = '/' THEN
	(* Absolute path *)
	dir := FileSystem.GetRoot();
      ELSE
	IF UserSpaceThread.Self() = NIL THEN
	  (* Called from inside kernel. *)
	  dir := FileSystem.Lookup(NIL, "/efs");
	ELSE
	  proc := Proc.Self();
	  dir := proc.cwd;
	END;
      END;
      
      fh := FileSystem.Lookup(dir, file);
      EVAL fh.open(3);
    EXCEPT
    | NameServer.Error(e) =>
      Msg(file, ":", Fmt.Int(ORD(e)));
      TransError.Raise(TransError.NO_SUCH_STORAGE);
    END;
    
    RETURN NEW(T, h := fh);
  END Open;

PROCEDURE PrintStat () =
  BEGIN
    IO.Put("total writes: " & Fmt.Int(bytesWritten) & " bytes.\n");
    IO.Put("total reads: " & Fmt.Int(bytesRead) & " bytes.\n");
  END PrintStat;
  
BEGIN
  IF SpyTime THEN
    ioTimer := Spy.Create("trans:rawio", TRUE);
  END;
END RawIO.


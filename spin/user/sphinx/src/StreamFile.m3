(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 13-Jun-97  David Becker at the University of Washington
 *      StreamRead now handles Errno
 * 31-May-97  David Becker at the University of Washington
 *      Brought bsd unix interfaces into sphinx from urt
 *      Unified Socket.Error and Error.E into Errno exception 
 *)
MODULE StreamFile;
IMPORT TextRefTbl;
IMPORT Fmt, IO, Wr, TextWr;
IMPORT Text AS LibText;
IMPORT File;
IMPORT FileStat;
IMPORT Error, Errno, ErrnoDep;
IMPORT IoctlPosix;
IMPORT BSDtty;
IMPORT BsdSignal;
IMPORT Types;
IMPORT Sphinx;
IMPORT Proc, ProcRep;
IMPORT Translation;
FROM SphinxUtils IMPORT Msg;

VAR ttyTable := NEW(TextRefTbl.Default).init();
  (* Maps to device name like "/dev/mouse" to Info struct *)

PROCEDURE InternTty(path : TEXT) : Info =
  VAR
    r : REFANY;
    info : Info;
  BEGIN
    IF ttyTable.get(path, r) THEN
      RETURN r;
    ELSE
      info := NEW(Info, name := path);
      EVAL ttyTable.put(path, info);
      RETURN info;
    END;
  END InternTty;

PROCEDURE SetFG (fh : T; gid : Types.Pid) =
  BEGIN
    fh.info.gid := gid;
  END SetFG;
  
REVEAL T = Public BRANDED OBJECT
OVERRIDES 
  read := StreamRead;
  write := StreamWrite;
  stat := StreamStat;
  (* 
   * Since you can't really close the console,
   * somebody should do the right thing in Console.close.
   * Then we wouldn't worry about it here.
   *)
  close := StreamClose;
  ioctl := StreamIoctl;
END;
  
PROCEDURE StreamRead (f : T;
		      VAR data: ARRAY OF CHAR;
		      <*UNUSED*>offset : File.OffsetT
		      ):CARDINAL RAISES {Error.E} =
  VAR proc: Proc.T := Translation.GetCurrent();
  BEGIN
    IF proc.grp.gid # f.info.gid THEN
      (* I'm the background *)
      IF proc.parent # NIL THEN 
	IO.Put("StreamRead : read from background.\n");
	TRY
	  EVAL Sphinx.Kill(-proc.grp.gid, BsdSignal.TTIN);
	  EVAL Sphinx.Kill(proc.parent.pid,  BsdSignal.CHLD);
	EXCEPT
	| Errno.E(err) =>
	  RAISE Error.E(NEW(Error.T).init(err));
	END;
	RAISE Error.E(NEW(Error.T).init(ErrnoDep.EINTR));
      END;
    END;
    
    (* I'm the foreground *)
    RETURN f.dev.read(data);
  END StreamRead;
  
PROCEDURE StreamWrite (<*UNUSED*>f : T;
		       READONLY data: ARRAY OF CHAR;
		       <*UNUSED*>offset : File.OffsetT): CARDINAL =
  BEGIN
    IO.Put(LibText.FromChars(data));
    RETURN NUMBER(data);
  END StreamWrite;

PROCEDURE StreamClose (f : T) RAISES {Error.E} =
  BEGIN
    f.dev.close();
  END StreamClose;

PROCEDURE StreamStat (<*UNUSED*>f : T;
		      <*UNUSED*>VAR s : FileStat.T) =
  BEGIN

  END StreamStat;

PROCEDURE StreamIoctl (f : T;
		       cmd : INTEGER;
		       VAR arg: ARRAY OF CHAR;
		       mode : INTEGER) RAISES {Error.E} =
BEGIN

  IF cmd = IoctlPosix.TIOCGPGRP THEN
    VIEW(arg, Types.Pid) := f.info.gid;
  ELSIF cmd = IoctlPosix.TIOCSPGRP THEN
    f.info.gid := VIEW(arg, Types.Pid);
    Msg("tiocspgrp : " & f.info.name & ":" & Fmt.Int(f.info.gid) & "\n");
  ELSE 
    NARROW(f.dev,BSDtty.T).ioctl(cmd, arg, mode);
    (* XXX error check *)
  END;
END StreamIoctl;


(*
 /dev/null file handle
 *)

REVEAL Null = Public BRANDED OBJECT
OVERRIDES
  read := NullRead;
  write := NullWrite;
  close := NullClose;
END;
  
PROCEDURE NullRead (<*UNUSED*>f: Null;
		    <*UNUSED*>VAR data: ARRAY OF CHAR;
		    <*UNUSED*>off: File.OffsetT): CARDINAL =
BEGIN
  RETURN 0;
END NullRead;

PROCEDURE NullWrite (<*UNUSED*>f : Null;
		     READONLY data: ARRAY OF CHAR;
		     <*UNUSED*>off: File.OffsetT):CARDINAL =

BEGIN
  RETURN NUMBER(data);
END NullWrite;

PROCEDURE NullClose (<*UNUSED*>f : Null) =
BEGIN
END NullClose;


(* Text file handle *)
  
VAR
  textWr : TextWr.T;
  closed : BOOLEAN;
    
REVEAL Text = File.T BRANDED OBJECT
OVERRIDES 
  read := TextRead;
  write := TextWrite;
  close := TextClose;
END;

PROCEDURE TextRead (<*UNUSED*>f : Text;
		    <*UNUSED*>VAR data: ARRAY OF CHAR;
		    <*UNUSED*>offset : File.OffsetT) :CARDINAL
  RAISES {Error.E} =
  BEGIN
    RAISE Error.E(NEW(Error.T).init(ErrnoDep.ENOSYS));
  END TextRead;

PROCEDURE TextWrite (<*UNUSED*>f : Text;
		     READONLY data: ARRAY OF CHAR;
		     <*UNUSED*>offset : File.OffsetT): CARDINAL =

  BEGIN
    IF textWr = NIL OR closed THEN
      textWr := TextWr.New();
    END;
    Msg("regress put " & LibText.FromChars(data));
    TRY 
      Wr.PutString(textWr, data);
    EXCEPT
    ELSE
    END;
    RETURN NUMBER(data);
  END TextWrite;

PROCEDURE TextClose (<*UNUSED*>f : Text) =
  BEGIN
    closed := TRUE;
  END TextClose;

PROCEDURE GetTextFHContents() : TEXT =
  BEGIN
    IF textWr = NIL THEN
      IO.Put("Hey, you called GetTextFHContents before ever using textfh.\n");
      RETURN "";
    END;
    RETURN TextWr.ToText(textWr);
  END GetTextFHContents;
  
BEGIN
END StreamFile.

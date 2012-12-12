(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 18-Jul-97  Tsutomu Owa (owa) at the University of Washington
 *	Made Gethostname work.  It just fetches shell 'hostname' though.
 *	Added a dummy Setitimer routine.
 *
 * 13-Jun-97  David Becker at the University of Washington
 *      Added Errno to Sethostid raises list.
 *
 * 31-May-97  David Becker at the University of Washington
 *      Unified Socket.Error and Error.E into Errno exception 
 *)

(* implements dummy syscalls *)

MODULE Dummies EXPORTS Sphinx;
IMPORT Errno, ErrnoDep, Text, Translation;
IMPORT VMError;
IMPORT Word;
IMPORT IO;
IMPORT SphinxMachineDep;
IMPORT Strand;
IMPORT CPU;
IMPORT Shell, Glob;

PROCEDURE Chmod(<*UNUSED*>path: TEXT; <*UNUSED*>mode: INTEGER): INTEGER
  RAISES {Errno.E} =
  BEGIN
    IO.Put("chmod : not supported\n");
    RAISE Errno.E(ErrnoDep.ENOSYS);
  END Chmod;

PROCEDURE Chown(<*UNUSED*>path: TEXT; <*UNUSED*>pid, gid: INTEGER): INTEGER
  RAISES {Errno.E} =
  BEGIN
    IO.Put("chown : not supported\n");
    RAISE Errno.E(ErrnoDep.ENOSYS);
  END Chown;

PROCEDURE Rename(<*UNUSED*>from, to : TEXT) : INTEGER RAISES {Errno.E} =
  BEGIN
    IO.Put("rename : not supported\n");
    RAISE Errno.E(ErrnoDep.ENOSYS);
  END Rename;

PROCEDURE Mknod(<*UNUSED*>path: TEXT; <*UNUSED*>mode, dev: INTEGER) : INTEGER
  RAISES {Errno.E} =
  BEGIN
    IO.Put("mknod : not supported\n");
    RAISE Errno.E(ErrnoDep.ENOSYS);
  END Mknod;

  
PROCEDURE Setreuid(<*UNUSED*>ruid, euid : INTEGER) : INTEGER RAISES {Errno.E} =
  BEGIN
    IO.Put("setreuid : not supported\n");
    RAISE Errno.E(ErrnoDep.ENOSYS);
  END Setreuid;

PROCEDURE Setuid(<*UNUSED*>uid : INTEGER) : INTEGER RAISES {Errno.E} =
  BEGIN
    IO.Put("setuid : not supported\n");
    RAISE Errno.E(ErrnoDep.ENOSYS);
  END Setuid;
  

PROCEDURE Setgid(<*UNUSED*>gid : INTEGER) : INTEGER RAISES {Errno.E} =
  BEGIN
    IO.Put("setgid : not supported\n");
    RAISE Errno.E(ErrnoDep.ENOSYS);
  END Setgid;
  
PROCEDURE Umask(<*UNUSED*>mask : INTEGER) : INTEGER RAISES {Errno.E} =
  BEGIN
    IO.Put("umask : not supported\n");
    RAISE Errno.E(ErrnoDep.ENOSYS);
  END Umask;

PROCEDURE Link(<*UNUSED*>src, dest: TEXT) : INTEGER RAISES {Errno.E} =
  BEGIN
    IO.Put("link : not supported\n");
    RAISE Errno.E(ErrnoDep.ENOSYS);
  END Link;
  
PROCEDURE Symlink(<*UNUSED*>src, dest : TEXT) : INTEGER RAISES {Errno.E} =
  BEGIN
    IO.Put("symlink : not supported\n");
    RAISE Errno.E(ErrnoDep.ENOSYS);
  END Symlink;

PROCEDURE Getgroups(<*UNUSED*>n : INTEGER; <*UNUSED*>ptr : Word.T)
  : INTEGER RAISES {Errno.E} =
  BEGIN
    IO.Put("getgroups : not supported\n");
    RAISE Errno.E(ErrnoDep.ENOSYS);
  END Getgroups;
  
PROCEDURE Setgroups(<*UNUSED*>n: INTEGER; <*UNUSED*>ptr: Word.T)
  : INTEGER RAISES {Errno.E} =
  BEGIN
    IO.Put("setgroups : not supported\n");
    RAISE Errno.E(ErrnoDep.ENOSYS);
  END Setgroups;

PROCEDURE Setdomainname(<*UNUSED*>ptr: Word.T; <*UNUSED*>len: INTEGER)
  : INTEGER RAISES {Errno.E}=
  BEGIN
    IO.Put("setdomainname : not supported\n");
    RAISE Errno.E(ErrnoDep.ENOSYS);
  END Setdomainname;

PROCEDURE Getdomainname(ptr : Word.T; <*UNUSED*>len : INTEGER)
        : INTEGER RAISES {VMError.E} =
  VAR
    name := "crapbox.cs.washington.edu";
    buf : ARRAY [0 .. 1023] OF CHAR; 
  BEGIN
    Text.SetChars(buf, name);
    Translation.Write(Translation.GetCurrent(),
		      SUBARRAY(buf, 0, Text.Length(name)), ptr);
    RETURN 0;
  END Getdomainname;

PROCEDURE Gethostname(ptr: Word.T; len: INTEGER)
        : INTEGER RAISES {VMError.E, Errno.E} =
  VAR
    name : TEXT;
    buf : ARRAY [0 .. 1023] OF CHAR; 
    nameLen : CARDINAL;
  BEGIN
    name := Glob.GetVariable(Shell.Vars(), "hostname");
    IF name = NIL THEN
      name := "";
    END;
    nameLen := Text.Length(name);

    IF nameLen > len THEN
      (* What's the correct way to handle this? *)
      RAISE Errno.E(ErrnoDep.EFAULT);
    ELSE
      Text.SetChars(buf, name);
      IF nameLen+1 <= len THEN 
        buf[nameLen] := VAL(0, CHAR);		(* null terminate it *)
      END;
      Translation.Write(Translation.GetCurrent(),
		      SUBARRAY(buf, 0, nameLen+1), ptr);
      RETURN 0;
    END;

    END Gethostname;

(*
PROCEDURE Gethostname(ptr: Word.T; <*UNUSED*>len: INTEGER)
        : INTEGER RAISES {VMError.E} =
  VAR
    name := "crapbox";
    buf : ARRAY [0 .. 1023] OF CHAR; 
  BEGIN
    Text.SetChars(buf, name);
    Translation.Write(Translation.GetCurrent(),
		      SUBARRAY(buf, 0, Text.Length(name)), ptr);
    RETURN 0;
  END Gethostname;
*)

PROCEDURE Getlogin(<*UNUSED*>n: INTEGER; ptr: Word.T)
  : INTEGER RAISES {VMError.E} =
  VAR
    name := "taz";
    buf : ARRAY [0 .. 1023] OF CHAR; 
  BEGIN
    Text.SetChars(buf, name);
    Translation.Write(Translation.GetCurrent(),
		      SUBARRAY(buf, 0, Text.Length(name)), ptr);
    RETURN 0;
  END Getlogin;

PROCEDURE Setlogin() : INTEGER RAISES {Errno.E} =
  BEGIN
    IO.Put("setlogin : not supported\n");
    RAISE Errno.E(ErrnoDep.ENOSYS);
  END Setlogin;

PROCEDURE Setregid(<*UNUSED*>rgid, egid : INTEGER) : INTEGER RAISES {Errno.E} =
  BEGIN
    IO.Put("setregid: not supported\n");
    RAISE Errno.E(ErrnoDep.ENOSYS);
  END Setregid;
  
PROCEDURE Sethostid() : INTEGER RAISES {Errno.E} =
  BEGIN
    IO.Put("sethostid: not supported\n");
    RAISE Errno.E(ErrnoDep.ENOSYS);
  END Sethostid;
  

  
PROCEDURE Getuid(<*UNUSED*>us : Strand.T; VAR ss : CPU.SavedState) =
  BEGIN
    SphinxMachineDep.GetpidReturn(ss, 0, 0);
  END Getuid;

PROCEDURE Getgid(<*UNUSED*>us : Strand.T; VAR ss : CPU.SavedState) =
  BEGIN
    SphinxMachineDep.GetpidReturn(ss, 0, 0);
  END Getgid;

PROCEDURE Sethostname(<*UNUSED*>ptr : Word.T; <*UNUSED*>len : INTEGER)
  : INTEGER RAISES {Errno.E}=
  BEGIN
    IO.Put("sethostname : not supported\n");
    RAISE Errno.E(ErrnoDep.ENOSYS);
  END Sethostname;

BEGIN
END Dummies.

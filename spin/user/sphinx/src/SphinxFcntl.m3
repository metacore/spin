(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 13-Jun-97  David Becker at the University of Washington
 *      Change all procs that use File to raise Error.E
 * 31-May-97  David Becker at the University of Washington
 *      Unified Socket.Error and Error.E into Errno exception 
 * 11-Jun-96 oystr at the University of Washington
 *	Add F_SETFL, F_GETFL support.
 *
 * 14-Apr-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
MODULE SphinxFcntl EXPORTS Sphinx;
IMPORT Word;
IMPORT IO;
IMPORT Ctypes;
IMPORT Fmt;
IMPORT OpenFile;
IMPORT ErrnoDep;
IMPORT Errno, Error;
IMPORT File;
IMPORT Fcntl AS URTFcntl;
IMPORT Proc, ProcRep;
IMPORT IoctlPosix;
FROM SphinxUtils IMPORT Msg;

VAR
  ioctlStr := NEW(REF ARRAY OF CHAR, BYTESIZE(Ctypes.int));
  
PROCEDURE Fcntl (fd : INTEGER; cmd, arg : INTEGER) : INTEGER RAISES {Errno.E, Error.E} =
  VAR
    proc := Proc.Self();
    fh : OpenFile.T;
  BEGIN
    LOCK proc.mu DO
      fh := Proc.FindFH(proc, fd);
    END;

    LOCK fh DO 
      CASE cmd OF
      | URTFcntl.F_DUPFD =>
	LOCK proc.mu DO
	  FOR i := arg TO ProcRep.MAX_NOFILE-1 DO
	    IF proc.fdTable[i] = NIL THEN
	      Proc.AllocateFDatSlot(proc, fh, i);
	      RETURN i;
	    END;
	  END;
	END;
	RAISE Errno.E(ErrnoDep.EMFILE);
      | URTFcntl.F_GETFD =>
	IF proc.closeOnExec[fd] THEN
	  RETURN URTFcntl.FD_CLOEXEC;
	ELSE
	  RETURN 0;
	END;
      | URTFcntl.F_SETFD =>
	IF arg # 0 THEN
	  proc.closeOnExec[fd] := TRUE;
	ELSE
	  proc.closeOnExec[fd] := FALSE;
	END;
	RETURN 0;
      | URTFcntl.F_SETFL =>
	
	IF Word.And(arg, URTFcntl.O_NONBLOCK) #
	  Word.And(fh.flags, URTFcntl.O_NONBLOCK) THEN
	  (* async mode changed *)
	  IF Word.And(arg, URTFcntl.O_NONBLOCK) # 0 THEN
	    (* blocking -> nonblocking *)
	    VIEW(ioctlStr^, Ctypes.int) := 1;
	  ELSE
	    (* nonblocking -> blocking *)
	    VIEW(ioctlStr^, Ctypes.int) := 0;
	  END;
	  TYPECASE fh.h OF
	  | File.T(file) =>
	    file.ioctl(IoctlPosix.FIONBIO, ioctlStr^, fh.flags);
	  ELSE
	    Msg("ioctl: you are trying to SETFL non-file. This is weird.\n");
	  END;

	END;

	(* Ignore the lower 3 bits(rw modes) in arg.
	   [I hate this unintelligible expression ;(] *)
	arg := Word.Or(Word.And(arg, Word.Not(3)), Word.And(fh.flags, 3));
	fh.flags := arg;
	  
	(* Should check validity of flags *)
	RETURN 0;
      | URTFcntl.F_GETFL =>
	RETURN fh.flags;
      | URTFcntl.F_GETOWN =>
	RETURN 0;
      | URTFcntl.F_SETOWN =>
	IO.Put("fcntl setown is ignored!\n");
	RETURN 0;
      ELSE
	Msg("fcntl : unknown command ", Fmt.Int(cmd));
	RAISE Errno.E(ErrnoDep.EINVAL);
      END;
    END;
END Fcntl;

PROCEDURE Dup(fd : INTEGER) : INTEGER RAISES {Errno.E, Error.E} =
  BEGIN
    RETURN Fcntl(fd, URTFcntl.F_DUPFD, 0);
  END Dup;

PROCEDURE Dup2(fd : INTEGER; new : INTEGER) : INTEGER RAISES {Errno.E, Error.E} =
  BEGIN
    RETURN Fcntl(fd, URTFcntl.F_DUPFD, new);
  END Dup2;
  
BEGIN
END SphinxFcntl.

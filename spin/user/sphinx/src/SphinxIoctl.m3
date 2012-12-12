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
 * 12-Jun-96 oystr at the University of Washington
 *	Make sure the ioctl 4-byte cmd hasn't been sign extended.
 *
 *)
MODULE SphinxIoctl EXPORTS Sphinx;
IMPORT OpenFile, File, IO;
IMPORT Proc, ProcRep;
IMPORT Translation, Errno, Error, ErrnoDep, Word;
IMPORT VMError;
FROM Ioctl IMPORT IOCPARM_LEN; (* from urt *)

CONST MaxArgLen = 1024;
  
PROCEDURE Ioctl (fd : INTEGER; cmd : INTEGER; arg : INTEGER)
  : INTEGER RAISES {Errno.E, VMError.E, Error.E} =
  VAR
    fh : OpenFile.T;
    argLen : INTEGER;
    buf : REF ARRAY OF CHAR := NIL;
    proc := Proc.Self();
  BEGIN
    LOCK proc.mu DO 
      fh := Proc.FindFH(proc, fd);
    END;
    (*
     * cmd's are 4-bytes long, as far as C is concerned.
     *)
    cmd := Word.And (cmd,16_ffffffff);
    argLen := MIN(IOCPARM_LEN(cmd), MaxArgLen);
    IF argLen > 0 THEN
      buf := NEW(REF ARRAY OF CHAR, argLen);
      Translation.Read(proc, arg, SUBARRAY(buf^, 0, argLen));
    ELSE
      buf := NEW(REF ARRAY OF CHAR, BYTESIZE(Word.T));
      VIEW(buf^, Word.T) := arg;
    END;
    
    TYPECASE fh.h OF
    | File.T(file) =>
      file.ioctl(cmd, buf^, 0);
    | NULL =>
      RAISE Errno.E(ErrnoDep.EBADF);
    ELSE
      IO.PutError("SphinxIoctl.Ioctl on non file. call mef.\n");
      RAISE Errno.E(ErrnoDep.EBADF);
    END;
    IF argLen > 0 THEN
      Translation.Write(proc, SUBARRAY(buf^, 0, argLen), arg);
    END;
    RETURN 0;
  END Ioctl;

BEGIN
END SphinxIoctl.

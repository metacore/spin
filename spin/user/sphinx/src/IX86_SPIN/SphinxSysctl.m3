(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 31-May-97  David Becker at the University of Washington
 *      Unified Socket.Error and Error.E into Errno exception 
 * 24-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)


(* Implementation of sysctl(2). *)

MODULE SphinxSysctl EXPORTS Sphinx, SphinxSysctl;
IMPORT Fmt;
IMPORT Text;
IMPORT CharArray;
IMPORT Translation;
IMPORT Proc;
IMPORT Errno;
IMPORT VMError;
IMPORT ErrnoDep;
IMPORT CPU;
FROM SphinxUtils IMPORT Msg;

CONST MaxBufferSize = 16_2000;

PROCEDURE SetString (buf: REF ARRAY OF CHAR; VAR len: INTEGER; str: TEXT) =
  BEGIN
    Text.SetChars(buf^, str);
    len := Text.Length(str);
  END SetString;
  
PROCEDURE SetInt (buf: REF ARRAY OF CHAR; VAR len: INTEGER; val: INTEGER) =
  BEGIN
    VIEW(buf^, INTEGER) := val;
    len := BYTESIZE(INTEGER);
  END SetInt;
    
PROCEDURE HW (READONLY mib: ARRAY OF INTEGER;
	      oldBuf: REF ARRAY OF CHAR; VAR oldBufLen: INTEGER;
	      newBuf: REF ARRAY OF CHAR; <*UNUSED*>newBufLen: INTEGER) RAISES
	      {Errno.E} =
  BEGIN
    IF NUMBER(mib) # 1 AND mib[0] # HW_DEVCONF THEN
      RAISE Errno.E(ErrnoDep.ENOTDIR);
    END;
    IF oldBuf = NIL OR newBuf # NIL THEN
      RAISE Errno.E(ErrnoDep.EINVAL);
    END;
    
    CASE mib[0] OF 
    | HW_PAGESIZE =>
      SetInt(oldBuf, oldBufLen, CPU.PAGESIZE);
    ELSE
      Msg("sysctl hw: unknown mib ", Fmt.Int(mib[0]));
      RAISE Errno.E(ErrnoDep.EINVAL);
    END;
  END HW;
  
PROCEDURE Sysctl (mibAddr, mibDepth, oldBufAddr: INTEGER;
		  VAR oldBufLen: INTEGER;
		  newBufAddr, newBufLen: INTEGER)
  : INTEGER RAISES {Errno.E, VMError.E} =
  VAR
    proc := Proc.Self();
    mib: ARRAY [0 .. 31] OF INTEGER;
    mibLen: CARDINAL;
    newBuf, oldBuf: REF ARRAY OF CHAR := NIL;
    ctlProc: PROCEDURE (READONLY mib: ARRAY OF INTEGER;
		     oldBuf: REF ARRAY OF CHAR; VAR oldBufLen: INTEGER;
		     newBuf: REF ARRAY OF CHAR; newBufLen: INTEGER)
      RAISES {Errno.E};
  BEGIN
    IF mibLen > LAST(mib) THEN
      Msg("sysctl: mib too deep(", Fmt.Int(mibLen));
      RAISE Errno.E(ErrnoDep.EINVAL);
    END;
    mibLen := mibDepth * BYTESIZE(INTEGER);
    Translation.Read(proc, mibAddr, 
		     SUBARRAY(VIEW(mib, ARRAY OF CHAR), 0, mibLen));
    
    CASE mib[0] OF
    | CTL_HW => ctlProc := HW;
    ELSE
      Msg("sysctl: unknown toplevel mib ", Fmt.Int(mib[0]));
      RAISE Errno.E(ErrnoDep.EINVAL);
    END;

    IF oldBufLen >= MaxBufferSize OR newBufLen >= MaxBufferSize THEN
      Msg("sysctl: buffer size too big ",
	  Fmt.Int(oldBufLen), Fmt.Int(newBufLen));
      RAISE Errno.E(ErrnoDep.EINVAL);
    END;

    IF newBufAddr # 0 THEN
      newBuf := CharArray.Allocate(newBufLen);
      Translation.Read(proc, newBufAddr, SUBARRAY(newBuf^, 0, newBufLen));
      CharArray.Free(newBuf);
    END;
    IF oldBufAddr # 0 THEN
      oldBuf := CharArray.Allocate(oldBufLen);
    END;
    
    ctlProc(SUBARRAY(mib, 1, mibDepth-1), oldBuf, oldBufLen,
	    newBuf, newBufLen);
    
    IF oldBufAddr # 0 THEN
      Translation.Write(proc, SUBARRAY(oldBuf^, 0, oldBufLen), oldBufAddr);
      CharArray.Free(oldBuf);
    END;
    
    RETURN 0;
  END Sysctl;


BEGIN
END SphinxSysctl.

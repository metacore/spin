(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
 * HISTORY
 * 18-Feb-96  Yasushi Saito (yasushi) at the University of Washington
 *	added Get{UserSpaceThread,Space}.
 * 17-Nov-95  Yasushi Saito (yasushi) at the University of Washington
 *	created.
 *)
MODULE Sieg;
IMPORT Translation, VirtAddr, Text;
IMPORT SiegInterface,Auth;
IMPORT SpinException;
IMPORT VMError;
IMPORT NameServer;
IMPORT TextF;

CONST
  MaxTextLen = 128;
  (* Max bytenumber of text that can be read from user space *)
  Chunk = 32;
  
TYPE TextHeader = RECORD
  chars: VirtAddr.Address;
  length: INTEGER;
END;

PROCEDURE UnpackTEXT(s: Translation.T; addr: VirtAddr.Address): TEXT
  RAISES {VMError.E}=
  VAR
    header: TextHeader;
    body: TEXT;
  BEGIN
    Translation.Read(s, addr, VIEW(header, ARRAY OF CHAR));
    body := NEW(TEXT, header.length);
    Translation.Read(s, header.chars, body^);
    RETURN body;
  END UnpackTEXT;
  
PROCEDURE UnpackCTEXT(s: Translation.T; addr : VirtAddr.Address): TEXT
  RAISES {VMError.E} =
  VAR
    buf : ARRAY [0..31] OF CHAR;
    v: TEXT := NIL;
    len : CARDINAL;
  BEGIN
    (* Read max. 32 bytes of string each time, and append to V. *)
    REPEAT 
      Translation.Read(s, addr, buf);
      len := 0;
      
      (* search for the null character *)
      WHILE len <= LAST(buf) AND buf[len] # '\000' DO
	INC(len);
      END;
      IF len <= LAST(buf) THEN
	IF v # NIL THEN v := v & Text.FromChars(SUBARRAY(buf, 0, len)); 
	ELSE v := Text.FromChars(SUBARRAY(buf, 0, len));
	END;
	EXIT;
      ELSE 
	IF v # NIL THEN v := v & Text.FromChars(buf);
	ELSE v := Text.FromChars(buf);
	END;
	INC(addr, BYTESIZE(buf));
      END;
    UNTIL Text.Length(v) >= MaxTextLen;
    RETURN v;
  END UnpackCTEXT;
  
PROCEDURE UnpackCArrayOfChar (s: Translation.T;
			      addr : VirtAddr.Address;
			      VAR buf: ARRAY OF CHAR;
			      VAR len: INTEGER) RAISES {VMError.E}=
  VAR
    BufLen := NUMBER(buf);
  BEGIN
    len := 0;
    
    (* Read max. "Chunk" bytes of string each time, and append to V. *)
    LOOP
      Translation.Read(s, addr, SUBARRAY(buf, len, BufLen-len));
      
      (* search for the null character *)
      WHILE len <= LAST(buf) AND buf[len] # '\000' DO
	INC(len);
      END;
      
      IF len <= LAST(buf) THEN
	EXIT;
      ELSE 
	INC(addr, Chunk);
      END;
    END;
  END UnpackCArrayOfChar;

PROCEDURE UnpackNSName (space: Translation.T;
			addr: VirtAddr.Address;
			VAR name: NameServer.Name) RAISES {VMError.E}=
  VAR
    bufLen := NUMBER(name.str^);
    len, x, readLen: CARDINAL;
  BEGIN
    len := 0;
    (* Read max. "Chunk" bytes of string each time, and append to V. *)
    LOOP
      readLen := MIN(Chunk, bufLen-len);
      Translation.Read(space, addr, SUBARRAY(name.str^, len, readLen));

      x := len + readLen;
      (* look for a null character *)
      WHILE len < x AND name.str[len] # '\000' DO
	INC(len);
      END;
      
      IF len < x THEN
	name.from := 0;
	name.end := len;
	RETURN;
      ELSE 
	INC(addr, readLen);
      END;
    END;
  END UnpackNSName;

CONST ErrorCodes = ARRAY [SpinException.ExceptionCode.NoException
			  .. SpinException.ExceptionCode.UnknownException]
  OF INTEGER {
	      0,
	      14,
	      14,
	      10000, (* unknown exception, *)
	      10000,
	      10001, (* assert *)
	      22, (* val out of range *)
	      22, (* array out of range *)
	      10002, (* incompatible array shapes *)
	      14,  (* nil *)
	      9, (* narrow *)
	      10003, (* func retval *)
	      10004, (* case *)
	      10005, (* typecase *)
	      10006, (* stack overflow *)
	      14, (* view size *)
	      14, (* view alignment *)
	      10007, (* no handler *)
	      14, (* unaligned *)
	      10008,
	      10009};
  
PROCEDURE SpinExceptionToErrno(READONLY e : SpinException.ExceptionInfo)
  : INTEGER =
  BEGIN
    RETURN ErrorCodes[e.code];
  END SpinExceptionToErrno;
  
BEGIN
  EVAL SiegInterface.Export(NEW(Auth.AuthAlways));
END Sieg.

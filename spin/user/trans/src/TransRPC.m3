(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 27-Nov-96  Yasushi Saito (yasushi) at the University of Washington
 *	Made safe.
 * 07-May-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

MODULE TransRPC;
IMPORT Round, Ctypes, Text;
IMPORT SID;
IMPORT TID;

REVEAL SendBuf = SendBufPublic BRANDED OBJECT
  maxSize: CARDINAL;
OVERRIDES
  clear := Clear;
  startNewCommand := StartNewCommand;
  stretchIfNecessary := StretchIfNecessarySend;
  packInt := PackInt;
  (* packInt2 := PackInt2;
  packInt3 := PackInt3;
  packInt4 := PackInt4;
  packInt5 := PackInt5; *)
  packHeader2 := PackHeader2;
  packHeader3 := PackHeader3;
  packHeader4 := PackHeader4;
  packHeader5 := PackHeader5;
  packBool:= PackBool;
  packText := PackText;
  packArray := PackArray;
  endPack := EndPack;
END;

(* Note: RecvBuf methods are implemented in
   osf/UNIXRPC.m3 or spin/SpinRPC.m3 *)

CONST SizeofInt = BYTESIZE(INTEGER);
  
  
PROCEDURE Clear (t: SendBuf) =
  BEGIN
    <*ASSERT t.buf # NIL*>
    t.idx := 8; (* 4 for len, 4 for nrequests. *)
    VIEW(SUBARRAY(t.buf^, 4, 4), Ctypes.int) := 0;
  END Clear;
  
PROCEDURE StartNewCommand (t: SendBuf) =
  BEGIN
    <*ASSERT t.buf # NIL*>
    (* Increment the # of requests *)
    INC(VIEW(SUBARRAY(t.buf^, 4, 4), Ctypes.int));
  END StartNewCommand;

PROCEDURE StretchIfNecessarySend (t: SendBuf; size: INTEGER) =
  BEGIN
    IF size >= NUMBER(t.buf^) THEN
      (* Buffer overflow. *)
      WITH n = NEW(REF ARRAY OF CHAR, MAX(size, 2*NUMBER(t.buf^))) DO
	SUBARRAY(n^, 0, NUMBER(t.buf^)) := t.buf^;
	t.buf := n;
      END;
    END;
  END StretchIfNecessarySend;

PROCEDURE PackInt (t: SendBuf; val: INTEGER) =
  BEGIN
    StretchIfNecessarySend(t, t.idx + BYTESIZE(INTEGER));
    VIEW(SUBARRAY(t.buf^, t.idx, SizeofInt), INTEGER) := val;
    INC(t.idx, SizeofInt);
  END PackInt;
  
PROCEDURE PackHeader2 (t: SendBuf; op: INTEGER; sid: SID.T) =
  BEGIN
    StretchIfNecessarySend(t, t.idx + SizeofInt*3);
    VIEW(SUBARRAY(t.buf^, t.idx, SizeofInt), INTEGER) := op;
    VIEW(SUBARRAY(t.buf^, t.idx+SizeofInt, SizeofInt), INTEGER) := sid.hid;
    VIEW(SUBARRAY(t.buf^, t.idx+SizeofInt*2, SizeofInt), INTEGER) := sid.lid;
    INC(t.idx, SizeofInt*3);
  END PackHeader2;
PROCEDURE PackHeader3 (t: SendBuf; op: INTEGER; sid: SID.T; tid: TID.T) =
  BEGIN
    StretchIfNecessarySend(t, t.idx + SizeofInt*4);
    VIEW(SUBARRAY(t.buf^, t.idx, SizeofInt), INTEGER) := op;
    VIEW(SUBARRAY(t.buf^, t.idx+SizeofInt, SizeofInt), INTEGER) := sid.hid;
    VIEW(SUBARRAY(t.buf^, t.idx+SizeofInt*2, SizeofInt), INTEGER) := sid.lid;
    VIEW(SUBARRAY(t.buf^, t.idx+SizeofInt*3, SizeofInt), INTEGER) := tid;
    INC(t.idx, SizeofInt*4);
  END PackHeader3;
PROCEDURE PackHeader4 (t: SendBuf; op: INTEGER; sid: SID.T; tid: TID.T;
		       v: INTEGER) =
  BEGIN
    StretchIfNecessarySend(t, t.idx + SizeofInt*5);
    VIEW(SUBARRAY(t.buf^, t.idx, SizeofInt), INTEGER) := op;
    VIEW(SUBARRAY(t.buf^, t.idx+SizeofInt, SizeofInt), INTEGER) := sid.hid;
    VIEW(SUBARRAY(t.buf^, t.idx+SizeofInt*2, SizeofInt), INTEGER) := sid.lid;
    VIEW(SUBARRAY(t.buf^, t.idx+SizeofInt*3, SizeofInt), INTEGER) := tid;
    VIEW(SUBARRAY(t.buf^, t.idx+SizeofInt*4, SizeofInt), INTEGER) := v;
    INC(t.idx, SizeofInt*5);
  END PackHeader4;
PROCEDURE PackHeader5 (t: SendBuf; op: INTEGER; sid: SID.T; tid: TID.T;
		       v1, v2: INTEGER) =
  BEGIN
    StretchIfNecessarySend(t, t.idx + SizeofInt*6);
    VIEW(SUBARRAY(t.buf^, t.idx, SizeofInt), INTEGER) := op;
    VIEW(SUBARRAY(t.buf^, t.idx+SizeofInt, SizeofInt), INTEGER) := sid.hid;
    VIEW(SUBARRAY(t.buf^, t.idx+SizeofInt*2, SizeofInt), INTEGER) := sid.lid;
    VIEW(SUBARRAY(t.buf^, t.idx+SizeofInt*3, SizeofInt), INTEGER) := tid;
    VIEW(SUBARRAY(t.buf^, t.idx+SizeofInt*4, SizeofInt), INTEGER) := v1;
    VIEW(SUBARRAY(t.buf^, t.idx+SizeofInt*5, SizeofInt), INTEGER) := v2;
    INC(t.idx, SizeofInt*6);
  END PackHeader5;

PROCEDURE PackBool (t: SendBuf; b: BOOLEAN) =
  VAR val: INTEGER;
  BEGIN
    StretchIfNecessarySend(t, t.idx + SizeofInt);
    IF b THEN val := 999 ELSE val := -999; END;
    VIEW(SUBARRAY(t.buf^, t.idx, SizeofInt), INTEGER) := val;
    INC(t.idx, SizeofInt);
  END PackBool;

PROCEDURE PackText (t: SendBuf; s: TEXT) =
  VAR
    len := Text.Length(s);
    recordLen := Round.Up8(len);
  BEGIN
    StretchIfNecessarySend(t, t.idx + recordLen + BYTESIZE(INTEGER));
    PackInt(t, len);
    Text.SetChars(SUBARRAY(t.buf^, t.idx, len), s);
    INC(t.idx, recordLen);
  END PackText;

PROCEDURE PackArray (t: SendBuf; READONLY x: ARRAY OF CHAR) =
  VAR
    len := NUMBER(x);
    recordLen := Round.Up8(len);
  BEGIN
    StretchIfNecessarySend(t, t.idx + recordLen+BYTESIZE(INTEGER));
    PackInt(t, len);
    
    SUBARRAY(t.buf^, t.idx, len) := x;
    INC(t.idx, recordLen);
  END PackArray;

PROCEDURE EndPack (t: SendBuf) =
  BEGIN
    (* Store the size of the packet *)
    VIEW(t.buf^, Ctypes.int) := t.idx;
  END EndPack;

PROCEDURE CreateSendBuf (bufSize: CARDINAL): SendBuf =
  VAR t := NEW(SendBuf);
  BEGIN
    t.buf := NEW(REF ARRAY OF CHAR, bufSize);
    Clear(t);
    RETURN t;
  END CreateSendBuf;

BEGIN
  Port := 8765;
END TransRPC.

(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 10-Dec-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
MODULE MachineRPC EXPORTS SpinRPC;
IMPORT IO, Fmt;
IMPORT Mbuf;
IMPORT SID;
IMPORT TID;
IMPORT Ctypes;
IMPORT Text;
IMPORT Round;
IMPORT Socket;
IMPORT Debugger;
IMPORT Errno;
FROM TransUtils IMPORT Debug, Msg;

(* XXX the RPC representation is architecture dependent!!!
   Alpha can only talk to Alpha, and x86 can only talk to x86.
   But I don't care for the moment. -- yas *)
   
REVEAL SpinRecvBuf = TPublic BRANDED OBJECT
OVERRIDES
  currentIdx := CurrentIdx;
  reset := Reset;
  unpackInt := UnpackInt;
  unpackInt2 := UnpackInt2;
  unpackInt3 := UnpackInt3;
  unpackInt4 := UnpackInt4;
  unpackHeader := UnpackHeader;
  unpackHeader2 := UnpackHeader2;
  unpackHeader3 := UnpackHeader3;
  unpackHeader4 := UnpackHeader4;

  unpackBool := UnpackBool;
  unpackText := UnpackText;
  unpackArray := UnpackArray;
  endUnpack := EndUnpack;
END;

CONST SizeofInt = BYTESIZE(INTEGER);
  
PROCEDURE CurrentIdx (t: SpinRecvBuf): CARDINAL =
  BEGIN
    RETURN t.cumulativeIdx;
  END CurrentIdx;
  
PROCEDURE Reset (t: SpinRecvBuf; mbuf: Mbuf.T; maxSize_: CARDINAL;
		 sock_: Socket.T) =
  BEGIN
    <*ASSERT t.mbuf = NIL*>
    t.mbuf := mbuf;
    t.buf := Mbuf.Array(mbuf);
    t.bufSize := NUMBER(t.buf^);
    t.cumulativeIdx := 0;
    t.curIdx := 0;
    t.maxSize := maxSize_;
    t.sock := sock_;
  END Reset;

PROCEDURE GetNextMbuf (t: SpinRecvBuf) =
  VAR n: CARDINAL;
  BEGIN
    t.mbuf := Mbuf.m_free(t.mbuf);
    IF t.mbuf = NIL THEN
      (* We've read all the current mbuf chain. Read the next one from
	 the socket. *)
      <*ASSERT t.cumulativeIdx < t.maxSize*>
      TRY
	n := Socket.Recv(t.sock, t.mbuf, t.maxSize-t.cumulativeIdx);
      EXCEPT
      | Errno.E(errno) =>
	IO.Put("machinerpc.getnextmbuf: " & Fmt.Int(errno) & ".\n");
      END;
      <*ASSERT n <= t.maxSize-t.cumulativeIdx*>
    END;
    t.buf := Mbuf.Array(t.mbuf);
    t.bufSize := NUMBER(t.buf^);
    t.curIdx := 0;
  END GetNextMbuf;

PROCEDURE UnpackInt (t: SpinRecvBuf): INTEGER =
  VAR
    val: ARRAY [0..1] OF Ctypes.int;
  BEGIN
    IF t.curIdx >= t.bufSize THEN
      (* XXX this doesn't handle the case where the mbuf size <
	 SizeofInt. *)
      GetNextMbuf(t);
    END;
    (* Mbuf only guarantees 4 byte alignment. So we have to take
       2 4byte int and combine it into long. *)
    val[0] := VIEW(SUBARRAY(t.buf^, t.curIdx, 4), Ctypes.int);
    val[1] := VIEW(SUBARRAY(t.buf^, t.curIdx+4, 4), Ctypes.int);
    INC(t.curIdx, SizeofInt);
    INC(t.cumulativeIdx, SizeofInt);
    <*ASSERT t.cumulativeIdx <= t.maxSize*>
    RETURN VIEW(val, INTEGER);
  END UnpackInt;
  
PROCEDURE UnpackInt2 (t: SpinRecvBuf; VAR v1, v2: INTEGER) =
  VAR
    val: ARRAY [0..1] OF Ctypes.int;
  BEGIN
    IF t.curIdx + SizeofInt * 2 > t.bufSize THEN
      v1 := UnpackInt(t);
      v2 := UnpackInt(t);
    ELSE
      val[0] := VIEW(SUBARRAY(t.buf^, t.curIdx, 4), Ctypes.int);
      val[1] := VIEW(SUBARRAY(t.buf^, t.curIdx+4, 4), Ctypes.int);
      v1 := VIEW(val, INTEGER);
      val[0] := VIEW(SUBARRAY(t.buf^, t.curIdx+8, 4), Ctypes.int);
      val[1] := VIEW(SUBARRAY(t.buf^, t.curIdx+12, 4), Ctypes.int);
      v2 := VIEW(val, INTEGER);
      INC(t.curIdx, SizeofInt*2);
      INC(t.cumulativeIdx, SizeofInt*2);
      <*ASSERT t.cumulativeIdx <= t.maxSize*>
    END;
  END UnpackInt2;
  
PROCEDURE UnpackInt3 (t: SpinRecvBuf; VAR v1, v2, v3: INTEGER) =
  VAR
    val: ARRAY [0..1] OF Ctypes.int;
  BEGIN
    IF t.curIdx + SizeofInt * 3 > t.bufSize THEN
      v1 := UnpackInt(t);
      v2 := UnpackInt(t);
      v3 := UnpackInt(t);
    ELSE
      val[0] := VIEW(SUBARRAY(t.buf^, t.curIdx, 4), Ctypes.int);
      val[1] := VIEW(SUBARRAY(t.buf^, t.curIdx+4, 4), Ctypes.int);
      v1 := VIEW(val, INTEGER);
      val[0] := VIEW(SUBARRAY(t.buf^, t.curIdx+SizeofInt, 4),
		     Ctypes.int);
      val[1] := VIEW(SUBARRAY(t.buf^, t.curIdx+SizeofInt+4, 4),
		     Ctypes.int);
      v2 := VIEW(val, INTEGER);
      val[0] := VIEW(SUBARRAY(t.buf^, t.curIdx+SizeofInt*2, 4),
		     Ctypes.int);
      val[1] := VIEW(SUBARRAY(t.buf^, t.curIdx+SizeofInt*2+4, 4),
		     Ctypes.int);
      v3 := VIEW(val, INTEGER);
      INC(t.curIdx, SizeofInt*3);
      INC(t.cumulativeIdx, SizeofInt*3);
      <*ASSERT t.cumulativeIdx <= t.maxSize*>
    END;
  END UnpackInt3;
  
PROCEDURE UnpackInt4 (t: SpinRecvBuf; VAR v1, v2, v3, v4: INTEGER) =
  VAR
    val: ARRAY [0..1] OF Ctypes.int;
  BEGIN
    IF t.curIdx + SizeofInt * 4 > t.bufSize THEN
      v1 := UnpackInt(t);
      v2 := UnpackInt(t);
      v3 := UnpackInt(t);
      v4 := UnpackInt(t);
    ELSE
      val[0] := VIEW(SUBARRAY(t.buf^, t.curIdx, 4), Ctypes.int);
      val[1] := VIEW(SUBARRAY(t.buf^, t.curIdx+4, 4), Ctypes.int);
      v1 := VIEW(val, INTEGER);
      val[0] := VIEW(SUBARRAY(t.buf^, t.curIdx+SizeofInt, 4),
		     Ctypes.int);
      val[1] := VIEW(SUBARRAY(t.buf^, t.curIdx+SizeofInt+4, 4),
		     Ctypes.int);
      v2 := VIEW(val, INTEGER);
      val[0] := VIEW(SUBARRAY(t.buf^, t.curIdx+SizeofInt*2, 4),
		     Ctypes.int);
      val[1] := VIEW(SUBARRAY(t.buf^, t.curIdx+SizeofInt*2+4, 4),
		     Ctypes.int);
      v3 := VIEW(val, INTEGER);
      val[0] := VIEW(SUBARRAY(t.buf^, t.curIdx+SizeofInt*3, 4),
		     Ctypes.int);
      val[1] := VIEW(SUBARRAY(t.buf^, t.curIdx+SizeofInt*3+4, 4),
		     Ctypes.int);
      v4 := VIEW(val, INTEGER);
      INC(t.curIdx, SizeofInt*4);
      INC(t.cumulativeIdx, SizeofInt*4);
      <*ASSERT t.cumulativeIdx <= t.maxSize*>
    END;
  END UnpackInt4;
  
PROCEDURE UnpackHeader (t: SpinRecvBuf; VAR sid: SID.T) =
  VAR
    val: ARRAY [0..1] OF Ctypes.int;
  BEGIN
    IF t.curIdx + SizeofInt * 2 > t.bufSize THEN
      sid.hid := UnpackInt(t);
      sid.lid := UnpackInt(t);
    ELSE
      val[0] := VIEW(SUBARRAY(t.buf^, t.curIdx, 4), Ctypes.int);
      val[1] := VIEW(SUBARRAY(t.buf^, t.curIdx+4, 4), Ctypes.int);
      sid.hid := VIEW(val, INTEGER);
      val[0] := VIEW(SUBARRAY(t.buf^, t.curIdx+SizeofInt, 4),
		     Ctypes.int);
      val[1] := VIEW(SUBARRAY(t.buf^, t.curIdx+SizeofInt+4, 4),
		     Ctypes.int);
      sid.lid := VIEW(val, INTEGER);
      INC(t.curIdx, SizeofInt*2);
      INC(t.cumulativeIdx, SizeofInt*2);
      <*ASSERT t.cumulativeIdx <= t.maxSize*>
    END;
  END UnpackHeader;
  
PROCEDURE UnpackHeader2 (t: SpinRecvBuf; VAR sid: SID.T; VAR tid: TID.T) =
  VAR
    val: ARRAY [0..1] OF Ctypes.int;
  BEGIN
    IF t.curIdx + SizeofInt * 3 > t.bufSize THEN
      sid.hid := UnpackInt(t);
      sid.lid := UnpackInt(t);
      tid := UnpackInt(t);
    ELSE
      val[0] := VIEW(SUBARRAY(t.buf^, t.curIdx, 4), Ctypes.int);
      val[1] := VIEW(SUBARRAY(t.buf^, t.curIdx+4, 4), Ctypes.int);
      sid.hid := VIEW(val, INTEGER);
      val[0] := VIEW(SUBARRAY(t.buf^, t.curIdx+SizeofInt, 4),
		     Ctypes.int);
      val[1] := VIEW(SUBARRAY(t.buf^, t.curIdx+SizeofInt+4, 4),
		     Ctypes.int);
      sid.lid := VIEW(val, INTEGER);
      val[0] := VIEW(SUBARRAY(t.buf^, t.curIdx+SizeofInt*2, 4),
		     Ctypes.int);
      val[1] := VIEW(SUBARRAY(t.buf^, t.curIdx+SizeofInt*2+4, 4),
		     Ctypes.int);
      tid := VIEW(val, INTEGER);
      INC(t.curIdx, SizeofInt*3);
      INC(t.cumulativeIdx, SizeofInt*3);
      <*ASSERT t.cumulativeIdx <= t.maxSize*>
    END;
  END UnpackHeader2;
  
PROCEDURE UnpackHeader3 (t: SpinRecvBuf; VAR sid: SID.T; VAR tid: TID.T;
			 VAR v: INTEGER) =
  VAR
    val: ARRAY [0..1] OF Ctypes.int;
  BEGIN
    IF t.curIdx + SizeofInt * 4 > t.bufSize THEN
      sid.hid := UnpackInt(t);
      sid.lid := UnpackInt(t);
      tid := UnpackInt(t);
      v := UnpackInt(t);
    ELSE
      val[0] := VIEW(SUBARRAY(t.buf^, t.curIdx, 4), Ctypes.int);
      val[1] := VIEW(SUBARRAY(t.buf^, t.curIdx+4, 4), Ctypes.int);
      sid.hid := VIEW(val, INTEGER);
      val[0] := VIEW(SUBARRAY(t.buf^, t.curIdx+SizeofInt, 4),
		     Ctypes.int);
      val[1] := VIEW(SUBARRAY(t.buf^, t.curIdx+SizeofInt+4, 4),
		     Ctypes.int);
      sid.lid := VIEW(val, INTEGER);
      val[0] := VIEW(SUBARRAY(t.buf^, t.curIdx+SizeofInt*2, 4),
		     Ctypes.int);
      val[1] := VIEW(SUBARRAY(t.buf^, t.curIdx+SizeofInt*2+4, 4),
		     Ctypes.int);
      tid := VIEW(val, INTEGER);
      val[0] := VIEW(SUBARRAY(t.buf^, t.curIdx+SizeofInt*3, 4),
		     Ctypes.int);
      val[1] := VIEW(SUBARRAY(t.buf^, t.curIdx+SizeofInt*3+4, 4),
		     Ctypes.int);
      v := VIEW(val, INTEGER);
      INC(t.curIdx, SizeofInt*4);
      INC(t.cumulativeIdx, SizeofInt*4);
      <*ASSERT t.cumulativeIdx <= t.maxSize*>
    END;
  END UnpackHeader3;
  
PROCEDURE UnpackHeader4 (t: SpinRecvBuf; VAR sid: SID.T; VAR tid: TID.T;
			 VAR v1, v2: INTEGER) =
  VAR
    val: ARRAY [0..1] OF Ctypes.int;
  BEGIN
    IF t.curIdx + SizeofInt * 5 > t.bufSize THEN
      sid.hid := UnpackInt(t);
      sid.lid := UnpackInt(t);
      tid := UnpackInt(t);
      v1 := UnpackInt(t);
      v2 := UnpackInt(t);
    ELSE
      val[0] := VIEW(SUBARRAY(t.buf^, t.curIdx, 4), Ctypes.int);
      val[1] := VIEW(SUBARRAY(t.buf^, t.curIdx+4, 4), Ctypes.int);
      sid.hid := VIEW(val, INTEGER);
      val[0] := VIEW(SUBARRAY(t.buf^, t.curIdx+SizeofInt, 4),
		     Ctypes.int);
      val[1] := VIEW(SUBARRAY(t.buf^, t.curIdx+SizeofInt+4, 4),
		     Ctypes.int);
      sid.lid := VIEW(val, INTEGER);
      val[0] := VIEW(SUBARRAY(t.buf^, t.curIdx+SizeofInt*2, 4),
		     Ctypes.int);
      val[1] := VIEW(SUBARRAY(t.buf^, t.curIdx+SizeofInt*2+4, 4),
		     Ctypes.int);
      tid := VIEW(val, INTEGER);
      val[0] := VIEW(SUBARRAY(t.buf^, t.curIdx+SizeofInt*3, 4),
		     Ctypes.int);
      val[1] := VIEW(SUBARRAY(t.buf^, t.curIdx+SizeofInt*3+4, 4),
		     Ctypes.int);
      v1 := VIEW(val, INTEGER);
      val[0] := VIEW(SUBARRAY(t.buf^, t.curIdx+SizeofInt*4, 4),
		     Ctypes.int);
      val[1] := VIEW(SUBARRAY(t.buf^, t.curIdx+SizeofInt*4+4, 4),
		     Ctypes.int);
      v2 := VIEW(val, INTEGER);
      INC(t.curIdx, SizeofInt*5);
      INC(t.cumulativeIdx, SizeofInt*5);
      <*ASSERT t.cumulativeIdx <= t.maxSize*>
    END;
  END UnpackHeader4;

PROCEDURE UnpackBool (t: SpinRecvBuf): BOOLEAN =
  VAR val: INTEGER;
  BEGIN
    IF t.curIdx + SizeofInt > t.bufSize THEN
      val := UnpackInt(t);
    ELSE
      val := VIEW(SUBARRAY(t.buf^, t.curIdx, SizeofInt), INTEGER);
      INC(t.curIdx, SizeofInt);
      INC(t.cumulativeIdx, SizeofInt);
      <*ASSERT t.cumulativeIdx <= t.maxSize*>
    END;
    <*ASSERT val = -999 OR val = 999*>
    RETURN val = 999;
  END UnpackBool;

PROCEDURE UnpackText (t: SpinRecvBuf): TEXT =
  VAR 
    len: INTEGER;
    r: TEXT;
  BEGIN
    (* XXX we don't handle the case where the text spans multiple mbufs. *)
    len := UnpackInt(t);
    <*ASSERT len < 128*>
    r := Text.FromChars(SUBARRAY(t.buf^, t.curIdx, len));
    INC(t.curIdx, Round.Up8(len));
    <*ASSERT t.curIdx <= t.maxSize*>
    RETURN r;
  END UnpackText;

PROCEDURE UnpackArray (t: SpinRecvBuf; VAR x: ARRAY OF CHAR): CARDINAL =
  VAR 
    len: CARDINAL;
    remaining, idx: CARDINAL;
  BEGIN
    len := UnpackInt(t);
    <*ASSERT len <= NUMBER(x)*>
    remaining := len;
    idx := 0;

    WHILE remaining > 0 DO
      IF t.bufSize-t.curIdx < remaining THEN
	WITH copyLen = t.bufSize-t.curIdx DO 
	  SUBARRAY(x, idx, copyLen) := SUBARRAY(t.buf^, t.curIdx, copyLen);
	  INC(idx, copyLen);
	  INC(t.cumulativeIdx, copyLen);
	  DEC(remaining, copyLen);
	  GetNextMbuf(t);	  (* Follow the mbuf chain. *)
	END;
      ELSE
	SUBARRAY(x, idx, remaining) := SUBARRAY(t.buf^, t.curIdx, remaining);
	WITH xlen = Round.Up8(len) DO 
	  INC(t.curIdx, xlen);
	  INC(t.cumulativeIdx, xlen);
	END;
	EXIT;
      END;
    END;
    RETURN len;
  END UnpackArray;
  
PROCEDURE EndUnpack(t: SpinRecvBuf) =
  BEGIN
    IF Debug THEN
      IF t.cumulativeIdx # t.maxSize THEN
	Msg("endunpack: message not consumed.\n");
	Debugger.Enter();
      END;
    END;
    Mbuf.m_freem(t.mbuf);
    t.mbuf := NIL;
  END EndUnpack;

BEGIN
END MachineRPC.

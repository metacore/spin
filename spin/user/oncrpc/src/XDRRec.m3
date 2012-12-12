(*
   XDRRec.m3
   XDR with record marking on readers and writers.
   David Nichols, Xerox PARC
   July, 1991

   $Id: XDRRec.m3,v 1.2 1996/03/06 01:33:00 mef Exp $
 *)

(* Copyright (c) 1991, 1992 Xerox Corporation.  All rights reserved.

   Use and copying of this software and preparation of derivative works
   based upon this software are permitted.  Any distribution of this
   software or derivative works must comply with all applicable United
   States export control laws.  This software is made available AS IS, and
   Xerox Corporation makes no warranty about the software, its performance
   or its conformity to any specification. *)

MODULE XDRRec;

(* XDR sources and sinks aimed at streams and using the XDR record-marking
   protocol. *)

(* Unix Emulation *)
IMPORT Utypes;

(* M3 LIBS *)
IMPORT Rd;
IMPORT Thread;
IMPORT Word;
IMPORT Wr;

(* ONC RPC *)
IMPORT XDR;
IMPORT XDRMem;

(*
 * Sources.
 *)

REVEAL
  (* For reading, we just keep track of the number of bytes left in the
     current segment. *)
  Source = XDR.Source BRANDED OBJECT
             rd       : Rd.T;
             bytesLeft: INTEGER;  (* bytes left in this seg *)
             lastSeg  : BOOLEAN;  (* current seg is last one *)
           OVERRIDES
             GetWord32  := GetWord32;
             GetBytes := GetBytes;
           END;

PROCEDURE NewSource (rd: Rd.T): Source =
  BEGIN
    RETURN NEW(Source, rd := rd, bytesLeft := 0, lastSeg := FALSE);
  END NewSource;

(* Zap all data upto the next record boundary.  XDR.Get* calls will fail
   when a source reaches a record boundary until the next call to
   NextRecord. *)
PROCEDURE NextRecord (s: Source) RAISES {XDR.Failed, Thread.Alerted} =
  VAR
    n: INTEGER;
    a: ARRAY [0 .. 99] OF CHAR;
  BEGIN
    TRY
      WHILE NOT s.lastSeg OR s.bytesLeft > 0 DO
        WHILE s.bytesLeft > 0 DO
          n :=
            Rd.GetSub(s.rd, SUBARRAY(a, 0, MIN(NUMBER(a), s.bytesLeft)));
          DEC(s.bytesLeft, n);
        END;
        IF NOT s.lastSeg THEN ReadNextSeg(s); END;
      END;
      s.lastSeg := FALSE;
    EXCEPT
      Rd.EndOfFile, Rd.Failure =>
        RAISE XDR.Failed(
                NEW(XDR.Failure, info := "Error reading from stream"));
    END;
  END NextRecord;

(* Read next segment header and set bytes left in seg. *)
PROCEDURE ReadNextSeg (s: Source)
  RAISES {Rd.Failure, Rd.EndOfFile, Thread.Alerted} =
  VAR a: ARRAY [0 .. 3] OF CHAR;
  BEGIN
    IF Rd.GetSub(s.rd, a) < 4 THEN RAISE Rd.EndOfFile; END;
    s.lastSeg := Word.Extract(ORD(a[0]), 7, 1) = 1;
    s.bytesLeft :=
      Word.Shift(Word.Extract(ORD(a[0]), 0, 7), 24)
        + Word.Shift(ORD(a[1]), 16) + Word.Shift(ORD(a[2]), 8) + ORD(a[3]);
  END ReadNextSeg;

PROCEDURE GetWord32 (s: Source): Utypes.u_int
  RAISES {XDR.Failed, Thread.Alerted} =
  VAR a: ARRAY [0 .. 3] OF CHAR;
  BEGIN
    GetBytes(s, a);
    RETURN Word.Shift(ORD(a[0]), 24) + Word.Shift(ORD(a[1]), 16)
             + Word.Shift(ORD(a[2]), 8) + ORD(a[3]);
  END GetWord32;

PROCEDURE GetBytes (s: Source; VAR v: ARRAY OF CHAR)
  RAISES {XDR.Failed, Thread.Alerted} =
  VAR
    start          := 0;
    n    : INTEGER;
  BEGIN
    TRY
      WHILE start < NUMBER(v) DO
        WHILE s.bytesLeft <= 0 DO
          IF s.lastSeg THEN
            RAISE
              XDR.Failed(NEW(XDR.Failure,
                             info := "End of segment on XDRRec.Source"));
          END;
          ReadNextSeg(s);
        END;
        n := MIN(s.bytesLeft, NUMBER(v) - start);
        n := Rd.GetSub(s.rd, SUBARRAY(v, start, n));
        INC(start, n);
	DEC(s.bytesLeft, n);
      END;
    EXCEPT
      Rd.Failure, Rd.EndOfFile =>
        RAISE XDR.Failed(
                NEW(XDR.Failure, info := "Rd.T failure on XDRRec.Source"));
    END;
  END GetBytes;

(*
 * Sinks.
 *)

REVEAL
  (* For writing, we write segs into an XDRMem.Sink, then prefix them with
     the header and send it. *)
  Sink = XDR.Sink BRANDED OBJECT
           wr : Wr.T;
           buf: REF ARRAY OF CHAR;
           mem: XDRMem.Sink;
         OVERRIDES
           PutWord32  := PutWord32;
           PutBytes := PutBytes;
         END;
CONST BufLen = 1024;

PROCEDURE NewSink (wr: Wr.T): Sink =
  BEGIN
    WITH buf = NEW(REF ARRAY OF CHAR, BufLen) DO
      RETURN NEW(Sink, wr := wr, mem := XDRMem.NewSink(buf), buf := buf);
    END;
  END NewSink;

PROCEDURE SendSeg (s: Sink; endOfRecord: BOOLEAN)
  RAISES {XDR.Failed, Thread.Alerted} =
  VAR
    a: ARRAY [0 .. 3] OF CHAR;
    n                         := XDRMem.GetSinkPos(s.mem);
  BEGIN
    IF endOfRecord THEN
      a[0] := VAL(Word.Extract(n, 24, 8) + 128, CHAR);
    ELSE
      a[0] := VAL(Word.Extract(n, 24, 8), CHAR);
    END;
    a[1] := VAL(Word.Extract(n, 16, 8), CHAR);
    a[2] := VAL(Word.Extract(n, 8, 8), CHAR);
    a[3] := VAL(Word.Extract(n, 0, 8), CHAR);
    TRY
      Wr.PutString(s.wr, a);
      Wr.PutString(s.wr, SUBARRAY(s.buf^, 0, n));
      Wr.Flush(s.wr);
    EXCEPT
      Wr.Failure =>
        RAISE XDR.Failed(
                NEW(XDR.Failure, info := "Error writing TCP connection"));
    END;
    XDRMem.SetSinkPos(s.mem, 0);
  END SendSeg;

(* Place a record boundary at the current point and flush the underlying
   writer. *)
PROCEDURE NewRecord (s: Sink) RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    SendSeg(s, endOfRecord := TRUE);
  END NewRecord;

PROCEDURE PutWord32 (s: Sink; v: INTEGER)
  RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    IF XDRMem.GetSinkPos(s.mem) + 4 > NUMBER(s.buf^) THEN
      SendSeg(s, endOfRecord := FALSE);
    END;
    s.mem.PutWord32(v);
  END PutWord32;

PROCEDURE PutBytes (s: Sink; READONLY v: ARRAY OF CHAR)
  RAISES {XDR.Failed, Thread.Alerted} =
  VAR
    space: INTEGER;
    n    : INTEGER;
    start          := 0;
  BEGIN
    LOOP
      space := NUMBER(s.buf^) - XDRMem.GetSinkPos(s.mem);
      IF space > 0 THEN
        n := MIN(space, NUMBER(v) - start);
        s.mem.PutBytes(SUBARRAY(v, start, n));
        INC(start, n)
      END;
      IF start < NUMBER(v) THEN
        SendSeg(s, endOfRecord := FALSE);
      ELSE
        EXIT;
      END;
    END;
  END PutBytes;

BEGIN
END XDRRec.

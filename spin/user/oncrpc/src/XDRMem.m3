(*
   XDRMem.m3
   XDR on a simple memory buffer.
   David Nichols, Xerox PARC
   February, 1992

   Copyright (c) 1992 Xerox Corporation.  All rights reserved.

   Use and copying of this software and preparation of derivative works
   based upon this software are permitted.  Any distribution of this
   software or derivative works must comply with all applicable United
   States export control laws.  This software is made available AS IS, and
   Xerox Corporation makes no warranty about the software, its performance
   or its conformity to any specification. *)

(* 
 * HISTORY 
 * 13-Jun-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.  Fixed GetText() routine.
 *
 *)

MODULE XDRMem;

(* M3 LIBS *)
IMPORT Thread;
IMPORT Word;
IMPORT Text;

(* ONC RPC *)
IMPORT XDR, RPCOS;

REVEAL
  Source = XDR.Source BRANDED OBJECT
             buf: REF ARRAY OF CHAR;
             pos: INTEGER;
             len: INTEGER;
           OVERRIDES
             GetWord32 := GetWord32;
             GetBytes  := GetBytes;
             GetText   := GetText;
           END;
(*
 * Sources.
 *)

PROCEDURE NewSource (buf: REF ARRAY OF CHAR): Source =
  BEGIN
    RETURN NEW(Source, buf := buf, pos := 0, len := NUMBER(buf^));
  END NewSource;

PROCEDURE GetWord32 (s: Source): RPCOS.UINT32
  RAISES {XDR.Failed, Thread.Alerted} =
  VAR v: RPCOS.UINT32;
  BEGIN
    IF s.pos + 4 > s.len THEN
      RAISE
        XDR.Failed(
          NEW(XDR.Failure, info := "Not enough data in XDRMem.Source"));
    END;
    v := Word.Shift(ORD(s.buf[s.pos]), 24)
           + Word.Shift(ORD(s.buf[s.pos + 1]), 16)
           + Word.Shift(ORD(s.buf[s.pos + 2]), 8) + ORD(s.buf[s.pos + 3]);
    INC(s.pos, 4);
    RETURN v;
  END GetWord32;

(* Get n bytes and realign. *)
PROCEDURE GetBytes (s: Source; VAR v: ARRAY OF CHAR)
  RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    IF s.pos + NUMBER(v) > s.len THEN
      RAISE
        XDR.Failed(
          NEW(XDR.Failure, info := "Not enough data in XDRMem.Source"));
    END;
    v := SUBARRAY(s.buf^, s.pos, NUMBER(v));
    INC(s.pos, NUMBER(v));
  END GetBytes;

PROCEDURE GetText (s: Source; len: CARDINAL) : TEXT RAISES {XDR.Failed, Thread.Alerted} =
  VAR t:TEXT;
  BEGIN
    IF s.pos + len > s.len THEN
      RAISE
        XDR.Failed(
          NEW(XDR.Failure, info := "Not enough data in XDRMem.Source"));
    END;
    t := Text.FromChars(SUBARRAY(s.buf^, s.pos, len));
    INC(s.pos,len);
    RETURN t;
  END GetText;


(* Set the starting point for fetching data. *)
PROCEDURE SetSourcePos (s: Source; pos: INTEGER) RAISES {XDR.Failed} =
  BEGIN
    IF pos > s.len THEN
      RAISE XDR.Failed(
              NEW(XDR.Failure, info := "Bad pos for XDRMem.SetSourcePos"));
    END;
    s.pos := pos;
  END SetSourcePos;

(* Set the bounds for fetching data.  Default is length of the buffer. *)
PROCEDURE SetSourceLen (s: Source; len: INTEGER) RAISES {XDR.Failed} =
  BEGIN
    IF len > NUMBER(s.buf^) THEN
      RAISE XDR.Failed(NEW(XDR.Failure,
                           info := "Bad length for XDRMem.SetSourceLen"));
    END;
    s.len := len;
  END SetSourceLen;


(*
 * Sinks.
 *)

REVEAL
  Sink = XDR.Sink BRANDED OBJECT
           buf: REF ARRAY OF CHAR;
           pos: INTEGER;
         OVERRIDES
           PutWord32  := PutWord32;
           PutBytes := PutBytes;
           PutText := PutText;
         END;


PROCEDURE NewSink (buf: REF ARRAY OF CHAR): Sink =
  BEGIN
    RETURN NEW(Sink, buf := buf, pos := 0);
  END NewSink;

PROCEDURE PutWord32 (s: Sink; v: INTEGER)
  RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    IF s.pos + 4 > NUMBER(s.buf^) THEN
      RAISE XDR.Failed(
              NEW(XDR.Failure, info := "Not enough space in XDRMem.Sink"));
    END;
    s.buf[s.pos] := VAL(Word.Extract(v, 24, 8), CHAR);
    s.buf[s.pos + 1] := VAL(Word.Extract(v, 16, 8), CHAR);
    s.buf[s.pos + 2] := VAL(Word.Extract(v, 8, 8), CHAR);
    s.buf[s.pos + 3] := VAL(Word.Extract(v, 0, 8), CHAR);
    INC(s.pos, 4);
  END PutWord32;

PROCEDURE PutBytes (s: Sink; READONLY v: ARRAY OF CHAR)
  RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    IF s.pos + NUMBER(v) > NUMBER(s.buf^) THEN
      RAISE XDR.Failed(
              NEW(XDR.Failure, info := "Not enough space in XDRMem.Sink"));
    END;
    SUBARRAY(s.buf^, s.pos, NUMBER(v)) := v;
    INC(s.pos, NUMBER(v));
  END PutBytes;

PROCEDURE PutText (s: Sink; READONLY t: TEXT; len: CARDINAL) 
  RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    IF s.pos + len > NUMBER(s.buf^) THEN
      RAISE XDR.Failed(
              NEW(XDR.Failure, info := "Not enough space in XDRMem.Sink"));
    END;
    Text.SetChars(SUBARRAY(s.buf^,s.pos,len),t);
    INC(s.pos, len);
  END PutText;

(* Set the place to start putting data. *)
PROCEDURE SetSinkPos (s: Sink; pos: INTEGER) RAISES {XDR.Failed} =
  BEGIN
    IF pos > NUMBER(s.buf^) THEN
      RAISE XDR.Failed(
              NEW(XDR.Failure, info := "Bad pos for XDRMem.SetSinkPos"));
    END;
    s.pos := pos;
  END SetSinkPos;

(* Get the current spot. *)
PROCEDURE GetSinkPos (s: Sink): INTEGER =
  BEGIN
    RETURN s.pos;
  END GetSinkPos;

BEGIN
END XDRMem.

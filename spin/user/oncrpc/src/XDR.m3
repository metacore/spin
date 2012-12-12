(*
   XDR.m3
   Marshalling routines for Sun RPC.
   David Nichols, Xerox PARC
   July, 1991

   $Id: XDR.m3,v 1.3 1996/04/15 03:21:21 mef Exp $
*)

(* HISTORY
 * 08-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Took out support for real.
 *
 *
*)
(* Copyright (c) 1991, 1992 Xerox Corporation.  All rights reserved.

   Use and copying of this software and preparation of derivative works
   based upon this software are permitted.  Any distribution of this
   software or derivative works must comply with all applicable United
   States export control laws.  This software is made available AS IS, and
   Xerox Corporation makes no warranty about the software, its performance
   or its conformity to any specification. *)

MODULE XDR;
IMPORT RPCOS;

(* M3 LIBS *)
IMPORT Text;
IMPORT Thread;
IMPORT Word;
(* IMPORT FloatMode, XDRFloat; *)

(*
 * Sources
 *)

PROCEDURE GetInteger (s: Source): RPCOS.UINT32 RAISES {Failed, Thread.Alerted} =
  BEGIN
    RETURN s.GetWord32();
  END GetInteger;

PROCEDURE GetShort (s: Source): RPCOS.UINT16 RAISES {Failed, Thread.Alerted} =
  BEGIN
    RETURN VAL(Word.Extract(s.GetWord32(), 0, 16), RPCOS.UINT16);
  END GetShort;

PROCEDURE GetChar (s: Source): CHAR RAISES {Failed, Thread.Alerted} =
  BEGIN
    RETURN VAL(Word.Extract(s.GetWord32(), 0, 8), CHAR);
  END GetChar;

PROCEDURE GetBoolean (s: Source): BOOLEAN RAISES {Failed, Thread.Alerted} =
  BEGIN
    RETURN s.GetWord32() # 0;
  END GetBoolean;

(*
PROCEDURE GetReal (s: Source): REAL RAISES {Failed, Thread.Alerted} =
  VAR i := s.GetLong();
  BEGIN
    TRY
      RETURN XDRFloat.WordToReal(i);
    EXCEPT
      FloatMode.Trap (f) =>
        RAISE Failed(
                NEW(FloatConversionFailure,
                    info := "Floating point unmarshalling failed.",
                    flag := f));
    END;
  END GetReal;

PROCEDURE GetLongReal (s: Source): LONGREAL
  RAISES {Failed, Thread.Alerted} =
  VAR high, low: INTEGER;
  BEGIN
    high := s.GetLong();
    low := s.GetLong();
    TRY
      RETURN XDRFloat.WordToLongReal(high, low);
    EXCEPT
      FloatMode.Trap (f) =>
        RAISE Failed(
                NEW(FloatConversionFailure,
                    info := "Floating point unmarshalling failed.",
                    flag := f));
    END;
  END GetLongReal;

*)

PROCEDURE GetText (s: Source): TEXT RAISES {Failed, Thread.Alerted} =
  VAR
    len    : INTEGER;        (* length of the string *)
    t         : TEXT    := NIL; (* the result *)
    alignBytes: INTEGER;        (* number of alignment bytes *)
    buf: ARRAY [0 .. 3] OF CHAR;
  BEGIN
    len := s.GetWord32();
    alignBytes := 3 - (len + 3) MOD 4;

    t := s.GetText(len);
    (* Realign. *)
    s.GetBytes(SUBARRAY(buf, 0, alignBytes));
    RETURN t;
  END GetText;

(* Get n bytes and realign. *)
PROCEDURE GetBytes (s: Source; VAR v: ARRAY OF CHAR)
  RAISES {Failed, Thread.Alerted} =
  VAR
    alignBytes                         := 3 - (NUMBER(v) + 3) MOD 4;
    buf       : ARRAY [0 .. 3] OF CHAR;
  BEGIN
    s.GetBytes(v);
    s.GetBytes(SUBARRAY(buf, 0, alignBytes));
  END GetBytes;

(*
 * Sinks
 *)

PROCEDURE PutInteger (s: Sink; v: RPCOS.UINT32)
  RAISES {Failed, Thread.Alerted} =
  BEGIN
    s.PutWord32(v);
  END PutInteger;

PROCEDURE PutShort (s: Sink; v: RPCOS.UINT16) RAISES {Failed, Thread.Alerted} =
  BEGIN
    s.PutWord32(v);
  END PutShort;

PROCEDURE PutChar (s: Sink; v: CHAR) RAISES {Failed, Thread.Alerted} =
  BEGIN
    s.PutWord32(ORD(v));
  END PutChar;

PROCEDURE PutBoolean (s: Sink; v: BOOLEAN)
  RAISES {Failed, Thread.Alerted} =
  BEGIN
    IF v THEN s.PutWord32(1); ELSE s.PutWord32(0); END;
  END PutBoolean;

(*
PROCEDURE PutReal (s: Sink; v: REAL) RAISES {Failed, Thread.Alerted} =
  BEGIN
    s.PutLong(XDRFloat.RealToWord(v));
  END PutReal;

PROCEDURE PutLongReal (s: Sink; v: LONGREAL)
  RAISES {Failed, Thread.Alerted} =
  VAR high, low: INTEGER;
  BEGIN
    XDRFloat.LongRealToWord(v, high, low);
    s.PutLong(high);
    s.PutLong(low);
  END PutLongReal;
*)

PROCEDURE PutText (s: Sink; v: TEXT) RAISES {Failed, Thread.Alerted} =
  VAR
    len   := Text.Length(v);
    alignBytes                         := 3 - (len + 3) MOD 4;
    buf       : ARRAY [0 .. 3] OF CHAR;
  BEGIN
    s.PutWord32(len);
    s.PutText(v,len);
    s.PutBytes(SUBARRAY(buf, 0, alignBytes));
  END PutText;

PROCEDURE PutBytes (s: Sink; READONLY v: ARRAY OF CHAR)
  RAISES {Failed, Thread.Alerted} =
  VAR
    alignBytes                         := 3 - (NUMBER(v) + 3) MOD 4;
    buf       : ARRAY [0 .. 3] OF CHAR;
  BEGIN
    s.PutBytes(v);
    s.PutBytes(SUBARRAY(buf, 0, alignBytes));
  END PutBytes;

BEGIN
END XDR.

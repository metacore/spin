(*
   XDRFloat.i3
   Convert between REAL, LONGREAL and IEEE (XDR) representation.
   David Goldberg, Xerox PARC
   March, 1992

   $Id: XDRFloat.i3,v 1.1 1996/02/09 18:08:19 mef Exp $
*)

(* Copyright (c) 1992 Xerox Corporation.  All rights reserved.

   Use and copying of this software and preparation of derivative works
   based upon this software are permitted.  Any distribution of this
   software or derivative works must comply with all applicable United
   States export control laws.  This software is made available AS IS, and
   Xerox Corporation makes no warranty about the software, its performance
   or its conformity to any specification. *)

INTERFACE XDRFloat;

IMPORT FloatMode, Word;

(* Sign is 1 bit, exp is 8 bits, significand is 23 bits. *)

PROCEDURE WordToReal (word: Word.T): REAL RAISES {FloatMode.Trap};

PROCEDURE RealToWord (x: REAL): Word.T;


(* Sign is 1 bit, exp is 11 bits (both in wordHigh), significand is 52
   bits. *)

PROCEDURE WordToLongReal (wordHigh, wordLow: Word.T): LONGREAL
  RAISES {FloatMode.Trap};

PROCEDURE LongRealToWord (            x                : LONGREAL;
                          VAR (*out*) wordHigh, wordLow: Word.T    );

END XDRFloat.

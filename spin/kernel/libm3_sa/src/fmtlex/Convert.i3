(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Fri Feb 18 13:17:07 PST 1994 by kalsow    *)
(*      modified on Fri Jan  5 06:59:24 1990 by muller        *)

INTERFACE Convert;

(* Binary/ASCII conversions of numbers.

   This interface provides binary/ASCII conversions for
   INTEGERs

   Index: conversion;  numbers;  ASCII *)

TYPE
  Buffer = ARRAY OF CHAR;
  Base   = [2..16];

EXCEPTION Failed;


(*---- Binary to ASCII conversions ----*)

(* The "From" procedures convert binary values to ASCII character strings.
   Each procedure returns the number characters that resulted.  Extra
   space in the buffers is left unmodified.  Failed is raised if the
   supplied buffer is too small to hold the result. *)

PROCEDURE FromInt (VAR buf    : Buffer;
                       value  : INTEGER;
                       base   : Base := 10;
                       prefix : BOOLEAN := FALSE): INTEGER RAISES {Failed};
(* converts value to ASCII in the specified base and stores the result in buf.
   If prefix=TRUE, include the base prefix in the result. *)

PROCEDURE FromUnsigned (VAR buf    : Buffer;
                            value  : INTEGER;
                            base   : Base := 10;
                            prefix : BOOLEAN := FALSE): INTEGER RAISES{Failed};
(* treats value as an unsigned 32-bit number, converts it to ASCII in the
   specified base and stores the result in buf.  If prefix=TRUE, include
   the base prefix in the result. *)

(* The "To" procedures convert ASCII character strings to their
   corresponding binary representations.  The procedures convert
   the maximum number of characters possible.  The number of characters
   actually used is returned in 'used'.  *)

PROCEDURE ToInt (READONLY buf  : Buffer;
                      VAR used : INTEGER;
                          base : Base := 10): INTEGER   RAISES {};
(* converts an integer.  The characters are interpreted in the specified
   base unless an explicit base prefix is in the number. *)

PROCEDURE ToUnsigned (READONLY buf  : Buffer;
                           VAR used : INTEGER;
                               base : Base := 10): INTEGER  RAISES {};
(* converts an unsigned number. The characters are interpreted in the specified
   base unless an explicit base prefix is in the number. *)

END Convert.

(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Tue Mar 17 12:16:54 PST 1992 by muller         *)
(*      modified on Wed Apr 12 09:23:40 1989 by kalsow         *)

(*
 * HISTORY
 * 24-May-96  Wilson Hsieh (whsieh) at the University of Washington
 *	Word interface is FUNCTIONAL
 *
 * 26-Oct-95  Wilson Hsieh (whsieh) at the University of Washington
 *	make Word interface BOUNDED
 *
 *)

INTERFACE Word;

(* A Word.T w represents a sequence of Word.Size bits
	  w(0), ..., w(Word.Size-1).
   It also represents the unsigned number
	  sum of 2^(i) * w(i) for i in 0, ..., Word.Size-1. *)

TYPE
  T = INTEGER;
      (* encoding is implementation-dependent; e.g., 2's complement. *)

CONST
  Size : INTEGER = BITSIZE (T);                (* implementation-dependent *)

FUNCTIONAL EPHEMERAL PROCEDURE Plus (x, y: T): T;         (* (x + y) MOD 2^[Word.Size] *)
FUNCTIONAL EPHEMERAL PROCEDURE Times (x, y: T): T;        (* (x * y) MOD 2^[Word.Size] *)
FUNCTIONAL EPHEMERAL PROCEDURE Minus (x, y: T): T;        (* (x - y) MOD 2^[Word.Size] *)
FUNCTIONAL EPHEMERAL PROCEDURE Divide (x, y: T): T;       (* x divided by y *)
FUNCTIONAL EPHEMERAL PROCEDURE Mod (x, y: T): T;          (* x MOD y *)
FUNCTIONAL EPHEMERAL PROCEDURE LT (x, y: T): BOOLEAN;     (* x < y *)
FUNCTIONAL EPHEMERAL PROCEDURE LE (x, y: T): BOOLEAN;     (* x <= y *)
FUNCTIONAL EPHEMERAL PROCEDURE GT (x, y: T): BOOLEAN;     (* x > y *)
FUNCTIONAL EPHEMERAL PROCEDURE GE (x, y: T): BOOLEAN;     (* x >= y *)
FUNCTIONAL EPHEMERAL PROCEDURE And (x, y: T): T;          (* Bitwise AND of x and y *)
FUNCTIONAL EPHEMERAL PROCEDURE Or (x, y: T): T;           (* Bitwise OR of x and y *)
FUNCTIONAL EPHEMERAL PROCEDURE Xor (x, y: T): T;          (* Bitwise XOR of x and y *)
FUNCTIONAL EPHEMERAL PROCEDURE Not (x: T): T;             (* Bitwise complement of x *)

FUNCTIONAL EPHEMERAL PROCEDURE Shift (x: T; n: INTEGER): T;
(* For all i such that both i and i - n are in the range [0 .. Word.Size - 1],
   bit i of the result equals bit i - n of x. The other bits of the result are
   0. Thus, shifting by n > 0 is like multiplying by 2^(n) *)

FUNCTIONAL EPHEMERAL PROCEDURE LeftShift (x: T; n: [0..Size-1]): T;
(* = Shift (x, n) *)

FUNCTIONAL EPHEMERAL PROCEDURE RightShift (x: T; n: [0..Size-1]): T;
(* = Shift (x, -n) *)

FUNCTIONAL EPHEMERAL PROCEDURE Rotate (x: T; n: INTEGER): T;
(* Bit i of the result equals bit (i - n) MOD Word.Size of x. *)

FUNCTIONAL EPHEMERAL PROCEDURE LeftRotate (x: T; n: [0..Size-1]): T;
(* = Rotate (x, n) *)

FUNCTIONAL EPHEMERAL PROCEDURE RightRotate (x: T; n: [0..Size-1]): T;
(* = Rotate (x, -n) *)
 
FUNCTIONAL EPHEMERAL PROCEDURE Extract (x: T; i, n: CARDINAL): T;
(* Take n bits from x, with bit i as the least significant bit, and return them
   as the least significant n bits of a word whose other bits are 0. A checked
   runtime error if n + i > Word.Size. *)

FUNCTIONAL EPHEMERAL PROCEDURE Insert (x, y: T; i, n: CARDINAL): T;
(* Return x with n bits replaced, with bit i as the least significant bit, by
   the least significant n bits of y. The other bits of x are unchanged. A
   checked runtime error if n + i > Word.Size. *)

END Word.

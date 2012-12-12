(* Copyright (C) 1994, Digital Equipment Corporation.       *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Fri Nov 18 15:37:18 PST 1994 by kalsow  *)
(*      modified on Sat Oct  6 02:01:36 1990 by muller      *)

(*
 *
 * HISTORY					            
 * 14-Oct-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added PutBoolean.
 *
 * 25-Apr-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added PutRef.
 *
 * 24-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	Added SimplePutter
 *
*)

(* RTIO is a simple low-level interface for I/O used by the
   runtime.  None of its routines do any locking or require
   any memory allocation.  Output is buffered internally
   until "Flush" is called or the internal buffer overflows.

   Clients beware, this interface may change at any time.
*)

INTERFACE RTIO;

IMPORT Word;

PROCEDURE PutChar ( c: CHAR);
(* Write "c". *)

PROCEDURE PutBoolean ( b: BOOLEAN);
(* Write a boolean b. *)

PROCEDURE PutChars ( a: ADDRESS;  n: INTEGER);
(* Write characters "a[0..n-1]". *)

PROCEDURE PutString (s: ADDRESS);
(* Write the null terminated string beginning at "s". *)

PROCEDURE PutText ( t: TEXT);
(* Write text "t" on file "f". *)

PROCEDURE PutInt (i: INTEGER;  width := 0);
(* Convert integer "i" to decimal digits and write it right-justified
   in a field of "width" characters. *)

PROCEDURE PutWord (i: Word.T;  width := 0);
(* Convert unsigned integer "i" to decimal digits and write it
   right-justified in a field of "width" characters. *)

PROCEDURE PutHex (i: INTEGER;  width := 0);
(* Convert unsigned integer "i" to hexidecimal digits with a "0x" prefix
   and write it right-justified in a field of "width" characters. *)

PROCEDURE PutAddr (a: ADDRESS;  width := 0);
(* == PutHex (LOOPHOLE (a, INTEGER), width) *)

PROCEDURE PutRef (a: REFANY;  width := 0);
(* == PutHex (LOOPHOLE (a, INTEGER), width) *)

PROCEDURE Flush ();
(* Flush any buffered characters to the operating system. *)




(* A SimplePutter is an object that clients can pass around if they
 * want to redirect trivial output.  *)

TYPE SimplePutter = OBJECT
  METHODS
       putChar(c: CHAR) := PutChar2;
       putBoolean(b: BOOLEAN) := PutBoolean2;
       putChars(a: ADDRESS; n: INTEGER) := PutChars2;
       putString(s: ADDRESS) := PutString2;
       putText(t: TEXT) := PutText2;
       putInt(i: INTEGER; width := 0) := PutInt2;
       putWord(w: Word.T; width := 0) := PutWord2;
       putHex(i: INTEGER; width := 0) := PutHex2;
       putAddr(a: ADDRESS; width := 0) := PutAddr2;
       putRef(a: REFANY; width := 0) := PutRef2;
       flush() := Flush2;
  END;


PROCEDURE PutChar2(p: SimplePutter; c: CHAR);
PROCEDURE PutBoolean2(p: SimplePutter; b: BOOLEAN);
PROCEDURE PutChars2(p: SimplePutter; a: ADDRESS;  n: INTEGER);
PROCEDURE PutString2(p: SimplePutter;s: ADDRESS);
PROCEDURE PutText2(p: SimplePutter; t: TEXT);
PROCEDURE PutInt2(p: SimplePutter; i: INTEGER;  width := 0);
PROCEDURE PutWord2(p: SimplePutter; w: Word.T;  width := 0);
PROCEDURE PutHex2(p: SimplePutter; i: INTEGER;  width := 0);
PROCEDURE PutAddr2(p: SimplePutter; a: ADDRESS;  width := 0);
PROCEDURE PutRef2(p: SimplePutter; a: REFANY;  width := 0);
PROCEDURE Flush2(p: SimplePutter);



END RTIO.

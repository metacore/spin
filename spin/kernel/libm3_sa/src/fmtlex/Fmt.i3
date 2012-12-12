(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Tue Mar 15 12:56:39 PST 1994 by heydon     *)
(*      modified on Fri Feb 18 13:12:30 PST 1994 by kalsow     *)
(*      modified on Tue Nov  9 08:37:38 PST 1993 by mcjones    *)
(*      modified on Thu Apr 29 16:32:36 PDT 1993 by muller     *)
(*      modified on Mon Feb 15 15:18:41 PST 1993 by ramshaw    *)

(*
 * HISTORY
 * 12-Jun-97  Wilson Hsieh (whsieh) at the University of Washington
 *	add Addr and Ref
 *
 *)

INTERFACE Fmt;

IMPORT Word;

PROCEDURE Bool(b: BOOLEAN): TEXT;
(* Format "b" as {\tt \char'42TRUE\char'42} or  {\tt \char'42FALSE\char'42}. *)

PROCEDURE Char(c: CHAR): TEXT;
(* Return a text containing the character "c". *)

TYPE Base = [2..16];

PROCEDURE Int(n: INTEGER; base: Base := 10): TEXT;
PROCEDURE Unsigned(n: Word.T; base: Base := 16): TEXT;
(* Format the signed or unsigned number "n" in the specified base. *)

(* The value returned by "Int" or "Unsigned" never contains upper-case
   letters, and it never starts with an explicit base and underscore.
   For example, to render an unsigned number "N" in hexadecimal as a
   legal Modula-3 literal, you must write something like:
| "16_" & Fmt.Unsigned(N, 16)
*)

TYPE Align = {Left, Right};

PROCEDURE Pad(
    text: TEXT;
    length: CARDINAL;
    padChar: CHAR := ' ';
    align: Align := Align.Right): TEXT;
(* If "Text.Length(text) >= length", then "text" is returned
   unchanged.  Otherwise, "text" is padded with "padChar" until it has
   the given "length".  The text goes to the right or left, according
   to "align". *)

PROCEDURE F(fmt: TEXT; t1, t2, t3, t4, t5: TEXT := NIL)
  : TEXT;
(* Uses "fmt" as a format string. The result is a copy of "fmt" in
   which all format specifiers have been replaced, in order, by the
   text arguments "t1", "t2", etc. *)

(* A format specifier contains a field width, alignment and one of two
   padding characters. The procedure "F" evaluates the specifier and
   replaces it by the corresponding text argument padded as it would
   be by a call to "Pad" with the specified field width, padding
   character and alignment.

   The syntax of a format specifier is:
| %[-]{0-9}s
   that is, a percent character followed by an optional minus sign, an
   optional number and a compulsory terminating "s".

   If the minus sign is present the alignment is "Align.Left",
   otherwise it is "Align.Right". The alignment corresponds to the
   "align" argument to "Pad".

   The number specifies the field width (this corresponds to the
   "length" argument to "Pad"). If the number is omitted it defaults
   to zero.

   If the number is present and starts with the digit "0" the padding character
   is "'0'"; otherwise it is the space character. The padding character
   corresponds to the "padChar" argument to "Pad".

   It is a checked runtime error if "fmt" is "NIL" or the number of
   format specifiers in "fmt" is not equal to the number of non-nil
   arguments to "F".

   Non-nil arguments to "F" must precede any "NIL" arguments; it is a
   checked runtime error if they do not.

   If "t1" to "t5" are all "NIL" and "fmt" contains no format
   specifiers, the result is "fmt".

   Examples:
| F("%s %s\n", "Hello", "World") `returns` "Hello World\n".
| F("%s", Int(3))                `returns` "3"
| F("%2s", Int(3))               `returns` " 3"
| F("%-2s", Int(3))              `returns` "3 "
| F("%02s", Int(3))              `returns` "03"
| F("%-02s", Int(3))             `returns` "30"
| F("%s", "%s")                  `returns` "%s"
| F("%s% tax", Int(3))           `returns` "3% tax"

   The following examples are legal but pointless:
| F("%-s", Int(3))               `returns` "3"
| F("%0s", Int(3))               `returns` "3"
| F("%-0s", Int(3))              `returns` "3"
*)

PROCEDURE FN(fmt: TEXT; READONLY texts: ARRAY OF TEXT)
  : TEXT;
(* Similar to "F" but accepts an array of text arguments. It is a
   checked runtime error if the number of format specifiers in "fmt"
   is not equal to "NUMBER(texts)" or if any element of "texts" is
   "NIL". If "NUMBER(texts) = 0" and "fmt" contains no format
   specifiers the result is "fmt". *)

(* Example:

| FN("%s %s %s %s %s %s %s",
|   ARRAY OF TEXT{"Too", "many", "arguments",
|     "for", "F", "to", "handle"})

   returns {\tt \char'42Too many arguments for F to handle\char'42}.
*)

(* format an ADDRESS *)
PROCEDURE Addr (a: ADDRESS) : TEXT;

(* format a REF *)
PROCEDURE Ref (r: REFANY) : TEXT;

END Fmt.





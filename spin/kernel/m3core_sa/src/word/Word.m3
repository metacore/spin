(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Mon Dec 16 11:02:01 PST 1991 by kalsow     *)
(*      modified on Sat May 19 07:28:29 1990 by muller         *)

(*
 * HISTORY
 * 24-May-96  Wilson Hsieh (whsieh) at the University of Washington
 *	Word interface is FUNCTIONAL
 *
 * 26-Oct-95  Wilson Hsieh (whsieh) at the University of Washington
 *	make Word interface EPHEMERAL
 *
 *)

MODULE Word;
IMPORT Word; (* let the compiler implement each of these as inlines *)

FUNCTIONAL EPHEMERAL PROCEDURE Plus (x, y: T): T     = BEGIN RETURN Word.Plus (x, y)   END Plus;
FUNCTIONAL EPHEMERAL PROCEDURE Times (x, y: T): T    = BEGIN RETURN Word.Times (x, y)  END Times;
FUNCTIONAL EPHEMERAL PROCEDURE Minus (x, y: T): T    = BEGIN RETURN Word.Minus (x, y)  END Minus;
FUNCTIONAL EPHEMERAL PROCEDURE Divide (x, y: T): T   = BEGIN RETURN Word.Divide (x, y) END Divide;
FUNCTIONAL EPHEMERAL PROCEDURE Mod (x, y: T): T      = BEGIN RETURN Word.Mod (x, y)    END Mod;
FUNCTIONAL EPHEMERAL PROCEDURE LT (x, y: T): BOOLEAN = BEGIN RETURN Word.LT (x, y)     END LT;
FUNCTIONAL EPHEMERAL PROCEDURE LE (x, y: T): BOOLEAN = BEGIN RETURN Word.LE (x, y)     END LE;
FUNCTIONAL EPHEMERAL PROCEDURE GT (x, y: T): BOOLEAN = BEGIN RETURN Word.GT (x, y)     END GT;
FUNCTIONAL EPHEMERAL PROCEDURE GE (x, y: T): BOOLEAN = BEGIN RETURN Word.GE (x, y)     END GE;
FUNCTIONAL EPHEMERAL PROCEDURE And (x, y: T): T      = BEGIN RETURN Word.And (x, y)    END And;
FUNCTIONAL EPHEMERAL PROCEDURE Or (x, y: T): T       = BEGIN RETURN Word.Or (x, y)     END Or;
FUNCTIONAL EPHEMERAL PROCEDURE Xor (x, y: T): T      = BEGIN RETURN Word.Xor (x, y)    END Xor;
FUNCTIONAL EPHEMERAL PROCEDURE Not (x: T): T         = BEGIN RETURN Word.Not (x)       END Not;

FUNCTIONAL EPHEMERAL PROCEDURE Shift (x: T; n: INTEGER): T
 = BEGIN RETURN Word.Shift (x, n) END Shift;

FUNCTIONAL EPHEMERAL PROCEDURE LeftShift (x: T; n: [0..Size-1]): T
 = BEGIN RETURN Word.LeftShift (x, n) END LeftShift;

FUNCTIONAL EPHEMERAL PROCEDURE RightShift (x: T; n: [0..Size-1]): T
 = BEGIN RETURN Word.RightShift (x, n) END RightShift;

FUNCTIONAL EPHEMERAL PROCEDURE Rotate (x: T; n: INTEGER): T
 = BEGIN RETURN Word.Rotate (x, n) END Rotate;

FUNCTIONAL EPHEMERAL PROCEDURE LeftRotate (x: T; n: [0..Size-1]): T
 = BEGIN RETURN Word.LeftRotate (x, n) END LeftRotate;

FUNCTIONAL EPHEMERAL PROCEDURE RightRotate (x: T; n: [0..Size-1]): T
 = BEGIN RETURN Word.RightRotate (x, n) END RightRotate;
 
FUNCTIONAL EPHEMERAL PROCEDURE Extract (x: T; i, n: CARDINAL): T
 = BEGIN RETURN Word.Extract (x, i, n) END Extract;

FUNCTIONAL EPHEMERAL PROCEDURE Insert (x, y: T; i, n: CARDINAL): T
 = BEGIN RETURN Word.Insert (x, y, i, n) END Insert;

BEGIN
END Word.

(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * Convert things into printable texts.
 *
 *
 * HISTORY
 * 27-Mar-96  Brian Bershad (bershad) at the University of Washington
 *	Add Type.
 *
 * 13-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	Created.
 *
 *)
INTERFACE Textify;

IMPORT Thread, CPU;

PROCEDURE ThreadName (READONLY t: Thread.T; html: BOOLEAN := FALSE): TEXT;
PROCEDURE Regs(READONLY s: CPU.GeneralRegs; html: BOOLEAN := FALSE;): TEXT;

(* Addrs *)
PROCEDURE Ref (READONLY ref: REFANY; html: BOOLEAN := FALSE): TEXT;
PROCEDURE Address (READONLY addr: ADDRESS; html: BOOLEAN := FALSE): TEXT;

(*
 * Type information 
 *)
PROCEDURE Type(READONLY ref: REFANY; html: BOOLEAN := FALSE): TEXT;



(*
 * Return the named text inside the specified HTML modifier.
 *)

CONST ITALIC = "i";
      BOLDFACE = "b";
      TYPESCRIPT = "tt";

PROCEDURE Htmlize(t: TEXT; in: TEXT; html: BOOLEAN := TRUE): TEXT;

END Textify.



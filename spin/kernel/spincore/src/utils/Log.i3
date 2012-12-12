(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 10-Dec-94  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Log functions for use in debugging.
 *)
INTERFACE Log;
IMPORT Word;

(* The routines below log without allocation or preemption *)
PROCEDURE Logi(x: Word.T);    (* decimal integer *)
PROCEDURE Logx(x: Word.T);    (* hex integer *)
PROCEDURE LogAdr(x: ADDRESS); (* address in hex *)
PROCEDURE LogRef(x: REFANY);  (* refany in hex *)
PROCEDURE Dumplog();

PROCEDURE Log(READONLY x: TEXT);(* log arbitrary text *)

END Log.

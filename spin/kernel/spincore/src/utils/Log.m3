(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 10-Dec-94  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Log functions for use in debugging.
 *)
UNSAFE (* to import externals *)
MODULE Log;
IMPORT LogExtern, Word, M3toC;

PROCEDURE Logi(x: Word.T) =     (* decimal integer *)
  BEGIN
    LogExtern.plogi(x);
  END Logi;

PROCEDURE Logx(x: Word.T) =    (* hex integer *)
  BEGIN
    LogExtern.plogx(x);
  END Logx;

PROCEDURE LogAdr(x: ADDRESS) = (* address in hex *)
  BEGIN
    LogExtern.plogx(LOOPHOLE(x,Word.T));
  END LogAdr;

PROCEDURE LogRef(x: REFANY) =  (* refany in hex *)
  BEGIN
    LogExtern.plogx(LOOPHOLE(x,Word.T));
  END LogRef;

PROCEDURE Dumplog() =
  BEGIN
    LogExtern.dumplog();
  END Dumplog;

PROCEDURE Log(READONLY text: TEXT) =
  BEGIN
    (* Using TtoS here is ok since plog is non-blocking, non-allocating *)
    (* and non-preemptible. text also appears on stack. all bases are *)
    (* covered. *)
    IF text = NIL THEN
      LogExtern.plog(M3toC.TtoS(">> NIL <<\n")); (* safe *)
    ELSE
      LogExtern.plog(M3toC.TtoS(text)); (* safe *)
    END;
  END Log;

BEGIN
END Log.

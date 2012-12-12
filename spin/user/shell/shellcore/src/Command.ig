(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 03-Jan-97  Yasushi Saito (yasushi) at the University of Washington
 *	Changed to use ShellCommand
 * 21-Apr-96  Marc Fiuczynski (mef) at the University of Washington
 *	Cleaned up.
 *
 * 12-Feb-96  Brian Bershad (bershad) at the University of Washington
 *	Add Uninstall facility.
 *
 * 26-Dec-95  Emin Gun Sirer (egs) at the University of Washington
 *	Whisted.
 *)

(* This generic is provided mainly for backward compatibility.
   Newer commands should use ShellCommand directly. *)

GENERIC INTERFACE Command();
PROCEDURE Install();
PROCEDURE Uninstall();
END Command.



(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 10-Dec-94  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Log functions for use in debugging.
 *)
UNSAFE (* to define externals *)
INTERFACE LogExtern;
IMPORT Word, Ctypes;

(* The routines below log without allocation or preemption *)

<*EXTERNAL*>PROCEDURE plog(x: Ctypes.const_char_star);
<*EXTERNAL*>PROCEDURE plogi(x: Word.T);    (* decimal integer *)
<*EXTERNAL*>PROCEDURE plogx(x: Word.T);    (* hex integer *)

<*EXTERNAL*>PROCEDURE dumplog();

END LogExtern.

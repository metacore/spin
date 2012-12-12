(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 4-oct-96  becker at the University of Washington
 *	Turned off Debug
 *
 * 10-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
INTERFACE SphinxUtils;
IMPORT Proc;

CONST
  Debug = FALSE;
  MicroBench = FALSE;

PROCEDURE Msg(a,b,c,d,e: TEXT := NIL);
PROCEDURE ProcName(proc: Proc.T := NIL): TEXT;
  (* Return the descriptive name of the process "proc".*)
VAR
  profCount: CARDINAL;
  
END SphinxUtils.

(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 26-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

INTERFACE TransUtils;
IMPORT Spy;

CONST
  Debug = FALSE; (* do sanity checks. *)
  DebugMsg = FALSE; (* output silly diagnostic messages. *)
  SpyTime = TRUE; (* use spy timer *)
  NItr = 200; (* # of spy entries. *)
  DetectDeadlock = FALSE;
  (* Only when "DetectDeadlock" is true, the lock manager detects the
   local deadlock situation.
   *)
VAR
  setRangeTimer, commitTimer: Spy.T;
  GatherStats: BOOLEAN;
  
PROCEDURE Warn(a,b,c,d,e,f,g,h,i,j: TEXT := NIL);
PROCEDURE Msg(a,b,c,d,e,f,g,h,i,j: TEXT := NIL);
  (* Display the list of texts with trailing newline. *)
PROCEDURE Msg1(a,b,c,d,e,f,g,h,i,j: TEXT := NIL);
  (* Display the list of texts without trailing newline. *)
  
PROCEDURE ValueIsNotPageAligned(p: INTEGER): BOOLEAN;
END TransUtils.

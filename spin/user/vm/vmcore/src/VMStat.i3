(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 01-Oct-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

(* This interface defines some statistics variables.
 See "man vmstat(1)" in OSF/1 also. *)

INTERFACE VMStat;

CONST Enabled = FALSE;
  
VAR
  fault: CARDINAL; (* # of page faults. *)
  cow: CARDINAL; (* # of cow faults. *)
  zero: CARDINAL; (* # of zero fill faults *)
  react: CARDINAL; (* fault while in inactive list. *)
  pin: CARDINAL; (* # of pagein. *)
  pout: CARDINAL; (* # of pageout. *)
  
END VMStat.

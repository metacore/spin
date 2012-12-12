(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 04-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

(* Various user definable flags to control the behavior of the system. *)
INTERFACE TransFlags;

VAR
  clearLogOnStartup: BOOLEAN;
  (* If true, the log file is erased when it is opened. This should be used
     only for debugging. *)

END TransFlags.

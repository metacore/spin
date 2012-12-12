(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 27-May-97  Yasushi Saito (yasushi) at the University of Washington
 *	Added IsLocked.
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created. Mutexes.
 *)

(* This interface defines auxiliary procedures for MUTEX. For 
   usual locking and unlocking, use Thread.Acquire and Thread.Release.
*)
INTERFACE Mutex;

PROCEDURE TryLock(l: MUTEX): BOOLEAN;
  (* Try to lock "l". This proc does not block.
     Returns TRUE if successful. *)
  
PROCEDURE IsLocked(l: MUTEX): BOOLEAN;
  (* Returns TRUE if "l" is locked. The state of "l" is not changed. *)

END Mutex.

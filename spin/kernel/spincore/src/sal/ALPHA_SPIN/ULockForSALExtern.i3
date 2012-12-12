(*
 * Copyright 1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 31-Jan-97  Marc Fiuczynski (mef) at the University of Washington
 *	created.
 *)

UNSAFE (* for EXTERNALs *)
INTERFACE ULockForSALExtern;
IMPORT ULockForSAL;
(* CRUCIAL!!!
   This type must mirror the structure defined by ULockInterface.h 
 *)

TYPE T = RECORD
  ulock_setup     : PROCEDURE(VAR rwl: ULockForSAL.T; lip: ADDRESS; canwait: BOOLEAN);
  ulock_terminate : PROCEDURE(VAR rwl: ULockForSAL.T);
  ulock_write     : PROCEDURE(VAR rwl: ULockForSAL.T);
  ulock_read      : PROCEDURE(VAR rwl: ULockForSAL.T);
  ulock_done      : PROCEDURE(VAR rwl: ULockForSAL.T);
  ulock_try_write : PROCEDURE(VAR rwl: ULockForSAL.T): BOOLEAN;
  ulock_try_read  : PROCEDURE(VAR rwl: ULockForSAL.T): BOOLEAN;
  write_to_read   : PROCEDURE(VAR rwl: ULockForSAL.T);
  set_recursive   : PROCEDURE(VAR rwl: ULockForSAL.T);
  clear_recursive : PROCEDURE(VAR rwl: ULockForSAL.T);
END;

<* EXTERNAL "ULock" *>
VAR ULockInterface : T;

END ULockForSALExtern.

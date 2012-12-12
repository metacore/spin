(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 08-Apr-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed the readerQ/writerQ and lastlocker to addresses so that
 *	the type can be incorporated into a record that is untraced.  The
 *	ULock implementation will explicitly LOOPHOLE the fields.
 *
 * 04-Mar-96  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Support for DEC OSF synchronization. A Ulock is DEC's
 *      version of reader/writer locks. Ulocks are recursive.
 *)
INTERFACE ULockForSAL;
IMPORT Sema, Thread; <* NOWARN *>
IMPORT Ctypes;

(*
 * XXX Warning: The following record has to be the same size as a
 *     lock_data_t defined in kern/lock.h.
 *)
TYPE T = RECORD
  readerCnt  : BITS 32 FOR Ctypes.int := 0;
  writerCnt  : BITS 32 FOR Ctypes.int := 0;
  readerVal  : BITS 32 FOR Ctypes.int := 0; (* rd sema.val             *)
  writerVal  : BITS 32 FOR Ctypes.int := 0; (* wr sema.val             *)
  reader     : ADDRESS;                     (* rd sema.list - Thread.T *)
  writer     : ADDRESS;                     (* wr sema.list - Thread.T *)
  lastlocker : ADDRESS;                     (* Thread.T                *)
END;

PROCEDURE ulock_setup(VAR rwl: T; lip: ADDRESS; canwait: BOOLEAN);
PROCEDURE ulock_terminate(VAR rwl: T);
PROCEDURE ulock_write(VAR rwl: T);
PROCEDURE ulock_read(VAR rwl: T);
PROCEDURE ulock_done(VAR rwl: T);
PROCEDURE ulock_try_write(VAR rwl: T): BOOLEAN;
PROCEDURE ulock_try_read(VAR rwl: T): BOOLEAN;

END ULockForSAL.

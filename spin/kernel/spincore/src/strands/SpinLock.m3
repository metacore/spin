(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created.
 *
 * This module is unsafe because it uses an extern.
 *)
UNSAFE MODULE SpinLock;

IMPORT AtomicOps;

PROCEDURE TryLock(VAR lock: INTEGER) : BOOLEAN =
  BEGIN
    RETURN AtomicOps.TryLock(lock);
  END TryLock;

PROCEDURE Unlock(VAR lock: INTEGER) =
  BEGIN
    AtomicOps.Unlock(lock);
  END Unlock;


PROCEDURE Lock(VAR lock: INTEGER) =
  BEGIN
    WHILE AtomicOps.TryLock(lock) DO
    END;
  END Lock;

BEGIN
END SpinLock.

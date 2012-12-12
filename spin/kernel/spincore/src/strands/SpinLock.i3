(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created.
 *)
INTERFACE SpinLock;

PROCEDURE TryLock(VAR lock: INTEGER) : BOOLEAN;
PROCEDURE Unlock(VAR lock: INTEGER);
PROCEDURE Lock(VAR lock: INTEGER);

END SpinLock.

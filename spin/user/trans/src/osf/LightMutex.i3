(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *)
INTERFACE LightMutex;
TYPE T = MUTEX;
PROCEDURE Create(): T;
PROCEDURE Lock(t: T);
PROCEDURE Unlock(t: T);
PROCEDURE TryLock(t: T): BOOLEAN;
END LightMutex.

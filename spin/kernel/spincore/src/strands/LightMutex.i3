(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 01-Feb-97  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
INTERFACE LightMutex;

TYPE T = INTEGER;
PROCEDURE Create(): T;
PROCEDURE Lock(VAR t: T);
PROCEDURE Unlock(VAR t: T);
PROCEDURE TryLock(VAR t: T): BOOLEAN;
  
END LightMutex.

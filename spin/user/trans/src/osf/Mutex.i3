(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 10-Dec-96  Yasushi Saito (yasushi) at the University of Washington
 *	created
 *	
 *)
INTERFACE Mutex;

PROCEDURE TryLock(mu: MUTEX): BOOLEAN;
  
END Mutex.

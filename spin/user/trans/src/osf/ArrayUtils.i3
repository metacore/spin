(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 28-Apr-97  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
INTERFACE ArrayUtils;

PROCEDURE Clear(VAR a: ARRAY OF CHAR);
PROCEDURE Set(VAR a: ARRAY OF CHAR; ch: CHAR);
  
END ArrayUtils.

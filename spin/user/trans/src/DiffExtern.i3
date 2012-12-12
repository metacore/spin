(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 06-Apr-97  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
UNSAFE INTERFACE DiffExtern;
<*EXTERNAL*>PROCEDURE diff(old, new, diff: ADDRESS; copy: BOOLEAN): INTEGER;
END DiffExtern.

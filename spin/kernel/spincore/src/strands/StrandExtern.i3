(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 10-Sep-97  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
UNSAFE INTERFACE StrandExtern;
IMPORT Strand;

<*EXTERNAL*> VAR Cur: Strand.T;
(* Current active strand.
   The only reason this is marked EXTERNAL is to make access faster. *)
<*EXTERNAL*> VAR dieing: INTEGER;
  
END StrandExtern.

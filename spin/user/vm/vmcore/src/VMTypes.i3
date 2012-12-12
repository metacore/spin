(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 21-Jun-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created
 *)
INTERFACE VMTypes;

CONST
  (* They are physaddr protection "other" bits. *)
  CopyOnWrite = 1; 
  Busy = 2; (* some activity is going on on the page. *)
  Waiting = 4; (* someone is waiting on the page. "Busy" and "Waiting"
		must be checked while the object is being locked. *)
  
TYPE
  PageNumber = INTEGER;
  PageCount = INTEGER;

END VMTypes.

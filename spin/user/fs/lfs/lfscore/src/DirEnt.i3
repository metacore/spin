(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 7-Oct-96 Eric Christoffersen (ericc) at the University of Washington
 *	Created.  Substitute for DirEnt removed from fscore.
 *
*)
INTERFACE DirEnt;

TYPE
  T = RECORD
    ino:INTEGER;
    name:TEXT;
    nextPos:INTEGER;
  END;

END DirEnt.

(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 29-May-97  Yasushi Saito (yasushi) at the University of Washington
 *	Whisted. This should be phased out once we have functional and generic 
 *	vnode cache.
 *)

INTERFACE Cache;

TYPE T <: TPublic;
  TPublic = OBJECT
  METHODS
    init(size : INTEGER) : T;
    find(mountPoint: REFANY; inumber : INTEGER) : REFANY;
    insert(val : REFANY; mountPoint: REFANY; inumber : INTEGER);
    purgeEntries(mountPoint: REFANY);
    (* Delete all the entries tagged by "mountPoint". *)
  END;
  
END Cache.

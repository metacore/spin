(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 26-Jul-97  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
INTERFACE FSRoot;
IMPORT StatFs;
IMPORT Error;

TYPE T <: Public;
  Public = OBJECT
  METHODS
    sync() RAISES {Error.E};
    statfs((*OUT*)VAR buf: StatFs.T) RAISES {Error.E};
  END;
  
END FSRoot.

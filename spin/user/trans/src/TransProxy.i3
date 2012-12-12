(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 18-May-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
INTERFACE TransProxy;
IMPORT TID;

PROCEDURE Resolve(tid: TID.T): BOOLEAN;
  
END TransProxy.

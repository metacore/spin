(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 07-May-96  Stefan Savage (savage) at the University of Washington
 *	Created.
 *)

INTERFACE AddressMapRep;

IMPORT AddressMap;
IMPORT AddressMapEntry;

TYPE
  T = AddressMap.Public OBJECT
    list: 
    lastAccessedEntry: AddressMapEntry.T;
    (* This acts as a single entry lookaside cache. *) 
  END;

END AddressMapRep.






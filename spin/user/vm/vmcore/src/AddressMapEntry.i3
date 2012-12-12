(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 07-May-96  Stefan Savage (savage) at the University of Washington
 *	Created.
 *)

(* "T" records each mmap info. *)

INTERFACE AddressMapEntry;

IMPORT VMTypes;
IMPORT MemoryObject;
IMPORT Protection;
IMPORT AddressSpace;

TYPE
  T = REF RECORD
    from, end: VMTypes.PageNumber;
    mObj: MemoryObject.T;
    space: AddressSpace.T;
    mOff: VMTypes.PageNumber;
    prot: Protection.T;
    next: T; (* This is used to link together the map entries
	      belonging to single memory object.
	      MemoryObject.mapEntries holds the head of the
	      list. *)
    prev: T; (* next and prev are used to hold together unused
		address map entries. *)
    link: T;
    checksum: REF ARRAY OF INTEGER;
  END;

CONST Brand = "AddressMapEntry-1.0";

END AddressMapEntry.

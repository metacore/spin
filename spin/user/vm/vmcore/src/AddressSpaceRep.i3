(*
 * Copyright 1995, 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 14-Feb-96  Stefan Savage (savage) at the University of Washington
 *	Changed to object represetation
 *
 * 20-Dec-95  Stefan Savage (savage) at the University of Washington
 *	Created
 *)

INTERFACE AddressSpaceRep;

IMPORT VMTypes;
IMPORT AddressSpace;
IMPORT Dispatcher;
IMPORT ExternalRef;
IMPORT AddressMapEntry;

TYPE T = AddressSpace.Public OBJECT
  maps: AddressMapEntry.T; (* sorted dequeue of memory object mappings. *)
  mapCache: AddressMapEntry.T; (* one entry cache for lookup and mmap. *)
  externs: ExternalRef.T;
  first,last: VMTypes.PageNumber;
  lock: MUTEX;
  name: TEXT;
  id: CARDINAL;
  bindings : ARRAY [0 .. 2] OF Dispatcher.Binding; (* fault handlers *)
 END;

REVEAL AddressSpace.T <: T;

END AddressSpaceRep.

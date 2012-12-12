(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 31-Jul-97  Yasushi Saito (yasushi) at the University of Washington
 *	Changed from Btree to dequeue based implemnetation.
 * 07-May-96  Stefan Savage (savage) at the University of Washington
 *	Created.
 *)

MODULE AddressMap;
IMPORT AddressMapQ;
IMPORT AddressMapRep, AddressMapEntry, AddressMapKey;
IMPORT Fmt, IO;
IMPORT VMError, VMDebug;
IMPORT Debugger; <*NOWARN*>

REVEAL
  T = AddressMapRep.T BRANDED OBJECT
    q: AddressMapEntry.T;
  OVERRIDES
    init := Init;
    insert := Insert;
    delete := Delete;
    operate := Operate;
    lookup := Lookup;
    iterate := Iterate;
    print := Print;
  END;

BEGIN
END AddressMap.

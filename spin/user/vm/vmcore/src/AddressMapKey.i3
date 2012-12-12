(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 15-Apr-97  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
INTERFACE AddressMapKey;
IMPORT VMTypes;

TYPE T = RECORD
  from, end: VMTypes.PageNumber;
END;

CONST Brand = "AddressMapKey";

PROCEDURE Equal(READONLY me1, me2: T): BOOLEAN;
PROCEDURE Hash(me: T): CARDINAL;
PROCEDURE Compare(READONLY me1, me2: T): [-1..1];

END AddressMapKey.

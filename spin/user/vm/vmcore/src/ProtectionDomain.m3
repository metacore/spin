(*
 * Copyright 1995, 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 20-Feb-96  Stefan Savage (savage) at the University of Washington
 *	Created
 *)

MODULE ProtectionDomain;

IMPORT ProtectionDomainRep, AddressSpace, AddressSpaceRep, Word, AddressRange;

REVEAL
  T = ProtectionDomainRep.T BRANDED OBJECT
  OVERRIDES
    init := Initialize;
    destroy := Destroy;
    protect := Protect;
    info := Info;
    print := Print;
  END;

PROCEDURE Initialize(self: T; as: AddressSpace.T): T RAISES {Failure} =
  VAR
    minMaxFlag: BOOLEAN := FALSE;
  BEGIN
    self.addressSpace := as;
    RETURN self;
  END Initialize;

PROCEDURE Destroy(self: T) RAISES {Failure} =
  BEGIN
  END Destroy;

PROCEDURE Protect(self: T; aRange: AddressRange.T; prot: Protection) =
  BEGIN
  END Protect; 


PROCEDURE Info(self: T; aRange: AddressRange.T): Protection RAISES {Failure} =
  VAR
    dummy: Protection;
  BEGIN
    RETURN dummy;
  END Info;


PROCEDURE Print(self: T): TEXT =
  BEGIN
    RETURN "";
  END Print;

PROCEDURE Equal(as1, as2: T): BOOLEAN =
  BEGIN
  END Equal;

PROCEDURE Hash(as: T): Word.T =
  BEGIN
  END Hash;

PROCEDURE Compare(as1, as2: T): [-1..1] =
  BEGIN
  END Compare;


BEGIN
END ProtectionDomain.

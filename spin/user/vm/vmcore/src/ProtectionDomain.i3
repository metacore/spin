(*
 * Copyright 1995,1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 19-Feb-96  Stefan Savage (savage) at the University of Washington
 *	Created
 *)

INTERFACE ProtectionDomain;

IMPORT AddressSpace, Translation, AddressRange, Word;

EXCEPTION
  Failure;

TYPE
  T <: Public;
  Public = OBJECT 
  METHODS
    init(as: AddressSpace.T): T RAISES {Failure};
    destroy() RAISES {Failure};
    protect(aRange: AddressRange.T; prot: Protection);
    info(aRange: AddressRange.T): Protection RAISES {Failure};
    print(): TEXT;
  END;
  Protection = Translation.Protection;

CONST
  Brand = "ProtectionDomain-1.0";

PROCEDURE Equal(as1, as2: T): BOOLEAN;
PROCEDURE Hash(as: T): Word.T;
PROCEDURE Compare(as1, as2: T): [-1..1];

END ProtectionDomain. 

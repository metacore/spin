(*
 * Copyright 1995, 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *
 * 20-Feb-96  Stefan Savage (savage) at the University of Washington
 *	Created
 *)

INTERFACE ProtectionDomainRep;

IMPORT ProtectionDomain, AddressSpace;

TYPE T = ProtectionDomain.Public OBJECT
  addressSpace: AddressSpace.T;
 END;

REVEAL ProtectionDomain.T <: T;


END ProtectionDomainRep.

(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 13-Mar-96  Stefan Savage (savage) at the University of Washington
 *	Created.
 *
 *)

INTERFACE MachAddressRange;

IMPORT AddressRange, Mach;

EXCEPTION
  Failure;

TYPE T <: AddressRange.T OBJECT
  isShared: BOOLEAN;
  maxProtection: Mach.VMProtT;
  inheritance: Mach.VMInheritT;
END;
  
CONST Brand = "MachAddressRange-1.0";

END MachAddressRange.

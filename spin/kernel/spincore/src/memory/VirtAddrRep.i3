(*
 * Copyright 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 02-Jan-95  Stefan Savage (savage) at the University of Washington
 *	Created
 *)

INTERFACE VirtAddrRep;

IMPORT VirtAddr;

TYPE 

  REVEAL VirtAddr.T = BRANDED "VirtAddrT" REF RECORD
    beginAddress   : VirtAddr.Address;
    endAddress     : VirtAddr.Address;
  END;  

END VirtAddrRep.



(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 12-Apr-97  Tsutomu Owa (owa) at the University of Washington
 *	Change interface names so that these don't conflict w/
 *	those used by user/net.
 *
 * 18-Mar-97  Tsutomu Owa (owa) at the University of Washington
 *	Created.
 *)

INTERFACE DefaultAddrDep;
IMPORT StcpEtherPktFormat, StcpIpPktFormat;

(* to get ether address by ioctl *)
PROCEDURE GetMyAddr(VAR hwAddr : StcpEtherPktFormat.Address;
		    VAR ipAddr : StcpIpPktFormat.AddressArray
		    ) : BOOLEAN ;
END DefaultAddrDep.


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

UNSAFE (* for EtherPacketExtern *)
MODULE DefaultAddrDep;
IMPORT StcpIf, StcpEtherPktFormat, StcpIpPktFormat;
IMPORT StcpSocketAddr, StcpEtherPacketExtern;
IMPORT IO, Ctypes;
(*
IMPORT Debugger;
 *)

PROCEDURE GetMyAddr(VAR hwAddr : StcpEtherPktFormat.Address;
		    VAR ipAddr : StcpIpPktFormat.AddressArray
		    ) : BOOLEAN =
  PROCEDURE ioctl(ifp: ADDRESS;
		  cmd: Ctypes.unsigned_long;
		  VAR data: ARRAY OF CHAR): Ctypes.int =
    BEGIN
      WITH tempifp = LOOPHOLE(ifp,UNTRACED REF StcpIf.ifnet) DO
        RETURN tempifp.if_ioctl(tempifp^,cmd,ADR(data[0]));
      END;
    END ioctl;
  VAR
    error : Ctypes.int;
    ifreq : ARRAY [1..BYTESIZE(StcpIf.ifreq)] OF CHAR;
    myStcpIfp : UNTRACED REF StcpIf.ifnet;
  BEGIN
    (*
    Debugger.Enter();
     *)
    (* defined at /usr/include/linux/sockios.h *)
    (* #define SIOCGIFADDR 0x8915 /* get PA address */ *)
    (* SIOCGIFADDR is 0xc0206921 *)
    myStcpIfp := StcpEtherPacketExtern.net_poll_getifp();
    error := ioctl(myStcpIfp, 16_c0206921, ifreq);
    IF error # 0 THEN
      IO.Put("    iotcl:SIOCGIFADDR failed.\n");
    ELSE
      WITH req = VIEW (ifreq, StcpIf.ifreq),
	   s = VIEW(req.ifr_ifru, StcpSocketAddr.OldT) DO
	hwAddr := SUBARRAY(s.sa_data, 0, 6);
	ipAddr := SUBARRAY(myStcpIfp.if_addrlist.ifa_addr.sa_data, 2, 6);
      END;
    END;
    IF (error # 0) THEN RETURN FALSE; ELSE RETURN TRUE; END;
  END GetMyAddr;

BEGIN
END DefaultAddrDep.


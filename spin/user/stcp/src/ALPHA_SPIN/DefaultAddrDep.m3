(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 24-Mar-97  Tsutomu Owa (owa) at the University of Washington
 *	Added Error checking code
 *
 * 09-Apr-97  Tsutomu Owa (owa) at the University of Washington
 *	Change interface names so that these don't conflict w/
 *	those used by user/net.
 *
 * 18-Mar-97  Tsutomu Owa (owa) at the University of Washington
 *	Created.
 *)

UNSAFE (* for StcpEtherPacketExtern *)
MODULE DefaultAddrDep;
IMPORT StcpIf, StcpEtherPktFormat, StcpIpPktFormat;
IMPORT StcpEtherPacketExtern;
IMPORT IO, Ctypes;
IMPORT Debugger;

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
    devaddr :  ARRAY [1..BYTESIZE(StcpIf.ifdevea)] OF CHAR;
    myStcpIfp : UNTRACED REF StcpIf.ifnet;
  BEGIN
    (*
    Debugger.Enter();
    *)
    myStcpIfp := StcpEtherPacketExtern.net_poll_getifp();
    (* defined at /usr/include/sys/ioctl.h
     * #define SIOCRPHYSADDR   _IOWR('i', 62, struct ifdevea) 
     *					  /* Read Phys addr */
     * IoctlPosix.SIOCRPHYSADDR is 10_3223087422
     *)
    error := ioctl(myStcpIfp, 10_3223087422, devaddr);
    IF error # 0 THEN
      IO.Put("    ioctl:SIOCRPHYSADDR failed.\n");
      Debugger.Enter();
    ELSE
      (* see if ifp is filled fine.  It should be set by SAL but sometimes
	 NIL.  Not sure why... *)
      IF myStcpIfp.if_addrlist.ifa_addr = NIL THEN
	IO.Put("Stcp:GetMyAddr(). ifp.if_addrlist.ifa_addr is NIL\n"); 
	IO.Put("Try to reboot it!\n");
        Debugger.Enter();
      END;
      WITH d = VIEW(devaddr, StcpIf.ifdevea) DO
	hwAddr := SUBARRAY(d.default_pa, 0, 6);
	ipAddr := SUBARRAY(myStcpIfp.if_addrlist.ifa_addr.sa_data, 2, 6);
      END;
    END;
    RETURN TRUE;
  END GetMyAddr;

BEGIN
END DefaultAddrDep.


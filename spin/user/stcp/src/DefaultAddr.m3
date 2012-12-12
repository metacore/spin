(*
 * Copyright 1997, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 26-Jul-97  Tsutomu Owa (owa) at the University of Washington
 *	Use Salnet.Arp() instead of Stcp's Arp module to get ether address.
 *
 * 09-Apr-97  Tsutomu Owa (owa) at the University of Washington
 *	Change interface names so that these don't conflict w/
 *	those used by user/net.
 *
 * 18-Mar-97  Tsutomu Owa (owa) at the University of Washington
 *	Reverted If structure defined as urt/urtcore/src.
 *	Put GetMyAddr into ALPHA_SPIN and IX86_SPIN.
 *
 * 03-Feb-97  Tsutomu Owa (owa) at the University of Washington
 *	Created for simple tcp.
 *)

MODULE DefaultAddr;

IMPORT Salnet;
IMPORT StcpIpPktFormat;
IMPORT IO;
IMPORT NetText;
IMPORT Ctypes;
IMPORT BuildInfo;

(* Given a ip address (targetIpAddress), get hardware address and set. *)
PROCEDURE SetTarget(server: Salnet.IpAddr) =
(*
  VAR
    ip: Ctypes.unsigned_int;
*)
  BEGIN
    (* Sanity check *)
    IF server = 0 THEN
      IO.Put("ERROR; can't set nil. using default\n");
      server := defaultServer;
    END;
    TRY
        VIEW(VIEW(targetIpAddress, ARRAY [0..3] OF CHAR),
 		Ctypes.unsigned_int) := server;
    EXCEPT
    ELSE
      IO.Put("ERROR: Salnet.DnsQuery failed. use default\n");
      server := defaultServer;
      targetIpAddress := defaultIpAddress;
    END;

    TRY
      Salnet.Arp(VIEW(VIEW(targetIpAddress, ARRAY [0..3] OF CHAR),
	             Ctypes.unsigned_int), targetHardwareAddress);
    EXCEPT
    ELSE
      IO.Put("ERROR: Salnet.Arp failed. using default\n");
      server := defaultServer;
      targetIpAddress := defaultIpAddress;
      targetHardwareAddress := defaultHardwareAddress;
    END;
      
    IO.Put("  Server: ");
    IO.PutInt(server);
    IO.Put(" ip addr: " &
	   NetText.FmtIp(VIEW(targetIpAddress,StcpIpPktFormat.Address)) & 
	   ", ether addr: " & NetText.FmtEther(targetHardwareAddress) &"\n");

  END SetTarget;

(* ---------------------------------------------------------------- *)
(* initialize some useful addresses here *)
PROCEDURE Init() =
  VAR
    ipaddr := Salnet.GetLocalIp();
  BEGIN
    (* Initialize the default values *)
    defaultServer := NetText.TextToIp( BuildInfo.GetHttpServAddr() );
    defaultIpAddress := VIEW( defaultServer, StcpIpPktFormat.AddressArray );
    Salnet.Arp( defaultServer, defaultHardwareAddress );

    (* Debugger.Enter(); *)
    myIpAddress := VIEW(ipaddr, StcpIpPktFormat.AddressArray);
    Salnet.GetLocalEther(myHardwareAddress);

    IO.Put("  Ether Addr: " &NetText.FmtEther(myHardwareAddress));
    IO.Put("  IP Addr: " &NetText.FmtIp(VIEW(myIpAddress,
    		StcpIpPktFormat.Address)) &"\n");

    defaultHttpdPort := BuildInfo.GetHttpPort();
    SetTarget( defaultServer );
  END Init;

BEGIN
END DefaultAddr.

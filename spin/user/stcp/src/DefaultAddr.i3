(*
 * Copyright 1997, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
:*
 *
 * HISTORY
 * 26-Jul-97  Tsutomu Owa (owa) at the University of Washington
 *	Added SetTarget() to set target hostname/addresses.
 *
 * 09-Apr-97  Tsutomu Owa (owa) at the University of Washington
 * 09-Apr-97  Tsutomu Owa (owa) at the University of Washington
 *	Change interface names so that these don't conflict w/
 *	those used by user/net.
 *
 * 03-Feb-97  Tsutomu Owa (owa) at the University of Washington
 *	Created for simple tcp.
 *)
INTERFACE DefaultAddr;
IMPORT StcpEtherPktFormat, StcpIpPktFormat;
IMPORT Salnet;

VAR
  defaultServer : Salnet.IpAddr;
  defaultIpAddress : StcpIpPktFormat.AddressArray;
  defaultHardwareAddress : StcpEtherPktFormat.Address;

 
  myIpAddress : ALIGNED 32 FOR StcpIpPktFormat.AddressArray;
  myHardwareAddress : ALIGNED 32 FOR StcpEtherPktFormat.Address;
  targetHardwareAddress : ALIGNED 32 FOR StcpEtherPktFormat.Address;
  targetIpAddress : ALIGNED 32 FOR StcpIpPktFormat.AddressArray;
  defaultHttpdPort : CARDINAL;

PROCEDURE SetTarget(server: Salnet.IpAddr);
  (* set target{Ip,Hardware}Address given a server ip address *)

PROCEDURE Init();
END DefaultAddr.

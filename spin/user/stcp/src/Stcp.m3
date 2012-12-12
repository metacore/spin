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
 * 09-Apr-97  Tsutomu Owa (owa) at the University of Washington
 *	Change interface names so that these don't conflict w/
 *	those used by user/net.
 *
 * 03-Feb-97  Tsutomu Owa (owa) at the University of Washington
 *	Created.
 *)

(* TRUSTED *) 
UNSAFE (* imports unsafe interface *)
MODULE Stcp;
IMPORT StcpInterface;
IMPORT StcpMbuf, StcpNet, IO;
IMPORT StcpEtherPacket;
IMPORT StcpTcpPacket;
IMPORT DefaultAddr;
IMPORT SimpleHttp;
IMPORT StcpEtherDev;
IMPORT Dispatcher;

(*
 * Init()
 * Used to initialize this module, since we cannot rely on M3
 * initialization order.
 *)
PROCEDURE Init() = 
  BEGIN
    StcpNet.Init();
    StcpMbuf.Init();
    DefaultAddr.Init();	
    StcpTcpPacket.Init();	
    StcpEtherPacket.Init();	
    StcpEtherDev.Init();

    
    Dispatcher.Uninstall(Dispatcher.GetOriginalHandler(StcpEtherDev.Receive));
    StcpEtherPacket.etherhandler:=
    Dispatcher.InstallHandler(StcpEtherDev.Receive, NIL,
	StcpEtherPacket.Arrived);

    (* StcpEtherArp.SendRequest();	This should be called at the end *)
    SimpleHttp.Init();		(* This should be called at the end *)
  END Init;

BEGIN
   (* register myself to NameServer *)
  EVAL StcpInterface.Export();

  Init();
  IO.Put("Stcp module initailized.\n");
END Stcp.

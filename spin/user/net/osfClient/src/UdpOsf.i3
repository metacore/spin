(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 06-Oct-96  Robert Grimm (rgrimm) at the University of Washington
 *      Added PacketSend to interface
 *
 * 01-Mar-96  Marc Fiuczynski (mef) at the University of Washington
 *	Interface to the OSF/1 3.x implementation of Udp/IP.
 *
 *)

INTERFACE UdpOsf;
IMPORT Mbuf;
IMPORT UdpPktFormat;
IMPORT ParseParams;
(* shell command support *)
CONST CommandName = "udposf";
      CommandHelp = "-- -debug level";
PROCEDURE Run(pp: ParseParams.T): BOOLEAN;

TYPE T = UdpPktFormat.NewT;
PROCEDURE Init();
PROCEDURE PacketSend(READONLY packet : Mbuf.T; rt : REFANY);
END UdpOsf.

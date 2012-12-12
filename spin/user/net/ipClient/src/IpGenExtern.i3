(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 05-Mar-96  Marc Fiuczynski (mef) at the University of Washington
 *	I want to move this to the Spin Unix RunTime, but cannot because
 *	IpPktFormat is still part of plexus.  Not sure if I will move it.
 *
 * 04-Mar-95  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *
 *)

UNSAFE (* for externals *)
INTERFACE IpGenExtern;
IMPORT IpPktFormat;
IMPORT Mbuf;

<* EXTERNAL "ip_output_upcall" *>
VAR IpOutput: PROCEDURE(READONLY ip : IpPktFormat.Header; READONLY m: Mbuf.T);

END IpGenExtern.

(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 *)

(* Untrusted *) 
INTERFACE UdpScale;
IMPORT UdpPktFormat;
<* OBSOLETE *> TYPE T = UdpPktFormat.T;
TYPE NewT = UdpPktFormat.NewT;
PROCEDURE Init();
END UdpScale. 

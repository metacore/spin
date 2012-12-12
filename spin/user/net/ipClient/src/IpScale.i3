(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 *)

(* Untrusted *) 
INTERFACE IpScale;
IMPORT IpPktFormat;
<* OBSOLETE *> TYPE T = IpPktFormat.T;
TYPE NewT = IpPktFormat.NewT;
PROCEDURE Init();
END IpScale. 

(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 *)

(* Untrusted *) 
INTERFACE TcpScale;
IMPORT TcpPktFormat;
<* OBSOLETE *> TYPE T = TcpPktFormat.T;
TYPE NewT = TcpPktFormat.NewT;
PROCEDURE Init();
END TcpScale. 

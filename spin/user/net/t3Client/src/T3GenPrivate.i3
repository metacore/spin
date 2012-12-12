(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 *)

UNSAFE (* for externals *)
INTERFACE T3GenPrivate;
IMPORT If;
IMPORT Mbuf;
IMPORT SocketRep;
IMPORT Ctypes;
<* EXTERNAL *> VAR t3_output_upcall: PROCEDURE(
                   VAR ifp: If.ifnet; 
                   m:Mbuf.T; 
                   VAR sa: SocketRep.M3sockaddr; rte:ADDRESS:=NIL):Ctypes.int;
END T3GenPrivate.

(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 06-Nov-95  Marc Fiuczynski (mef) at the University of Washington
 *	Updated to use datastructures from the Spin Unix RunTime.
 *
 *)

UNSAFE INTERFACE An1GenPrivate;
IMPORT Ctypes;
IMPORT If;
IMPORT Mbuf;
IMPORT SocketRep;
<* EXTERNAL *> VAR an1_output_upcall: PROCEDURE(VAR ifp: If.ifnet; 
                   m:Mbuf.T; 
                   VAR sa: SocketRep.M3sockaddr; rte:ADDRESS:=NIL): Ctypes.int;
END An1GenPrivate.

(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 30-Nov-95  Marc Fiuczynski (mef) at the University of Washington
 *	Took out the atm_output_upcall variable.  
 *	Needs to go back after SOSP.
 *
 *)

INTERFACE AtmGenPrivate;
(*
IMPORT Ctypes;
IMPORT If;
IMPORT Mbuf;
IMPORT SocketRep;
<* EXTERNAL fore_output_upcall *> VAR atm_output_upcall: PROCEDURE(VAR ifp: If.ifnet; 
                   m:Mbuf.T; 
                   VAR sa: SocketRep.M3sockaddr; rte:ADDRESS:=NIL): Ctypes.int;
*)
END AtmGenPrivate.

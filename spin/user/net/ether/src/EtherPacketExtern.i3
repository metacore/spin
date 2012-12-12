(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 *)

UNSAFE (* for externals *)
INTERFACE EtherPacketExtern;
IMPORT Mbuf, If, EtherPktFormat, SocketAddr, Ctypes;

<* EXTERNAL "sal_ether_input" *>
VAR ether_input: PROCEDURE(
    ifp: UNTRACED REF If.ifnet;
    READONLY ehp: EtherPktFormat.T; 
    m: Mbuf.T): BOOLEAN;

<* EXTERNAL "sal_ether_output" *>
VAR ether_output: PROCEDURE(
    VAR ifp: If.ifnet;
    m: Mbuf.T;
    VAR s: SocketAddr.T;
    rt: ADDRESS): Ctypes.int;


END EtherPacketExtern.

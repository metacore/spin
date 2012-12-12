(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 *)

INTERFACE An1Arp;
IMPORT Ctypes;
IMPORT IpPktFormat;
IMPORT An1PktFormat;
IMPORT If;
IMPORT Mbuf;
IMPORT SocketRep;
IMPORT Word;

CONST Brand = "An1Arp";

(* PROCEDURE PacketSend(); *)
TYPE
  ProtoAddrT = IpPktFormat.Address;
  HardwareAddrT = An1PktFormat.Address;
  AddressMapping = RECORD
    proType: Ctypes.unsigned_short;
    (*BYTESIZE(ProtoAddrT) must be smaller than BYTESIZE(Word.T) for generic table to work.*)
    protocolAddress : ProtoAddrT; (* this needs to become a SocketRep.sockaddr *)
    hardwareAddress : An1PktFormat.Address;
    ifq             : If.ifqueue;
    resolved        : BOOLEAN;
  END;
  T = REF AddressMapping;

(* XXX there should be a arp resolver for each hardware type, rather than on 
   central arp resolver as implemented here.
 *)

(*CONST
  An1 = 1;
*)

EXCEPTION NotResolved;
EXCEPTION NotInCache;
PROCEDURE Resolve(proType		: Ctypes.unsigned_short; 
                  protocolAddress	: ProtoAddrT; 
                  VAR entry: T;
                 ) RAISES { NotResolved, NotInCache };
PROCEDURE Add(entry:T);
PROCEDURE Delete(entry:T);
PROCEDURE Lookup(
  VAR ifp: If.ifnet; 
  VAR mbuf: Mbuf.T;
  VAR s: SocketRep.M3sockaddr) RAISES { NotResolved} ;

PROCEDURE Init();

(* 
 * For Generic table 
 *)
PROCEDURE Equal(pa1,pa2:  T): BOOLEAN;
PROCEDURE Hash (pa: T): Word.T;

<* EXTERNAL "bcopy" *> PROCEDURE bcopy(s,d:ADDRESS; size:CARDINAL);

END An1Arp.

(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 * 09-Dec-97  David Becker at the University of Washington
 *	Removed myAddr so hosts can have multiple addrs.
 *
 * 31-May-97  David Becker at the University of Washington
 *      Replaced If interface with NetDev/EtherDev
 *
 * 19-Oct-96  Marc Fiuczynski (mef) at the University of Washington
 *	Updated to work with FreeBSD SAL.
 *
 * 06-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed to new style spin shell commands.
 *
 *)

INTERFACE EtherArp;
IMPORT Ctypes, IpPktFormat, EtherPktFormat, If, EtherDev, Mbuf, SocketAddr, Net;

CONST Brand = "EtherArp";

TYPE Status = {Nascent, Permanent, Resolved, Stale, Duplicate};
     StatusSet = SET OF Status;

TYPE
  ProtoAddrT = IpPktFormat.AddressArray;
  HardwareAddrT = EtherPktFormat.Address;
  AddressMapping = RECORD
    protocolAddress : ProtoAddrT;
    (*BYTESIZE(ProtoAddrT) must be smaller than 
      BYTESIZE(Word.T) for generic table to work.*)
    proType: CARDINAL;
    hardwareAddress : EtherPktFormat.Address;
    ifq             : If.ifqueue;
    status          : StatusSet;
  END;
  T = REF AddressMapping;

EXCEPTION NotResolved;
EXCEPTION NotInCache;
TYPE ResolveResult = {OK, NotResolved, NotInCache};
PROCEDURE Resolve(
    proType         : Ctypes.unsigned_short; 
    protocolAddress : IpPktFormat.Address; 
    VAR entry       : T): ResolveResult; (* RAISES { NotResolved, NotInCache }; *)

PROCEDURE Add(VAR entry:T);
PROCEDURE Delete(e:REFANY);
PROCEDURE Lookup(
  dev: EtherDev.T;
  VAR mbuf: Mbuf.T;
  VAR s: SocketAddr.T) RAISES { NotResolved} ;

PROCEDURE Init(verbose:BOOLEAN);
PROCEDURE Uninit(verbose:BOOLEAN);

PROCEDURE CacheDump(proc: PROCEDURE(READONLY entry: AddressMapping));
PROCEDURE CacheDelete(READONLY entry: AddressMapping);

(* XXX stick these into EtherArpPrivate *)
PROCEDURE SendRequest(dev: EtherDev.T; READONLY dst:AddressMapping;
		srcProto: ProtoAddrT; READONLY srcEther: HardwareAddrT);

VAR
  debug_level : Net.Level := Net.oLevel.NODEBUG;
END EtherArp.

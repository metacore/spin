(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 31-May-97  David Becker at the University of Washington
 *      Replaced If interface with NetDev/EtherDev
 *
 * 06-Oct-96  Robert Grimm (rgrimm) at the University of Washington
 *      added call to DispatcherPrivate.KeepStub in Init proc
 *      to ensure that the PacketSend event can be handled.
 *
 * 12-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Converted to new spin shell command style.
 *	Made safe by calling into the If module.
 *)

MODULE EtherGen;

IMPORT Net, EtherArp, IO, Mbuf, EtherPktFormat, IpPktFormat, NetDev,
       SocketAddr, SocketRep, Ctypes, EtherPacket, Dispatcher;
IMPORT EtherDev, EtherGen;
IMPORT DispatcherPrivate; (* XXX get rid of this *)

CONST debug = FALSE;
CONST NETFORMAT_ETHERTYPE_IP     : EtherPktFormat.EtherType= 16_0008;

PROCEDURE PacketSend( 
  dev : NetDev.T;
  mbuf    : Mbuf.T; 
  VAR s   : SocketAddr.T;
  <* UNUSED *> rte     : ADDRESS := NIL) : Ctypes.int =
  VAR error : Ctypes.int;

  PROCEDURE Ip() : Ctypes.int = 
    VAR dst          : EtherArp.T;
        src          : EtherDev.EtherAddr;
        res          : EtherArp.ResolveResult;
        iperror      : Ctypes.int := 0;
        ipdaddrArray : ALIGNED 32 FOR IpPktFormat.AddressArray;
    BEGIN
      TYPECASE dev OF
      | EtherDev.T(eth) =>
        eth.etherAddr(src);
      ELSE
        IO.Put("PacketSend dev not a EtherDev\n");
        Mbuf.m_freem(mbuf);        
        RETURN 0; (* XXX *)
      END;

      (* resolve the destination ip address and if we succeed send the packet *)
      ipdaddrArray := VIEW(s.sa_data,EtherArp.ProtoAddrT); (* XXX copy required due to alignment *)
      WITH ipdaddr = VIEW(ipdaddrArray, IpPktFormat.Address) DO
        res := EtherArp.Resolve(Net.htons(EtherPktFormat.ETHERTYPE_IP),ipdaddr,dst);
      END;
      IF res = EtherArp.ResolveResult.OK THEN 
        WITH header = Mbuf.M_PREPEND(mbuf,BYTESIZE(EtherPktFormat.Header),Mbuf.M_WAIT), 
             ether_data = Mbuf.Array(header), (* get the ether_data area *)
             ether_hdr  = VIEW(ether_data^,EtherPktFormat.T)
         DO
          ether_hdr.dhost := dst.hardwareAddress;
          ether_hdr.shost := src;
          ether_hdr.type  := NETFORMAT_ETHERTYPE_IP;
          iperror         := EtherPacket.Output(dev,header);
        END;
      ELSE
        EtherArp.Lookup(dev,mbuf,s);
        RETURN 0;
      END;
      RETURN iperror;
    END Ip;

  BEGIN

    IF debug AND debug_level # Net.oLevel.NODEBUG THEN
      Net.Debug(debug_level,Net.oLevel.INFO,"EtherGen.PacketSend ");
    END;
    
    CASE s.sa_family OF 
    | SocketRep.AF_INET => (* IP packet assumes that mbuf starts with ip header. *)
      error := Ip();
    | SocketRep.AF_UNSPEC => (* assumes header starts with network layer information *)
      WITH header = Mbuf.M_PREPEND(mbuf,BYTESIZE(EtherPktFormat.Header),Mbuf.M_WAIT), 
           eth_data = Mbuf.Array(header), (* get the ether_data area *)
           eth_out = VIEW(eth_data^,EtherPktFormat.T),
           eth_header = VIEW(s.sa_data,EtherPktFormat.T) (* possible alignment problems *)
       DO
        eth_out := eth_header;
        error := EtherPacket.Output(dev,header);
      END;
    ELSE
      IO.Put("Address Family not supported.\n");
      IF mbuf # NIL THEN 
        Mbuf.m_freem(mbuf);
      END;
      error := 0;
    END;
    RETURN error;
  END PacketSend; 

PROCEDURE Init(verbose:BOOLEAN) = 
  BEGIN
    (* Temporary Hack: forces calls through dispatcher *)
    TRY
      DispatcherPrivate.KeepStub(EtherGen.PacketSend);
    EXCEPT
    | Dispatcher.Error =>
      IO.PutError("EtherGen dispatcher error: can't keep stub\n");
    END;
    IF verbose THEN IO.Put("EtherGen module initialized.\n"); END;
  END Init;

BEGIN
END EtherGen. 

(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *)

MODULE Ether6Gen;

IMPORT Net, IO, Mbuf, EtherPktFormat, NetDev, SocketAddr, SocketRep,
       Ctypes, EtherPacket, Dispatcher, EtherDev;

CONST
  debug = FALSE;

CONST
  NETFORMAT_ETHERTYPE_IP6    : EtherPktFormat.EtherType= 16_dd86;

PROCEDURE PacketSend( 
  dev       : NetDev.T;
  mbuf      : Mbuf.T; 
  VAR s     : SocketAddr.T;
  <*UNUSED*> 
  rte       : ADDRESS := NIL) : Ctypes.int =
  VAR error : Ctypes.int;

  PROCEDURE Ip6() : Ctypes.int = 
    VAR
      dst     : EtherDev.EtherAddr;
      src     : EtherDev.EtherAddr;
      iperror : Ctypes.int := 0;

    BEGIN
      TYPECASE dev OF
      | EtherDev.T(eth) =>
        eth.etherAddr(src);
      ELSE
        IO.Put("PacketSend dev not a EtherDev\n");
        Mbuf.m_freem(mbuf);        
        RETURN 0; (* XXX *)
      END;

      (* XXX need to resolve destination address using ICMPv6 neighbor solicit *)

      (* XXX loom10 destination ethernet address *)
      dst := EtherDev.EtherAddr{16_00,16_60,16_97,16_1b,16_fe,16_a2};

      WITH header = Mbuf.M_PREPEND(mbuf,BYTESIZE(EtherPktFormat.Header),Mbuf.M_WAIT), 
           ether_data = Mbuf.Array(header), (* get the ether_data area *)
           ether_hdr  = VIEW(ether_data^,EtherPktFormat.T)
       DO
        ether_hdr.dhost := dst;
        ether_hdr.shost := src;
        ether_hdr.type  := NETFORMAT_ETHERTYPE_IP6;
        iperror         := EtherPacket.Output(dev,header);
      END;
      
      RETURN iperror;
    END Ip6;

  BEGIN

    IF debug AND debug_level # Net.oLevel.NODEBUG THEN
      Net.Debug(debug_level,Net.oLevel.INFO,"Ether6Gen.PacketSend ");
    END;
    
    CASE s.sa_family OF 
    | SocketRep.AF_INET6 => (* IP packet assumes that mbuf starts with ip header. *)
      error := Ip6();
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

PROCEDURE Init(<*UNUSED*>verbose:BOOLEAN) = 
  BEGIN
  END Init;

BEGIN
END Ether6Gen.

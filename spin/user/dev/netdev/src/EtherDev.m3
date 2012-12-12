UNSAFE (* to import externals *)
MODULE EtherDev EXPORTS EtherDev, EtherDevRep;
IMPORT Mbuf, MbufPublic, Word;
IMPORT EtherDevExtern;

PROCEDURE IpAddr(self: T): INTEGER =
  BEGIN
    RETURN EtherDevExtern.ipaddr(self.ifp);
  END IpAddr;

PROCEDURE EtherHwAddr(self: T; (*OUT*) VAR addr: EtherAddr) =
  BEGIN
    addr := self.hwaddr;
  END EtherHwAddr;

PROCEDURE BsdIfp(self: T): ADDRESS =
  BEGIN
    RETURN self.ifp
  END BsdIfp;

PROCEDURE Mtu(<*UNUSED*>self: T): INTEGER =
  BEGIN
    RETURN 1500; (* ETHERMTU in netinet/if_ether.h *)
  END Mtu;

PROCEDURE Send(self: T; packet:Mbuf.T) =
  BEGIN
    (* XXX temporary hack to make vortex driver happy *)
    IF Word.And(packet.mh_hdr.mh_flags,Mbuf.M_PKTHDR) = 0 THEN
      VAR len: CARDINAL;
          m : Mbuf.T;
      BEGIN
        (* allocate an mbuf header, prepend to packet, and set length *)
        m := Mbuf.m_gethdr(Mbuf.M_DONTWAIT, Mbuf.MT_DATA);
        m.mh_hdr.mh_next := packet;
        m.mh_hdr.mh_len := 0;
        len := Mbuf.m_length(packet);
        MbufPublic.SetPktHdrLen(m,len);
        packet := m;
      END;
    END;
    EtherDevExtern.ifpsend(self.ifp,packet);
  END Send;

PROCEDURE Receive(<*UNUSED*>dev: T; packet: Mbuf.T) =
  BEGIN
    (* Default handler for EtherDev.Receive event.
       The handler of this event responsible for freeing the mbuf. *)
    Mbuf.m_freem(packet)
  END Receive;

BEGIN
END EtherDev.

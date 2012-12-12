INTERFACE EtherDevRep;
IMPORT EtherDev;
IMPORT Device, NameServer, Mbuf, MbufPublic, Word;
IMPORT IO, Text;
IMPORT StcpEtherPacket;
IMPORT Dispatcher;

REVEAL EtherDev.T = EtherDev.Public BRANDED OBJECT
		  hwaddr: EtherDev.EtherAddr;
		  ifp: ADDRESS;
		  OVERRIDES
		    bsdIfp := BsdIfp;
		    send := Send;
		    mtu := Mtu;
		    etherAddr := EtherHwAddr;
		    ipAddr := IpAddr;
		  END;

PROCEDURE BsdIfp(self: EtherDev.T): ADDRESS ;
PROCEDURE Send(self: EtherDev.T; packet:Mbuf.T) ;
PROCEDURE Mtu(<*UNUSED*>self: EtherDev.T): INTEGER ;
PROCEDURE EtherHwAddr(self: EtherDev.T; (*OUT*) VAR addr: EtherDev.EtherAddr) ;
PROCEDURE IpAddr(self: EtherDev.T): INTEGER ;

END EtherDevRep.

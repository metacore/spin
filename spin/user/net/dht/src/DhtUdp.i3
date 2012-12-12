INTERFACE DhtUdp;
IMPORT Mbuf, IpPktFormat, Ip6PktFormat;

PROCEDURE IP6toIP4(
  new_packet   : Mbuf.T;
  READONLY ip6 : Ip6PktFormat.T;
  READONLY ip4 : IpPktFormat.T): Mbuf.T;

PROCEDURE IP4toIP6(
  new_packet   : Mbuf.T;
  READONLY ip4 : IpPktFormat.T;
  READONLY ip6 : Ip6PktFormat.T): Mbuf.T;

END DhtUdp.

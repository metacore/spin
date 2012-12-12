INTERFACE DhtIp;
IMPORT Mbuf, IpPktFormat, Ip6PktFormat;
PROCEDURE IP6toIP4(
    VAR curr       : Mbuf.T; 
    VAR offset     : CARDINAL; 
    VAR ip         : IpPktFormat.T; 
    reverse_lookup : BOOLEAN := FALSE): Mbuf.T;

PROCEDURE IP4toIP6(
    VAR curr       : Mbuf.T; 
    VAR offset     : CARDINAL; 
    VAR ip6        : Ip6PktFormat.T; 
    reverse_lookup : BOOLEAN):Mbuf.T;
END DhtIp.


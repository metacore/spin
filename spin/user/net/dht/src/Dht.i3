INTERFACE Dht;
IMPORT IpPktFormat, Ip6PktFormat, Ctypes;

(* value used to map between v6/v4 addresses *)
CONST Brand = "Dht";
TYPE T = REF RECORD
  ip4 : IpPktFormat.Address;
  ip6 : Ip6PktFormat.Address;
END;

(* used by dht to filter out v4 packets that should be translated and
forwarded *)
TYPE T4 = REF RECORD
  addr : IpPktFormat.Address;
  mask : IpPktFormat.Address;
END;

(* used by dht to filter out v6 packets that should be translated and
forwarded *)
TYPE T6 = REF RECORD
  addr : Ip6PktFormat.Address;
  mask : Ip6PktFormat.Address;
END;

PROCEDURE IP4toIP6Checksum(
  READONLY ip4 : IpPktFormat.T; (* fields in network order *)
  READONLY ip6 : Ip6PktFormat.T;  (* fields in host order *)
  prevcsum     : Ctypes.unsigned_short) : Ctypes.unsigned_short;

PROCEDURE IP6toIP4Checksum(
  READONLY ip6 : Ip6PktFormat.T;  (* fields in host order *)
  READONLY ip4 : IpPktFormat.T; (* fields in network order *)
  prevcsum     : Ctypes.unsigned_short) : Ctypes.unsigned_short;

PROCEDURE FmtIp(ip:Ip6PktFormat.Address): TEXT;
PROCEDURE CreateIP6NODE4addr(
    ip4node4: IpPktFormat.Address;
    VAR ip6node4 : Ip6PktFormat.Address);
END Dht.



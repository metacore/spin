(*
 * HISTORY
 *)

INTERFACE Ip6Route;

IMPORT NetDev, Mbuf, SocketRep, Ctypes;
TYPE
  HardwareFmtProc = PROCEDURE(dev: NetDev.T; m: Mbuf.T; VAR s: SocketRep.sockaddr; rt: ADDRESS):Ctypes.int;
TYPE Route = RECORD
  dev        : NetDev.T;
  PacketSend : HardwareFmtProc;
END;  
TYPE T = REF Route;

END Ip6Route.

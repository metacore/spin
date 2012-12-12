(*
 * HISTORY
 * 31-May-97  David Becker at the University of Washington
 *      Replaced If interface with NetDev/EtherDev
 *
 * 15-Aug-95  Marc Fiuczynski (mef) at the University of Washington
 *	Dummy step towards real routing support.
 *
 *)


INTERFACE IpRoute;
IMPORT NetDev;
IMPORT IpPktFormat;
IMPORT Ctypes;
IMPORT Mbuf;
(*IMPORT Word;*)
IMPORT SocketRep;


TYPE Status = {Host, Net, Gateway};
     StatusSet = SET OF Status;
     HardwareFmtProc = PROCEDURE(dev: NetDev.T; m: Mbuf.T; VAR s: SocketRep.sockaddr; rt: ADDRESS):Ctypes.int;

TYPE Route = RECORD
  dev: NetDev.T;
  src,dst    : ALIGNED 32 FOR IpPktFormat.Address;
  PacketSend : HardwareFmtProc;
  status     : StatusSet;
END;  
TYPE T = REF Route;

PROCEDURE Lookup(dst:IpPktFormat.Address):T;

PROCEDURE NewIfHandler(dev: NetDev.T; out: HardwareFmtProc; target,route: IpPktFormat.Address):BOOLEAN ;

CONST Brand = "IpRoute";

PROCEDURE Init(verbose:BOOLEAN);

END IpRoute.

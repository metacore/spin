(*
 * HISTORY
 * 13-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *
 *)

INTERFACE IpRouteKey;
IMPORT IpPktFormat;
IMPORT Word;

TYPE T = IpPktFormat.Address;
(*
TYPE T = RECORD
  src : IpPktFormat.Address;
  dst : IpPktFormat.Address;
END;
*)

(* 
 * For Generic table 
 *)
PROCEDURE Equal(pa1: T; pa2: T): BOOLEAN;
PROCEDURE Hash (pa: T): Word.T;
CONST Brand = "IpRouteKey";
END IpRouteKey.

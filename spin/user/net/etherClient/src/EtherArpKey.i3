(* 
 * HISTORY
 * 13-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *
 *)

INTERFACE EtherArpKey;
IMPORT IpPktFormat;
IMPORT Word;

(*
TYPE T = EtherArp.AddressMapping;
*)

TYPE T = IpPktFormat.Address;
(* For Generic table *)
PROCEDURE Equal(pa1: T; pa2: T): BOOLEAN;
PROCEDURE Hash (pa: T): Word.T;

CONST Brand = "EtherArpKey";
END EtherArpKey.

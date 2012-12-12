INTERFACE DhtIP4;
IMPORT IpPktFormat, Word;
(* For Generic table *)
TYPE T = IpPktFormat.Address;
PROCEDURE Equal(READONLY pa1: T; READONLY pa2: T): BOOLEAN;
PROCEDURE Hash (READONLY pa: T): Word.T;
CONST Brand = "DhtIP4";
END DhtIP4.

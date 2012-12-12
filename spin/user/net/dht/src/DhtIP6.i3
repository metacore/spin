INTERFACE DhtIP6;
IMPORT Ip6PktFormat, Word;
(* For Generic table *)
TYPE T = Ip6PktFormat.Address;
PROCEDURE Equal(READONLY pa1: T; READONLY pa2: T): BOOLEAN;
PROCEDURE Hash (READONLY pa: T): Word.T;
CONST Brand = "DhtIP6";
END DhtIP6.

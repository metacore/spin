(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 * 20-Jun-97  Tsutomu Owa (owa) at the University of Washington
 *	Fixed checksum()
 *
 * 09-Oct-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.  FreeBSD dependent checksum routine.
 *)

UNSAFE
MODULE NetDep EXPORTS Net;
IMPORT NetExtern, Ctypes, Word;

(* EPHEMERAL *)
PROCEDURE checksum(
    READONLY 
    packet   : ARRAY OF CHAR;
    size     : CARDINAL := 0; 
    prevcsum : Ctypes.unsigned_short := LAST(Ctypes.unsigned_short))
  :Ctypes.unsigned_short = 
  VAR
    ck:Ctypes.unsigned_short;
  BEGIN
    (* allow client to checksum less than what's in the array *)
    WITH len = NUMBER(packet) DO
      IF size =  0 THEN
        size := len;
      ELSIF size > len THEN
        size := len;
      END;      
    END;

    WITH addr = LOOPHOLE(ADR(packet[FIRST(packet)]),ADDRESS) DO
      prevcsum := Word.And(Word.Not(prevcsum),16_ffff);
      ck := NetExtern.in_checksum(addr,size,prevcsum);
    END;
    RETURN ck;
  END checksum; 

BEGIN
END NetDep.

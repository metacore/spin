(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
 * HISTORY
 * 07-Jun-97  Tsutomu Owa (owa) at the University of Washington
 *	Fixed a minor bug in checksum()
 *
 * 09-Apr-97  Tsutomu Owa (owa) at the University of Washington
 *	Change interface names so that these don't conflict w/
 *	those used by user/net.
 *
 * 03-Feb-97  Tsutomu Owa (owa) at the University of Washington
 *	Copied from user/urt/urtcore/src/ALPHA_SPIN
 * 09-Oct-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *
 *)

UNSAFE
MODULE StcpNetDep EXPORTS StcpNet;
IMPORT StcpNetExtern, Ctypes, Word;

(* EPHEMERAL *)
PROCEDURE checksum(
    READONLY 
    packet   : ARRAY OF CHAR;
    size     : CARDINAL := 0; 
    prevcsum : Ctypes.unsigned_short := LAST(Ctypes.unsigned_short)):Ctypes.unsigned_short = 
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
      (* XXX always convert checksum to it's compliment
      IF prevcsum # 0 THEN
        prevcsum := Word.And(Word.Not(prevcsum),16_ffff);
      END;
      *)
      prevcsum := Word.And(Word.Not(prevcsum),16_ffff);
      ck := StcpNetExtern.in_checksum(addr,size,prevcsum);
    END;
    ck := Word.And(Word.Not(ck),16_ffff);
    RETURN ck;
  END checksum; 

BEGIN
END StcpNetDep.

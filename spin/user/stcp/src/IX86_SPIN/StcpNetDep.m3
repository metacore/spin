(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 * 07-June-97  Tsutomu Owa (owa) at the University of Washington
 *	Fixed a minor bug in checksum()
 *
 * 12-Apr-97  Tsutomu Owa (owa) at the University of Washington
 *	Change interface names so that these don't conflict w/
 *	those used by user/net.
 *
 * 03-Mar-97  Tsutomu Owa (owa) at the University of Washington
 *	Copied from user/urt/urtcore/src/IX86_SPIN.
 *
 * 09-Oct-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.  FreeBSD dependent checksum routine.
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
      (* IF prevcsum # 0 THEN
        prevcsum := Word.And(Word.Not(prevcsum),16_ffff);
      END;
      *)
      prevcsum := Word.And(Word.Not(prevcsum),16_ffff);
      ck := StcpNetExtern.in_checksum(addr,size,prevcsum);
    END;
    RETURN ck;
  END checksum; 

BEGIN
END StcpNetDep.

(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 27-Aug-96  Yasushi Saito (yasushi) at the University of Washington
 *	Made a record to save memory.
 * 01-Jul-96  Yasushi Saito (yasushi) at the University of Washington
 *	Made a ref record to save memory
 * 13-Feb-95  Stefan Savage (savage) at the University of Washington
 *	Created
 *
 *)

INTERFACE CacheBlock;
IMPORT PhysAddr;
IMPORT Translation;

(* "T" augments the phys frame. In addition to [memobj, offset] pair
   recorded in the frame tag, "T" records the protection and cow flag.
 *)


TYPE
  T = RECORD
    frame: PhysAddr.T;
    prot: BITS 3 FOR Translation.Protection;
    copyOnWrite: BITS 1 FOR BOOLEAN;
  END;

CONST Brand = "CacheBlock";
PROCEDURE Print(self: T): TEXT;
  
END CacheBlock.

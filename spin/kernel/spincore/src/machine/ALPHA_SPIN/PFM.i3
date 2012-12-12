(*
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 21-Feb-97  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

(* Alpha performance monitor interface. See also pfm(7). *)

INTERFACE PFM;

IMPORT Ctypes;
IMPORT Word;

VAR
  active: BOOLEAN;
  
TYPE
  PCount = RECORD
    pc: Word.T := 0;
    count: CARDINAL := 0;
  END;
  
  Iccsr = RECORD
    pc1: BITS 1 FOR [0..1];
    ign0: BITS 2 FOR [0..0];
    pc0: BITS 1 FOR [0..1];
    ign1: BITS 4 FOR [0..0];
    mux0: BITS 4 FOR [0..15];
    ign2: BITS 20 FOR [0..16_FFFFF];
    mux1: BITS 3 FOR [0..7];
    disable: BITS 2 FOR [0..3];
    ign3: BITS 27 FOR [0 .. 16_7ffffff];
  END;
      
  Counter = RECORD
    cntr0: Ctypes.unsigned_long; (* drive counter 0 *)
    cntr1: Ctypes.unsigned_long; (* drive counter 0 *)
    cycle: Ctypes.unsigned_long; (* pcc *)
  END;
      
  AddrRange = RECORD
    start, end: Word.T;
  END;
      
CONST 
  PCNTOPENALL = 16_18000;

  (* mux0 definitions *)
  PF_ISSUES	 = 16_0;	(* total issues/2			*)
  PF_PIPEDRY	 = 16_2;	(* pipeline dry, ie lack of valid i-str	*)
  PF_LOADI	 = 16_4;	(* load instructions			*)
  PF_PIPEFROZEN	 = 16_6;	(* pipeline frozen, resource conflicts	*)
  PF_BRANCHI	 = 16_8;	(* branch, and function calls		*)
  PF_CYCLES	 = 16_a;	(* total cycles				*)
  PF_PALMODE	 = 16_b;	(* cycles while executing palcode	*)
  PF_NONISSUES	 = 16_c;	(* non_issues / 2			*)
  PF_EXTPIN0	 = 16_e;	(* external pin, platform specific	*)

  (* mux1 definitions *)
  PF_DCACHE	 = 16_0;	(* data cache misses			*)
  PF_ICACHE	 = 16_1;	(* instruction cache misses		*)
  PF_DUAL	 = 16_2;	(* dual issue				*)
  PF_BRANCHMISS	 = 16_3;	(* branch mispredictions		*)
  PF_FPINST	 = 16_4;	(* floating point operate instructions	*)
  PF_INTOPS	 = 16_5;	(* integer operations			*)
  PF_STOREI	 = 16_6;	(* store instructions			*)
  PF_EXTPIN1	 = 16_7;	(* external pin, platform specific	*)

(*
 * These are the items that can be counted.
 * Set them with PCNTSETITEMS ioctl().
 *)
  PFM_COUNTERS	= 1;	(* Counters only (default) *)
  PFM_IPL	= 2;	(* Collect IPL statistics *)
  PFM_PROFILING	= 4;	(* User or Kernel PC samples *)


PROCEDURE Open(mode: Word.T);
PROCEDURE Close();
PROCEDURE Enable();
PROCEDURE Disable();
PROCEDURE SetItems(mode: INTEGER); (* bit-or of PFM_XXX *)
PROCEDURE ClearCount();
PROCEDURE GetCount(VAR c: Counter);
PROCEDURE SetMux(VAR c: Iccsr);
PROCEDURE SetKaddr(READONLY r: AddrRange);
PROCEDURE SetUaddr(READONLY r: AddrRange);
PROCEDURE GetBufferSize(): CARDINAL;
PROCEDURE Read(VAR p: ARRAY OF PCount): CARDINAL;
END PFM.





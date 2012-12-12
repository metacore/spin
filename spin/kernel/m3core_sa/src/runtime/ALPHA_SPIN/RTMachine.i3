(* Copyright (C) 1994, Digital Equipment Corporation.       *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Sat Nov 19 09:17:39 PST 1994 by kalsow  *)

(* This interface defines platform (machine + OS) dependent
   types and constants. *)

(*
 * HISTORY
 * 18-Nov-97  Tian Fung Lim (tian) at the University of Washington
 *	Moved RT0.Header here, because the layout is machine-specific.
 *
 * 14-Oct-97  Tian Fung Lim (tian) at the University of Washington
 *	Changed hash field width to 0 so header field is 32 bits wide.
 *
 * 14-Nov-96  Przemek Pardyak (pardy) at the University of Washington
 *	VM support for GC enabled.
 *
 * 20-Mar-96  Wilson Hsieh (whsieh) at the University of Washington
 *	added PreferredAlignment.
 *
 *)

INTERFACE RTMachine;

(*IMPORT Csetjmp, Usignal;*)
IMPORT Csetjmp;

(*--------------------------------------------------------- thread state ---*)

TYPE
  State = Csetjmp.jmp_buf;
  (* The machine state is saved in a "State".  This type is really
     opaque to the client, i.e. it does not need to be an array. *)

PROCEDURE SaveState (VAR s: State): INTEGER;
(* Capture the currently running thread's state *)

CONST
  FramePadBottom = 20;
  FramePadTop    = 20;
  (* Additional padding words from above and below an existing
     thread's stack pointer to copy when creating a new thread *)

(*------------------------------------------------------------------ heap ---*)

(* The heap page size is machine-dependent, since it might depend on the
   architecture's VM page size (if VM is TRUE).  Otherwise, 8192 bytes is a
   reasonable page size.  The page size must be a power of two. *)

CONST
  BytesPerHeapPage    = 8192;        (* bytes per page *)
  LogBytesPerHeapPage = 13;
  AdrPerHeapPage      = 8192;        (* addresses per page *)
  LogAdrPerHeapPage   = 13;

(* The collector supports the use of VM protection to achieve incremental,
   generational collection.  This is not possible on all architectures, and
   it may not be implemented in all cases where it is possible.  The
   boolean constant "VMHeap" is "TRUE" iff all necessary support is
   present for this architecture.  "VMHeap" is "TRUE" for the DS3100,
   whose implementation you might use as a reference. *)

CONST
  VMHeap = TRUE;

(*--------------------------------------------------------- thread stacks ---*)

CONST
  PointerAlignment = 8;
  (* The C compiler allocates all pointers on 'PointerAlignment'-byte
     boundaries.  The garbage collector scans thread stacks, but only
     looks at these possible pointer locations.  Setting this value
     smaller than is needed will only make your system run slower.
     Setting it too large will cause the collector to collect storage
     that is not free. *)

CONST
  StackFrameAlignment = 8;
  (* Stack frames must be aligned to this constraint (in ADRSIZE units). 
     It's not a big deal if this value is too large, but it may break 
     the thread mechanism to make it too small. *)

CONST
  PreferredAlignment = 64;
  (* preferred alignment in bits for packed data structures that have
     their alignment boosted *)

(*--------------------------------------------------------- header info ---*)

CONST
  MaxReferentSize = 16_ffffffff;
  MaxHash = 16_0;
  LogMaxReferentSize = 32;
  LogMaxHash = 0;

  NB = 1 + LogMaxReferentSize + LogMaxHash;
  (* 4 bits for copying collector
     20 bits for typecode
     NB for size/hash -- is 34
     7 for reference counting collector
   *)
  SB = BITSIZE (ADDRESS) - 4 - 20 - NB - 7; (* # spare bits in a ref header *)
  LowRef = 0;
  HighRef = 31;
  MidRef = 16;


TYPE
  Header = RECORD
    forwarded : BITS  1 FOR BOOLEAN  := FALSE; (* used during collection *)
    weak      : BITS  1 FOR BOOLEAN  := FALSE; (* any weakrefs? *)
    marka     : BITS  1 FOR BOOLEAN  := FALSE; (* used during collection *)
    markb     : BITS  1 FOR BOOLEAN  := FALSE; (* used during collection *)
    typecode  : BITS 20 FOR [0..16_FFFFF] := 0;     (* the typecode *)
    immobile  : BITS  1 FOR BOOLEAN  := FALSE; (* used during collection *)
    (* reference counting fields *)
    (* see SRC Report 64 for documentation, we are using the same meanings *)
    Z         : BITS   1 FOR BOOLEAN  := FALSE; (* on Zero Count List *)
    V         : BITS   1 FOR BOOLEAN  := FALSE; (* refcount overflow  *)
    refcount  : BITS   5 FOR [LowRef .. HighRef] := 0;
    spare     : BITS SB FOR [0 .. 0] := 0;     (* for future expansion *)
    hash      : BITS LogMaxHash FOR [0..MaxHash] := 0;
    size      : BITS LogMaxReferentSize FOR [0..MaxReferentSize] := 0;
  END;

(*----------------------------------------------- exception stack walking ---*)
(* The "FrameInfo" type must minimally include fields named "pc" and "sp". *)

TYPE
  FrameInfo = RECORD
    pc  : ADDRESS;
    sp  : ADDRESS;
    (* cxt : Usignal.struct_sigcontext; *)  (* SPIN *)
    lock: INTEGER;  (* to ensure that cxt isn't overrun!! *)
  END;

END RTMachine.












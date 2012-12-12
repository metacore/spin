(*
 *
 * Copyright 1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *)

(*
 * HISTORY
 * 27-Jan-97  Wilson Hsieh (whsieh) at the University of Washington
 *	created
 *
 *)


UNSAFE INTERFACE RTMachineCollectorExtern;

IMPORT Word;

(* reference counting collector interface *)

<* EXTERNAL *> 
VAR
  TransactionQueue: ADDRESS;
  TransactionQueueEnd: ADDRESS;
  (* the beginning and end of the log buffer *)

(* RAS regions *)
<* EXTERNAL *> PROCEDURE UpdateRC ();
<* EXTERNAL *> PROCEDURE EnqueueRC1 ();
<* EXTERNAL *> PROCEDURE EnqueueRC2 ();

(* lengths of RAS regions *)
CONST
  UpdateRCRASLength = 2;
  EnqueueRC1RASLength = 5;
  EnqueueRC2RASLength = 5;

<* EXTERNAL *> PROCEDURE AssignKnown (old, new: ADDRESS);

(* tracing variables *)
<* EXTERNAL *>
VAR NilOld, NilNew, OldEqualNew: Word.T;

<* EXTERNAL *>
VAR
  AllocationClock,
  GlobalClock, GlobalTotal, GlobalTotal2, GlobalCount,
  LocalClock, LocalTotal, LocalTotal2, LocalCount,
  HeapClock, HeapTotal, HeapTotal2, HeapCount,
  PossibleHeap, PossibleGlobal, PossibleLocal
  : Word.T;

(* RAS regions            *)
<* EXTERNAL *> PROCEDURE TraceRAS0 ();
<* EXTERNAL *> PROCEDURE TraceRAS1 ();
<* EXTERNAL *> PROCEDURE TraceRAS2 ();
<* EXTERNAL *> PROCEDURE TraceRAS3 ();
<* EXTERNAL *> PROCEDURE TraceRAS4 ();
<* EXTERNAL *> PROCEDURE TraceRAS5 ();
<* EXTERNAL *> PROCEDURE TraceRAS6 ();
<* EXTERNAL *> PROCEDURE TraceRAS7 ();
<* EXTERNAL *> PROCEDURE TraceRAS8 ();
<* EXTERNAL *> PROCEDURE GlobalRAS0 ();
<* EXTERNAL *> PROCEDURE GlobalRAS1 ();
<* EXTERNAL *> PROCEDURE GlobalRAS2 ();
<* EXTERNAL *> PROCEDURE GlobalRAS3 ();
<* EXTERNAL *> PROCEDURE GlobalRAS4 ();
<* EXTERNAL *> PROCEDURE GlobalRAS5 ();
<* EXTERNAL *> PROCEDURE GlobalRAS6 ();
<* EXTERNAL *> PROCEDURE GlobalRAS7 ();
<* EXTERNAL *> PROCEDURE GlobalRAS8 ();
<* EXTERNAL *> PROCEDURE CountRAS1 ();
<* EXTERNAL *> PROCEDURE CountRAS2 ();
<* EXTERNAL *> PROCEDURE CountRAS3 ();
<* EXTERNAL *> PROCEDURE CountRAS4 ();
<* EXTERNAL *> PROCEDURE CountRAS5 ();

CONST
  TraceRAS0Length = 2;
  TraceRAS1Length = 3;
  TraceRAS2Length = 3;
  TraceRAS3Length = 3;
  TraceRAS4Length = 3;
  TraceRAS5Length = 3;
  TraceRAS6Length = 3;
  TraceRAS7Length = 3;
  TraceRAS8Length = 3;
  GlobalRAS0Length = 2;
  GlobalRAS1Length = 3;
  GlobalRAS2Length = 3;
  GlobalRAS3Length = 3;
  GlobalRAS4Length = 3;
  GlobalRAS5Length = 3;
  GlobalRAS6Length = 3;
  GlobalRAS7Length = 3;
  GlobalRAS8Length = 3;
  CountRAS1Length = 3;
  CountRAS2Length = 3;
  CountRAS3Length = 3;
  CountRAS4Length = 3;
  CountRAS5Length = 3;

<* EXTERNAL *> PROCEDURE TraceRefHeap (addr, rhs: ADDRESS;
                                       tc: INTEGER (*RT0.Typecode*);
                                       definite: BOOLEAN) : ADDRESS;

<* EXTERNAL *> PROCEDURE TraceRefGlobal (addr, rhs: ADDRESS;
                                         tc: INTEGER (*RT0.Typecode*);
                                         definite: BOOLEAN) : ADDRESS;

END RTMachineCollectorExtern.

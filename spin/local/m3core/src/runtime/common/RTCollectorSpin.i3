

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


INTERFACE RTCollectorSpin;

(* reference counting collector interface *)

TYPE
  Transaction = RECORD
    lhs, rhs: ADDRESS;
  END;

  TransactionBuffer = UNTRACED REF Transaction;

<* EXTERNAL *>
VAR
  TransactionQueue: TransactionBuffer;
  Enqueue, EnqueueEnd: ADDRESS;  (* RAS region *)

END RTCollectorSpin.

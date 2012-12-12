

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


UNSAFE INTERFACE RTCollectorSpinExtern;

(* reference counting collector interface *)

IMPORT RTCollectorSpin;

(* current place to write *)
<* EXTERNAL *> VAR TransactionQueue: UNTRACED REF RTCollectorSpin.Transaction;

<* EXTERNAL *> PROCEDURE AssignKnown (old, new: ADDRESS);

END RTCollectorSpinExtern.

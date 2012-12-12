(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *)

(* The sole purpose of this interface is to reveal Transaction.T.
 We can't merge this into TransPrivate because it imports TransQ, which
 is defined on Transaction.T. *)

INTERFACE TransT;

TYPE T <: ROOT;

END TransT.

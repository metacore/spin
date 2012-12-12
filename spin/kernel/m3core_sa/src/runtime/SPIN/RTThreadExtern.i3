(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 30-Oct-95  Charlie Garrett (garrett) at the University of Washington
 *      Created.
 *)

UNSAFE INTERFACE RTThreadExtern;

FROM RTThread IMPORT State;

<*EXTERNAL*> PROCEDURE RTThread__Transfer (VAR from, to: State);
  (* Records the current machine state in "from" and sets
     the machine state to that contained in "to". *)

<*EXTERNAL*> VAR RTThread__handlerStack: ADDRESS;
(* linked list of exception frames. *)

END RTThreadExtern.

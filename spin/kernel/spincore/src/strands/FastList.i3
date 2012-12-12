(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *
 * 28-Aug-97  Robert Grimm (rgrimm) at the University of Washington
 *      Added an untraced type to document the security managers
 *      dependence on fast lists. Also, note that the typesafety
 *      argument below is wrong (see auth/SIDStack.i3).
 *
 * 06-Oct-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Fast list implementation based on RAS.
 *)
INTERFACE FastList;

TYPE
  T = OBJECT  (* list elements should derive from this type *)
    nextelem: T := NIL;
  END;
  UT = UNTRACED ROOT OBJECT
    nextelem: UT := NIL;
  END;

(* Because of typesafety restrictions, we cannot pass VAR args below *)
(* and must use REFs instead. *)

(*
 * Atomically dequeue a member from the front of the list.
 *)
EPHEMERAL
PROCEDURE Dequeue(list: REF T) : T;
(* 
 * BEGIN
 *   t := head;
 *   IF t = NIL THEN RETURN NIL; END;
 *   head := head.nextelem;
 *   RETURN t;
 * END;
 *)

(*
 * Atomically enqueue a member to the front of the list.
 *)
EPHEMERAL
PROCEDURE Enqueue(elem: T; list: REF T);
(* 
 * BEGIN
 *   elem.nextelem := list;
 *   list := elem;
 * END;
 *)

END FastList.

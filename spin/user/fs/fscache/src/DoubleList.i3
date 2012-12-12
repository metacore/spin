(*
 * Copyright 1994-1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 21-Jun-97  David Dion (ddion) at the University of Washington
 *	Created.
 *
 *)

(* DoubleList implements a doubly-linked list.  All operations are
   guarded by locks and hence atomic (except init). *)

INTERFACE DoubleList;

TYPE 
  T <: TPublic;
  TPublic = BRANDED OBJECT
  METHODS
    init() : T; 
    addTail(elt: EltT) RAISES { InList };
    addHead(elt: EltT) RAISES { InList };
    remove(elt: EltT) RAISES { NotInList };
    removeHead() : EltT RAISES { Empty };
    removeTail() : EltT RAISES { Empty };
    moveToHead(elt: EltT) RAISES { NotInList };
    moveToTail(elt: EltT) RAISES { NotInList };
    isEmpty() : BOOLEAN;
    isMember(elt: EltT) : BOOLEAN;
  END;

  EltT <: EltTPublic;
  EltTPublic = BRANDED OBJECT
  METHODS
    init() : EltT; 
  END;

EXCEPTION Empty;
EXCEPTION InList;
EXCEPTION NotInList;

END DoubleList.

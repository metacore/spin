(*
 *
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 *)


GENERIC MODULE Queue(Elem);

TYPE
  Link = REF RECORD
    e    : Elem.T;
    next : Link;
  END;

REVEAL
  T = BRANDED REF RECORD
    head, tail : Link;
  END;

PROCEDURE NewQ(): T =
  BEGIN
    RETURN NEW(T, head := NIL, tail := NIL);
  END NewQ;

PROCEDURE Enqueue(q: T; e: Elem.T) =
  BEGIN
    IF q.tail = NIL THEN
      (* Queue was empty *)
      q.head := NEW(Link, e := e, next := NIL);
      q.tail := q.head;
    ELSE
      q.tail.next := NEW(Link, e := e, next := NIL);
      q.tail := q.tail.next;
    END;
  END Enqueue;

PROCEDURE EnqueueUnique(q: T; e: Elem.T) =
  VAR
    link : Link := q.head;
  BEGIN
    
    WHILE link # NIL DO
      IF link.e = e THEN
        RETURN;
      END;
      link := link.next;
    END;

    Enqueue(q, e);
  END EnqueueUnique; 

PROCEDURE Dequeue(q: T): Elem.T =
  VAR
    result : Elem.T;
  BEGIN
    IF q.head # NIL THEN
      result := q.head.e;
      q.head := q.head.next;
      IF q.head = NIL THEN
        (* Queue is now empty *)
        q.tail := NIL;
      END;
    END;

    RETURN result;
  END Dequeue; 

PROCEDURE IsEmpty(q: T): BOOLEAN =
  BEGIN
    RETURN q.head = NIL;
  END IsEmpty;

BEGIN
END Queue.

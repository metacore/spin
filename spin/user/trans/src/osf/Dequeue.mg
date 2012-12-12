(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)

GENERIC MODULE Dequeue(Elem);

PROCEDURE NewHeader (): T =
  VAR head: T;
  BEGIN
    IF mu = NIL THEN
      (* HACK HACK NewHeader may be called from module initialization code
       when this module is not initialized yet. *)
      head := NEW(T);
    ELSE
      head := Allocate();
    END;
    
    head.next := head;
    head.prev := head;
    RETURN head;
  END NewHeader;

(* See if the queue "t" is sane. It sees if links are connected properly *)
PROCEDURE Sane (t: T): BOOLEAN =
  VAR h := t;
  BEGIN
    WHILE t.next # h DO
      IF t # t.next.prev OR t # t.prev.next THEN
	RETURN FALSE;
      END;
      t := t.next;
    END;
    RETURN TRUE;
  END Sane;

(* Insert B next to A *)
PROCEDURE InsertHead (a: T; b: T) =
  BEGIN
    (* Links are nuked when it is removed from the list "t" previously
     belonged to. *)
    <*ASSERT b.next = NIL AND b.prev = NIL *>
    b.prev := a;
    b.next := a.next;
    a.next.prev := b;
    a.next := b;
  END InsertHead;

PROCEDURE InsertTail (a: T; b: T) =
  BEGIN
    <*ASSERT b.next = NIL AND b.prev = NIL*>
    b.next := a;
    b.prev := a.prev;
    a.prev.next := b;
    a.prev := b;
  END InsertTail;

PROCEDURE NukeLinks (t: T): BOOLEAN =
  BEGIN
    <*ASSERT t.next # NIL AND t.prev # NIL*>
    t.next := NIL;
    t.prev := NIL;
    RETURN TRUE;
  END NukeLinks;
  
PROCEDURE Remove (t: T) =
  BEGIN
    <*ASSERT Sane(t)*>
    t.next.prev := t.prev;
    t.prev.next := t.next;
    <*ASSERT NukeLinks(t)*>
  END Remove;
  
PROCEDURE RemoveHead (t: T): T =
  VAR elem: T; 
  BEGIN
    <*ASSERT Sane(t)*>
    elem := t.next;
    elem.next.prev := elem.prev;
    elem.prev.next := elem.next;
    <*ASSERT NukeLinks(elem)*>
    RETURN elem;
  END RemoveHead;

PROCEDURE RemoveHeadSafe (t: T): T =
  VAR elem: T;
  BEGIN
    <*ASSERT Sane(t)*>
    IF t.next = t THEN
      (* queue is empty. *)
      RETURN NIL;
    ELSE
      elem := t.next;
      elem.next.prev := elem.prev;
      elem.prev.next := elem.next;
      <*ASSERT NukeLinks(elem)*>
      RETURN elem;
    END;
  END RemoveHeadSafe;
  
PROCEDURE RemoveTail (t: T): T =
  VAR elem: T; 
  BEGIN
    <*ASSERT Sane(t)*>
    elem := t.prev;
    elem.next.prev := elem.prev;
    elem.prev.next := elem.next;
    <*ASSERT NukeLinks(elem)*>
    RETURN elem;
  END RemoveTail;

PROCEDURE Member (head: T; t: T): BOOLEAN =
  VAR cur := head.next;
  BEGIN
    <*ASSERT Sane(head)*>
    WHILE cur # head DO
      IF cur = t THEN RETURN TRUE; END;
      cur := cur.next;
    END;
    <*ASSERT Sane(head)*>
    RETURN FALSE;
  END Member;
  
PROCEDURE Empty (t: T): BOOLEAN =
  BEGIN
    <*ASSERT Sane(t)*>
    IF t.prev = t THEN
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END Empty;
  
PROCEDURE Size (head: T): INTEGER =
  VAR
    i := Iterate(head);
    t: T;
    size := 0;
  BEGIN
    WHILE NextItr(i, t) DO
      INC(size);
    END;
    RETURN size;
  END Size;



PROCEDURE Iterate (head: T): Iterator =
  BEGIN
    RETURN Iterator{head := head, cur:= head.next};
  END Iterate;

PROCEDURE NextItr (VAR itr: Iterator; VAR t: T): BOOLEAN =
  BEGIN
    t := itr.cur;
    IF t = itr.head THEN RETURN FALSE; END;
    itr.cur := itr.cur.next;
    RETURN TRUE;
  END NextItr;

VAR
  mu: MUTEX;
  free: T;
  
PROCEDURE Allocate (): T =
  VAR elem: T;
  BEGIN
    LOCK mu DO
      IF free.prev = free THEN
	(* The free list is empty.
	   Note:I don't use Empty() to save proc call
	   overhead *)
	RETURN NEW(T);
      END;
      
      <*ASSERT Sane(free)*>
      elem := free.next;
      elem.next.prev := elem.prev;
      elem.prev.next := elem.next;
      <*ASSERT NukeLinks(elem)*>
      RETURN elem;
    END;
  END Allocate;

PROCEDURE Free (t: T) =
  BEGIN
    (* Either the queue is nil, or it't empty. *)
    <*ASSERT (t.next = NIL AND t.prev = NIL) OR (t.next = t AND t.prev = t)*>
    LOCK mu DO
      t.prev := free;
      t.next := free.next;
      free.next.prev := t;
      free.next := t;
    END;
  END Free;
  
BEGIN
  mu := NEW(MUTEX);
  (* create an empty dequeue *)
  free := NEW(T);
  free.next := free;
  free.prev := free;
END Dequeue.
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
MODULE DoubleList;

REVEAL 
  T = TPublic BRANDED OBJECT
    head: EltT;
    tail: EltT;
    listmu : MUTEX;
  OVERRIDES
    init := InitT;
    addTail := AddTail;
    addHead := AddHead;
    remove := Remove;
    removeHead := RemoveHead;
    removeTail := RemoveTail;
    moveToHead := MoveToHead;
    moveToTail := MoveToTail;
    isEmpty := IsEmpty;
    isMember := IsMember;
  END;

  EltT = EltTPublic BRANDED OBJECT
    prev: EltT;
    next: EltT;
    list: T;
    eltmu: MUTEX;
  OVERRIDES
    init := InitEltT;
  END;

(* DoubleList.m3 locking policy: when adding or removing an element from
   a list, always acquire the list lock first.  When changing the list
   membership of an element, must acquire lock on that element.  Otherwise,
   when accessing next or prev pointers of an element in a list, do not
   need to acquire element lock. *)


PROCEDURE InitT(self: T) : T =
  BEGIN
    self.head := NIL;
    self.tail := NIL;
    self.listmu := NEW(MUTEX);
    RETURN self;
  END InitT;

PROCEDURE AddTail(self: T; elt: EltT) RAISES { InList } =
  BEGIN
    IF elt = NIL THEN
      RETURN;
    END;

    LOCK self.listmu DO
      LOCK elt.eltmu DO
        (* check if element already in a list *)
        IF elt.list # NIL THEN
          RAISE InList;
        END;

        (* empty list *)
        IF self.head = NIL THEN
          elt.list := self;
          elt.prev := NIL;
          elt.next := NIL;
          self.head := elt;
          self.tail := elt;
        (* non-empty list *)
        ELSE
          elt.list := self;
          elt.prev := self.tail;
          elt.next := NIL;
          elt.prev.next := elt;
          self.tail := elt;
        END;
      END;
    END;
  END AddTail;

PROCEDURE AddHead(self: T; elt: EltT) RAISES { InList } =
  BEGIN
    IF elt = NIL THEN
      RETURN;
    END;

    LOCK self.listmu DO
      LOCK elt.eltmu DO
        (* check if element already in a list *)
        IF elt.list # NIL THEN
          RAISE InList;
        END;

        (* empty list *)
        IF self.head = NIL THEN
          elt.list := self;
          elt.prev := NIL;
          elt.next := NIL;
          self.head := elt;
          self.tail := elt;
        (* non-empty list *)
        ELSE
          elt.list := self;
          elt.prev := NIL;
          elt.next := self.head;
          elt.next.prev := elt;
          self.head := elt;
        END;
      END;
    END;
  END AddHead;

PROCEDURE Remove(self: T; elt: EltT) RAISES { NotInList } =
  BEGIN
    IF elt = NIL THEN
      RAISE NotInList;
    END;

    LOCK self.listmu DO
      LOCK elt.eltmu DO
        (* make sure element in this list *)
        IF elt.list # self THEN
          RAISE NotInList;
        END;

        (* one-item list *)
        IF self.head = elt AND self.tail = elt THEN
          self.head := NIL;
          self.tail := NIL;
        (* element is head *)
        ELSIF self.head = elt THEN
          self.head := elt.next;
          self.head.prev := NIL;
        (* element is tail *)
        ELSIF self.tail = elt THEN
          self.tail := elt.prev;
          self.tail.next := NIL;
        (* element is somewhere in middle *)
        ELSE
          elt.prev.next := elt.next;
          elt.next.prev := elt.prev;
        END;

        (* clear element *)
        elt.list := NIL;
        elt.prev := NIL;
        elt.next := NIL;
      END;
    END;
  END Remove;

PROCEDURE RemoveHead(self: T) : EltT RAISES { Empty } =
  VAR
    elt: EltT;
  BEGIN
    LOCK self.listmu DO
      IF self.head = NIL AND self.tail = NIL THEN
        RAISE Empty;
      END;

      elt := self.head;
      LOCK elt.eltmu DO
        (* one-item list *)
        IF self.tail = elt THEN
          self.head := NIL;
          self.tail := NIL;
        (* multi-item list *)
        ELSE
          self.head := elt.next;
          self.head.prev := NIL;
        END;

        (* clear element *)
        elt.list := NIL;
        elt.prev := NIL;
        elt.next := NIL;
      END;
    END;

    RETURN elt;
  END RemoveHead;      

PROCEDURE RemoveTail(self: T) : EltT RAISES { Empty } =
  VAR
    elt: EltT;
  BEGIN
    LOCK self.listmu DO
      IF self.head = NIL AND self.tail = NIL THEN
        RAISE Empty;
      END;

      elt := self.tail;
      LOCK elt.eltmu DO
        (* one-item list *)
        IF self.head = elt THEN
          self.head := NIL;
          self.tail := NIL;
        (* multi-item list *)
        ELSE
          self.tail := elt.prev;
          self.tail.next := NIL;
        END;

        (* clear element *)
        elt.list := NIL;
        elt.prev := NIL;
        elt.next := NIL;
      END;
    END;

    RETURN elt;
  END RemoveTail;

PROCEDURE MoveToHead(self: T; elt: EltT) RAISES { NotInList } =
  BEGIN
    IF elt = NIL THEN
      RAISE NotInList;
    END;

    LOCK self.listmu DO
      LOCK elt.eltmu DO
        (* elt is already head *)
        IF self.head = elt THEN
          RETURN;
        END;

        (* make sure element in this list *)
        IF elt.list # self THEN
          RAISE NotInList;
        END;

        (* first remove element from list *)
        (* element is tail *)
        IF self.tail = elt THEN
          self.tail := elt.prev;
          self.tail.next := NIL;
        (* element is somewhere in middle *)
        ELSE
          elt.prev.next := elt.next;
          elt.next.prev := elt.prev;
        END;
        
        (* add element to head of list *)
        elt.prev := NIL;
        elt.next := self.head;
        elt.next.prev := elt;
        self.head := elt;
      END;
    END;
  END MoveToHead;

PROCEDURE MoveToTail(self: T; elt: EltT) RAISES { NotInList } =
  BEGIN
    IF elt = NIL THEN
      RAISE NotInList;
    END;

    LOCK self.listmu DO
      LOCK elt.eltmu DO
        (* elt is already tail *)
        IF self.tail = elt THEN
          RETURN;
        END;

        (* make sure element in this list *)
        IF elt.list # self THEN
          RAISE NotInList;
        END;

        (* first remove element from list *)
        (* element is head *)
        IF self.head = elt THEN
          self.head := elt.next;
          self.head.prev := NIL;
        (* element is somewhere in middle *)
        ELSE
          elt.prev.next := elt.next;
          elt.next.prev := elt.prev;
        END;

        (* add element to tail of list *)
        elt.prev := self.tail;
        elt.next := NIL;
        elt.prev.next := elt;
        self.tail := elt;
      END;
    END;
  END MoveToTail;

PROCEDURE IsEmpty(self: T) : BOOLEAN =
  BEGIN
    LOCK self.listmu DO
      RETURN self.head = self.tail;
    END;
  END IsEmpty;

PROCEDURE IsMember(self: T; elt: EltT) : BOOLEAN =
  BEGIN
    LOCK self.listmu DO
      LOCK elt.eltmu DO
        RETURN elt.list = self;
      END;
    END;
  END IsMember;

PROCEDURE InitEltT(self: EltT) : EltT =
  BEGIN
    self.prev := NIL;
    self.next := NIL;
    self.list := NIL;
    self.eltmu := NEW(MUTEX);
    RETURN self;
  END InitEltT;


BEGIN
END DoubleList.


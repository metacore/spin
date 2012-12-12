
(*
 *
 * Copyright 1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *)


(*
 * HISTORY
 * 4-Mar-97  Wilson Hsieh (whsieh) at the University of Washington
 *	created
 *
 *)


UNSAFE MODULE Map;

IMPORT RTIO;

PROCEDURE Add (t: T; ptr: ADDRESS) =
  VAR
    current: S := t.first;
    new: S := NEW (S);
  BEGIN
    (* initialize the new element *)
    new.addr := ptr;

    IF current = NIL THEN
      t.first := new;
      RETURN;
    END;

    WHILE current # NIL DO
      IF current.addr = ptr THEN
        RTIO.PutText ("Repeated entry in Map.Add: ");
        RTIO.PutAddr (ptr);
        RTIO.PutText ("\n");
      END;
      IF current.next = NIL THEN
        current.next := new;
        RETURN;
      ELSE
        current := current.next;
      END;
    END;
  END Add;

PROCEDURE Remove (t: T; ptr: ADDRESS) : BOOLEAN =
  VAR
    current: S := t.first;
    prev: S := NIL;
  BEGIN
    IF current = NIL THEN
      RTIO.PutText ("Ptr not found in Map.Remove because map is empty:");
      RTIO.PutAddr (ptr);
      RTIO.PutText ("\n");
      RETURN FALSE;
    END;

    WHILE current # NIL DO
      IF current.addr = ptr THEN
        EXIT;
      END;
      IF current.next = NIL THEN
        RTIO.PutText ("Ptr not found in Map.Remove: ");
        RTIO.PutAddr (ptr);
        RTIO.PutText ("\n");
        RETURN FALSE;
      ELSE
        prev := current;
        current := current.next;
      END;
    END;
    IF prev = NIL THEN
      <* ASSERT t.first = current *>
      t.first := current.next;
    ELSE
      prev.next := current.next;
    END;
    DISPOSE (current);
    RETURN TRUE;
  END Remove;

PROCEDURE Move (t: T; old, new: ADDRESS) : BOOLEAN =
  VAR
    current: S := t.first;
  BEGIN
    IF current = NIL THEN
      RTIO.PutText ("Ptr not found in Map.Move because of empty map: ");
      RTIO.PutAddr (old);
      RTIO.PutText ("\n");
      RETURN FALSE;
    END;

    WHILE current # NIL DO
      IF current.addr = old THEN
        EXIT;
      END;
      IF current.addr = new THEN
        RTIO.PutText ("Duplicate found in Map.Move: ");
        RTIO.PutAddr (new);
        RTIO.PutText ("\n");
        RETURN FALSE;
      END;
      IF current.next = NIL THEN
        RTIO.PutText ("Ptr not found in Map.Move: ");
        RTIO.PutAddr (old);
        RTIO.PutText ("\n");
        RETURN FALSE;
      ELSE
        current := current.next;
      END;
    END;
    current.addr := new;
    RETURN TRUE;
  END Move;

BEGIN
END Map.

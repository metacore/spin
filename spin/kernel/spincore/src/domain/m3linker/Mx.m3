(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: Mx.m3                                                 *)
(* Last Modified On Tue Aug  2 07:32:49 PDT 1994 By kalsow     *)

(*
 * HISTORY
 * 10-Jun-97  Wilson Hsieh (whsieh) at the University of Washington
 *	add Copy procedure
 *
 *)

MODULE Mx;

IMPORT MxRep, MxMap, MxVSSet;

PROCEDURE NewSet (): LinkSet =
  VAR s := NEW (LinkSet);
  BEGIN
    s.interfaces     := MxMap.New (250);
    s.modules        := MxMap.New (250);
    s.virtuals       := MxMap.New (39);
    s.clients        := MxMap.New (39);
    s.vs_exports     := MxVSSet.New (2500);
    s.vs_impls       := MxVSSet.New (2500);
    s.exported_types := MxMap.New (1500);
    s.linked         := NIL;
    RETURN s;
  END NewSet;

PROCEDURE AddLinked (s, linked: LinkSet) =
  VAR
    ptr, prev, elem  : MxRep.LinkSetList;
  BEGIN
    ptr := s.linked;
    prev := NIL;
    WHILE ptr # NIL DO
      IF ptr.set = linked THEN
        RETURN;
      END;
      prev := ptr;
      ptr := ptr.next;
    END;
    elem := NEW(MxRep.LinkSetList, set := linked);
    IF prev = NIL THEN
      s.linked := elem;
    ELSE
      prev.next := elem;
    END;

    MxMap.AddLinked(s.interfaces, linked.interfaces);
    MxMap.AddLinked(s.modules, linked.modules);
    MxMap.AddLinked(s.virtuals, linked.virtuals);
    MxMap.AddLinked(s.clients, linked.clients);
    MxVSSet.AddLinked(s.vs_exports, linked.vs_exports);
    MxVSSet.AddLinked(s.vs_impls, linked.vs_impls);
    MxMap.AddLinked(s.exported_types, linked.exported_types);
  END AddLinked;

PROCEDURE FixSize (s: LinkSet) =
  BEGIN
    MxMap.FixSize(s.interfaces);
    MxMap.FixSize(s.modules);
    MxMap.FixSize(s.virtuals);
    MxMap.FixSize(s.clients);
    MxVSSet.FixSize(s.vs_exports);
    MxVSSet.FixSize(s.vs_impls);
    MxMap.FixSize(s.exported_types);
    s.linked := NIL;
  END FixSize; 

(* SPIN *)
PROCEDURE Copy (ul: UnitList) : UnitList =
  VAR newul: UnitList;
  BEGIN
    IF ul = NIL THEN RETURN NIL; END;

    newul := NEW (UnitList);
    newul.unit := ul.unit;
    newul.next := Copy (ul.next);

    RETURN newul;
  END Copy;

BEGIN
END Mx.

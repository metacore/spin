(*
 * Copyright 1997, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 10-Jun-97  Wilson Hsieh (whsieh) at the University of Washington
 *	check import/export
 *
 * 04-Mar-97  Przemek Pardyak (pardy) at the University of Washington
 *      Removed the destruction code.
 *
 * 27-Feb-97  Emin Gun Sirer (egs) at the University of Washington
 *      This stuff needs better documentation, especially wrt the locking
 *      protocol(s).
 *
 * 27-Feb-97  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Checks on domains.
 *)

UNSAFE MODULE DomainChecks;
IMPORT Domain, DomainRep, DomainPrivate;
IMPORT Mx, MxCheck, MxMerge, MxMap, MxRep;
IMPORT IO, Wr, RT0;
IMPORT RTTypeSRC, Auth;
IMPORT Ctypes, M3toC, Text, M3ID;

(* For M3 dynamic linking *)
TYPE
  RuntimeModulePtr = UNTRACED REF ARRAY OF RT0.ModulePtr;
  RuntimeLinkInfoPtr = UNTRACED REF ARRAY OF UNTRACED REF CHAR;

(************************** import/export checks ***************************)

PROCEDURE CheckImportExport (units: Mx.UnitList; domain: Domain.T)
  : BOOLEAN =
  VAR
    u: Mx.UnitList;
    ok := TRUE;
  BEGIN
    u := units;
    IF Domain.debugIE THEN IO.Put ("In DomainChecks.CheckImportExport\n"); END;
    WHILE u # NIL DO
      IF u.unit # NIL THEN
        IF NOT u.unit.interface AND NOT CheckUnit (u.unit, domain) THEN
          ok := FALSE;
        END;
      END;
      u := u.next;
    END;
    RETURN ok;
  END CheckImportExport;

TYPE UnitType = { Import, Export };

(* check the given unit -- imports and exports *)
PROCEDURE CheckUnit (unit: Mx.Unit; domain: Domain.T) : BOOLEAN =
  VAR
    ok := TRUE;
    current : Domain.T := domain;
    list : DomainRep.DList; (* domains that have been linked against *)
    base : Mx.LinkSet := domain.linkBase;
    name : TEXT := M3ID.ToText (unit.name);
      
  PROCEDURE Checker (READONLY n: Mx.InfoList; ut: UnitType) =
    VAR
      nm      : INTEGER;
      target  : Mx.Unit;
      targetd : Domain.T;
      targetn : TEXT;
    BEGIN
      FOR i := n.start TO n.start + n.cnt - 1 DO
        nm := unit.mInfo.info [i];
        target := MxMap.Get (base.interfaces, nm);

        IF target = NIL THEN
          ok := FALSE;
          IF ut = UnitType.Import THEN
            IO.PutError ("Linker could not find m3 link info for import of " &
                         name & "\n");
          ELSE
            IO.PutError ("Linker could not find m3 link info for export of " &
                         name & "\n");
          END;
        ELSE
          targetn := M3ID.ToText (target.name);
          targetd := FindDomain (target, list);

          IF ut = UnitType.Import THEN
            IF targetd = NIL OR NOT targetd.import THEN
              IO.PutError (name & " illegally imports " & targetn & "\n");
              ok := FALSE;
            END;
          ELSE (* ut = UnitType.Export *)
            IF targetd = NIL OR NOT targetd.interfacesCanBeImplemented THEN
              IO.PutError (name & " illegally exports " & targetn & "\n");
              ok := FALSE;
            END;
          END;
        END;
      END;
    END Checker;
    
  BEGIN
    IF Domain.debugIE THEN IO.Put ("Checking unit " & name & "\n"); END;

    (* find the list of domains linked against *)
    WHILE current.parent # NIL DO current := current.parent; END;
    list := current.rev_linked;

    Checker (unit.mInfo.imported_units, UnitType.Import);
    Checker (unit.mInfo.exported_units, UnitType.Export);

    RETURN ok;
  END CheckUnit;


(* find the first domain on list that contains unit *)
PROCEDURE FindDomain (unit: Mx.Unit; list: DomainRep.DList) : Domain.T =
  VAR
    current: Domain.T;
  BEGIN
    current := unit.domain;

    (* search up the domain hierarchy *)
    WHILE current # NIL DO
      IF OnList (current, list) THEN
        RETURN current;
      END;
      current := current.parent;
    END;

    IF Domain.debugIE THEN
      IO.Put ("****************\n");
      IO.Put ("Could not find unit " & M3ID.ToText (unit.name) & "\n");
      
      current := unit.domain;

      (* search up the domain hierarchy *)
      WHILE current # NIL DO
        IO.Put (current.name & "\n");
        current := current.parent;
      END;
      IO.Put ("****************\n");
      DomainRep.PrintDList (list);
      IO.Put ("****************\n");
    END;

    RETURN NIL;
  END FindDomain;

(* see if domain is on list *)
PROCEDURE OnList (domain: Domain.T; list: DomainRep.DList) : BOOLEAN =
  VAR
    current: DomainRep.DList := list;
  BEGIN
    WHILE current # NIL DO
      IF current.domain = domain THEN RETURN TRUE; END;
      current := current.next;
    END;
    RETURN FALSE;
  END OnList;

(************************* subtyping checks ******************************)

PROCEDURE CheckSubtypes(domain: Domain.T): BOOLEAN =
  VAR
    modules : RuntimeModulePtr;
    linkInfo: RuntimeLinkInfoPtr;
    childDefn, parentDefn : RT0.TypeDefn;
    auth : Auth.T;
  BEGIN
    DomainPrivate.CollectM3Modules(domain, modules, linkInfo);

    FOR i := FIRST(modules^) TO LAST(modules^) DO
      childDefn := LOOPHOLE(modules[i].type_cells, RT0.TypeDefn);
      WHILE (childDefn # NIL) DO 
        parentDefn := RTTypeSRC.FindType(childDefn.parentID);
        WHILE parentDefn # NIL DO
          TRY 
            IF parentDefn.brand # NIL THEN
              (* Test that the domain containing the supertype allows
                 it to be subtyped. *)
              WITH owner = DomainPrivate.TypeDefnToDomain(parentDefn) DO
                IF owner.authorizer # NIL AND
                  NOT owner.authorizer.authorize(NIL, domain) THEN
                  IO.PutError("Domain " & owner.name & 
                    " will not allow subtyping of " & 
                    M3toC.StoT(LOOPHOLE(childDefn.name, Ctypes.char_star)) &
                    " from " &
                    M3toC.StoT(LOOPHOLE(parentDefn.name, Ctypes.char_star)) & "\n");
                  RETURN FALSE;
                END;
              END;

              (* Test whether a type-specific authorizer has been installed
                 for this supertype. *)
              IF TypeAuthTbl.get(parentDefn.typecode, auth) THEN
                IF NOT auth.authorize(NIL, domain) THEN
                  IO.PutError("Domain " & domain.name & 
                    " does not have permission to subtype from " &
                    M3toC.StoT(LOOPHOLE(parentDefn.name, Ctypes.char_star)) & 
                    "\n");
                  RETURN FALSE;
                END;
              END;
            END;
          EXCEPT
          | Auth.Retry =>
            IO.PutError ("Auth.Retry in DomainChecks.CheckSubtype\n");
            RETURN FALSE; (* What does retry mean ? *)
          | DomainPrivate.NonDomainTypeDefn => (* If a type definition is not inside any
                                  * known domain, then let it be subtyped. 
                                  *)
          END;

          (* Move on to the next supertype. *)
          parentDefn := RTTypeSRC.FindType(parentDefn.parentID);
        END;
        childDefn := childDefn.next; 
      END;
    END;

    RETURN TRUE;
  END CheckSubtypes;

PROCEDURE CheckM3LinkInfo(linkBase: Mx.LinkSet;
                          units: Mx.UnitList; 
                          nUnits: INTEGER;
                          wr: Wr.T;
                          dump: BOOLEAN;
                          static: BOOLEAN): BOOLEAN =
  VAR
    bad: Mx.UnitList;
    u: Mx.UnitList;
    change: BOOLEAN;
    interfaces: BOOLEAN;
    nMerged: INTEGER := 0;
    ok: BOOLEAN;
  BEGIN
    IF DomainPrivate.verbose THEN
      IO.Put(">>> merging Modula-3 link information\n");
    END;

    u := units;
    IF static THEN
      WHILE u # NIL DO
        IF u.unit # NIL AND 
          Text.Equal(M3ID.ToText(u.unit.name), "M3_BUILTIN") AND
          u.unit.interface 
         THEN 
          bad := MxMerge.MergeUnit (u.unit, linkBase, wr);
          IF bad # NIL THEN
            IO.PutError("Domain: could not merge the builtin interface\n");
            RETURN FALSE;
          END;
          IF dump THEN
            IO.Put("<" & M3ID.ToText(u.unit.name));
            IF u.unit.interface THEN 
              IO.Put(".i3>"); 
            ELSE 
              IO.Put(".m3>"); 
            END;
          END;
          u.unit := NIL;
          INC(nMerged);
          EXIT;
        END;
        u := u.next;
      END;
    END;

    (* interfaces should come before modules *)
    interfaces := TRUE;
    FOR i := 0 TO 1 DO
      change := TRUE;
      WHILE change DO
        u := units;
        change := FALSE;
        WHILE u # NIL DO
          IF u.unit # NIL AND u.unit.interface = interfaces THEN
            IF dump THEN
              IO.Put("<? " & M3ID.ToText(u.unit.name));
              IF u.unit.interface THEN 
                IO.Put(".i3>"); 
              ELSE 
                IO.Put(".m3>"); 
              END;
            END;
            bad := MxMerge.MergeUnit (u.unit, linkBase, NIL);
            IF bad = NIL THEN
              IF dump THEN
                IO.Put("<OK " & M3ID.ToText(u.unit.name));
                IF u.unit.interface THEN 
                  IO.Put(".i3>"); 
                ELSE 
                  IO.Put(".m3>"); 
                END;
              END;
              change := TRUE;
              u.unit := NIL;
              INC(nMerged);
            END;
          END;
          u := u.next;
        END;
      END;
      interfaces := FALSE;
    END;

    IF dump THEN IO.Put("\n"); END;

    IF nMerged # nUnits THEN
      u := units;
      WHILE u # NIL DO
        IF u.unit # NIL THEN
          IO.PutError("Domain: could not merge unit " & 
            M3ID.ToText(u.unit.name) & "\n");
          bad := MxMerge.MergeUnit (u.unit, linkBase, wr);
          IF bad = NIL THEN
            IO.PutError("Domain: now it merged it ???? " & 
              M3ID.ToText(u.unit.name) & "\n");
          END;
        END;
        u := u.next;
      END;
      RETURN FALSE;
    END;

    IF DomainPrivate.verbose THEN
      IO.Put(">>> verifying correctness of Modula-3 link information\n");
    END;
    ok := MxCheck.IsProgram (linkBase, wr);
    Mx.FixSize(linkBase);
    RETURN ok;
  END CheckM3LinkInfo; 

BEGIN
END DomainChecks.

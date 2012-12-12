(* 
 * Copyright 1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * Access Mode
 *   is an immutable type consisting of a set of simple permissions
 *   and a list of Permission objects. The set of simple permissions
 *   is supported as an optimization for the core security services.
 *
 * HISTORY
 *
 * 18-Nov-97  Robert Grimm (rgrimm) at the University of Washington
 *      Generalized concept of simple permissions to big bit vector
 *
 * 27-Oct-97  Robert Grimm (rgrimm) at the University of Washington
 *      Updates for new permission interface.
 *      Changed Equal to Contain.
 *
 * 07-Aug-97  Robert Grimm (rgrimm) at the University of Washington
 *      More clean up and bettter consistency.
 *
 * 30-Jul-97  Robert Grimm (rgrimm) at the University of Washington
 *      Cleaned up and simplified code.
 *
 * 02-Mar-97  Robert Grimm (rgrimm) at the University of Washington
 *      Made opaque and added permissions.
 *
 * 28-Jan-97  Robert Grimm (rgrimm) at the University of Washington
 *      Created.
 *
 *)

MODULE AccessMode;

IMPORT Permission, AccessModeRep;

PROCEDURE Create( simple      : SimpleT;
                  permissions : Permission.ListT := NIL ) : T =
  (*
     Create a new access mode.
     NIL entries and duplicate permissions,
     i.e. permissions which are contained by others,
     are removed from the list of permissions.
   *)
  VAR
    count : INTEGER;
    list  : Permission.ListT := NIL;
  PROCEDURE HasDuplicate( index : INTEGER ) : BOOLEAN =
    (*
       Does permission at "index" have a duplicate permission,
       i.e. is it contained by another permission?
       To perform this test correctly (permissions can be
       properly contained but also can be equal),
       we always return TRUE if "p1" is properly contained by "p2",
       but, if "p1" equals "p2", "p2" must be after "p1" in the list.
     *)
    BEGIN
      WITH p1 = permissions[index] DO
        FOR i := FIRST(permissions^) TO LAST(permissions^) DO
          IF i # index THEN
            WITH p2 = permissions[i] DO
              IF     p2 # NIL
                 AND TYPECODE(p2) = TYPECODE(p1)
                 AND p2.contains(p1) THEN
                IF NOT p1.contains(p2) THEN
                  RETURN TRUE;
                ELSIF i > index THEN
                  RETURN TRUE;
                END;
              END;
            END;
          END;
        END;
      END;
      RETURN FALSE;
    END HasDuplicate;
  BEGIN
    (*
       If there is a list of permissions,
       count the unique non-nil entries
       and copy them into a new list.
     *)
    IF permissions # NIL THEN
      count := 0;
      FOR i := FIRST(permissions^) TO LAST(permissions^) DO
        IF permissions[i] # NIL AND NOT HasDuplicate( i ) THEN
          INC(count);
        END;
      END;
      IF count # 0 THEN
        list := NEW( Permission.ListT, count );
        count := 0;
        FOR i := FIRST(permissions^) TO LAST(permissions^) DO
          IF permissions[i] # NIL AND NOT HasDuplicate( i ) THEN
            list[count] := permissions[i];
            INC(count);
          END;
        END;
      END;
    END;

    RETURN NEW( T, simple := simple, permissions := list );
  END Create;

<* INLINE *>
PROCEDURE GetSimplePermissions( mode : T ) : SimpleT =
  (* Get simple permissions. *)
  BEGIN
    RETURN mode.simple;
  END GetSimplePermissions;

<*INLINE*>
PROCEDURE GetPermissionNumber( mode : T ) : INTEGER =
  (* Get number of distinct permissions in access mode. *)
  BEGIN
    WITH p = mode.permissions DO
      RETURN LAST(p^) - FIRST(p^) + 1;
    END;
  END GetPermissionNumber;

<* INLINE *>
PROCEDURE GetPermission( mode : T; index : INTEGER )
  : Permission.T =
  (*
     Get permission from access mode,
     where index ranges over 0 to "GetPermissionNumber" - 1.
   *)
  BEGIN
    WITH p = mode.permissions DO
      RETURN p[ FIRST(p^) + index ];
    END;
  END GetPermission;

PROCEDURE HasPermission( mode : T;
                         permission : Permission.T ) : BOOLEAN =
  (* Does access mode include this permission? *)
  BEGIN
    WITH p = mode.permissions DO
      IF p = NIL THEN RETURN FALSE; END;
      FOR i := FIRST(p^) TO LAST(p^) DO
        IF     TYPECODE(p[i]) = TYPECODE(permission)
           AND p[i].contains(permission) THEN
          RETURN TRUE;
        END;
      END;
    END;
    RETURN FALSE;
  END HasPermission;

PROCEDURE Combine( mode1, mode2 : T ) : T =
  (* Combine two access modes into a new one. *)
  VAR
    mode  : T;
    count : INTEGER;
    list  : Permission.ListT;
  PROCEDURE HasDuplicate1( index : INTEGER ) : BOOLEAN =
    (*
       Does permission at "index" in "mode1"
       have a permission in "mode2" which contains it?
     *)
    BEGIN
      WITH p  = mode1.permissions[index],
           p2 = mode2.permissions^        DO
        FOR i := FIRST(p2) TO LAST(p2) DO
          IF     TYPECODE(p2[i]) = TYPECODE(p)
             AND p2[i].contains(p) THEN
            RETURN TRUE;
          END;
        END;
      END;
      RETURN FALSE;
    END HasDuplicate1;
  PROCEDURE HasDuplicate2( index : INTEGER ) : BOOLEAN =
    (*
       Does permission at "index" in "mode2"
       have a permission in "mode1" which properly contains it?
     *)
    BEGIN
      WITH p  = mode2.permissions[index],
           p1 = mode1.permissions^        DO
        FOR i := FIRST(p1) TO LAST(p1) DO
          IF     TYPECODE(p1[i]) = TYPECODE(p)
             AND p1[i].contains(p)
             AND NOT p.contains(p1[i]) THEN
            RETURN TRUE;
          END;
        END;
      END;
      RETURN FALSE;
    END HasDuplicate2;
  BEGIN
    (* Create new access mode *)
    mode := NEW(T, simple := mode1.simple + mode2.simple,
                   permissions := NIL );

    (* Create the new permission list. *)
    WITH p1 = mode1.permissions, p2 = mode2.permissions DO
      IF p1 = NIL AND p2 = NIL THEN
        (* Nothing to do. *)
      ELSIF p1 = NIL THEN
        (*
           Since access modes are immutable and, on creation,
           the permission list is freshly allocated on the heap,
           we can reuse it here without copying.
        *)
        mode.permissions := mode2.permissions;
      ELSIF p2 = NIL THEN
        (* Dito. *)
        mode.permissions := mode1.permissions;
      ELSE
        (*
         * Count the number of distinct permission objects
         * in both access modes, allocate a new permission
         * list and copy over the distinct permission objects.
         *)
        count := 0;
        FOR i := FIRST(p1^) TO LAST(p1^) DO
          IF NOT HasDuplicate1(i) THEN INC(count); END;
        END;
        FOR i := FIRST(p2^) TO LAST(p2^) DO
          IF NOT HasDuplicate2(i) THEN INC(count); END;
        END;
        list := NEW( Permission.ListT, count );
        count := 0;
        FOR i := FIRST(p1^) TO LAST(p1^) DO
          IF NOT HasDuplicate1(i) THEN
            list[count] := p1[i];
            INC(count);
          END;
        END;
        FOR i := FIRST(p2^) TO LAST(p2^) DO
          IF NOT HasDuplicate2(i) THEN
            list[count] := p2[i];
            INC(count);
          END;
        END;
        mode.permissions := list;
      END;
    END;

    RETURN mode;
  END Combine;

PROCEDURE Contain( mode1, mode2 : T ) : BOOLEAN =
  (* Does "mode1" entail all permissions in "mode2" ? *)
  VAR
    found : BOOLEAN;
  BEGIN
    IF NOT mode1.simple >= mode2.simple THEN RETURN FALSE; END;
    WITH p1 = mode1.permissions, p2 = mode2.permissions DO
      IF p2 # NIL THEN
        IF p1 = NIL THEN RETURN FALSE; END;
        FOR i := FIRST(p2^) TO LAST(p2^) DO
          found := FALSE;
          FOR j := FIRST(p1^) TO LAST(p1^) DO
            IF     TYPECODE(p1[j]) = TYPECODE(p2[i])
               AND p1[j].contains(p2[i]) THEN
              found := TRUE;
              EXIT;
            END;
          END;
          IF NOT found THEN RETURN FALSE; END;
        END;
      END;
    END;

    RETURN TRUE;
  END Contain;

BEGIN
END AccessMode.

(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Sat Nov 19 09:27:18 PST 1994 by kalsow     *)
(*      modified on Fri May 28 14:54:41 PDT 1993 by muller     *)

(*
 * HISTORY
 * 17-Nov-97  Tian Fung Lim (tian) at the University of Washington
 *	Added hack to FixObjectsizes - we now use the elementSize field
 *	to hold the real, unaligned dataSize of the type.  This works
 *	because arrays are not subtyped.  The new elementSize field is
 *	used to compute dataOffsets (which should only be word aligned).
 *	Previously, the code assumed that the header size was one word.
 *
 * 14-Oct-97  Tian Fung Lim (tian) at the University of Washington
 *	Added call to re-initialize type security structures when linking
 *	new modules.
 *
 * 11-Feb-97  Przemek Pardyak (pardy) at the University of Washington
 *	Fixed detection of identity of object types.
 *
 * 14-Nov-96  Przemek Pardyak (pardy) at the University of Washington
 *	Exported IDMap to RTTypeSRC.
 *
 * 30-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	bug fix from SRC
 *
 * 14-Jun-96  Przemek Pardyak (pardy) at the University of Washington
 *	Got rid of typeidx.
 *
 * 27-Mar-96  Przemek Pardyak (pardy) at the University of Washington
 *	Reinitialization of types does not crash now if inconsistency
 *	is detected. It is up to the caller of Init() whether the system
 *	can continue.  In particular, if the initialial setup fails
 *	RTLinker will crash the system.
 *
 * 17-Feb-96  Przemek Pardyak (pardy) at the University of Washington
 *	Got rid of some dead code.  More output if Get() fails. 
 *
 * 20-Jan-96  Przemek Pardyak (pardy) at the University of Washington
 *	Got rid of an infinite loop in ShowTypes.
 *
 * 19-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	Removed reliance on Fmt (which has been moved to libm3_sa)
 *
 * 30-Nov-95  Charlie Garrett (garrett) at the University of Washington
 *	Add IsValid() check to tell if a Typecode value is reasonable.
 *	RTHeapStats uses this.
 *
 * 13-Nov-95  Przemek Pardyak (pardy) at the University of Washington
 *	Changed operations on subtypes to enable dynamic linking of types:
 *	typecode is set to be an offset from BaseTypecellPointer,
 *	subTypeCode and lastSubTypeCode are indeces into linearized tree
 *	representing the subtype relation. See RT0.i3 for changes in type 
 *	representation. 
 *
 * 14-Jul-95  Przemek Pardyak (pardy) at the University of Washington
 *	Added support for initialization of dynamicly linked modules
 *
 *)

UNSAFE MODULE RTType EXPORTS RTType, RTTypeSRC;

IMPORT RT0, RT0u, RTMisc, RTHeapRep, M3toC;
IMPORT Ctypes, Cstdlib, Word;
IMPORT RTLinker, RTOS, RTIO;
IMPORT RTTypeSecurity;
FROM RTIO IMPORT PutInt, PutString, PutText, PutAddr, PutHex, Flush;

CONST
  MAX_NUMBER_TYPES = 5000;
  (* This is a runtime constant which can be freely changed within the 
     limit of Typecode *)

TYPE
 TypePtr = UNTRACED REF RT0.TypeDefn;

VAR
  old_nTypes: INTEGER := 0;

(*------------------------------------------------ user callable routines ---*)

PROCEDURE MaxTypecode (): Typecode =
  BEGIN
    RETURN RT0u.nTypes - 1;
  END MaxTypecode;

PROCEDURE IsValid(tc: Typecode): BOOLEAN =
  VAR p: TypePtr;
  BEGIN
    IF (tc < 0) OR (tc > RT0u.nTypes) THEN
      RETURN FALSE;
    END;
    p := RT0u.types + tc * ADRSIZE (RT0.TypeDefn);
    RETURN p^ # NIL;
  END IsValid;

PROCEDURE IsSubtype (a, b: Typecode): BOOLEAN =
  VAR t_a := Get (a);
  VAR t_b := Get (b);
  VAR stc_a := t_a.subTypeCode;
  VAR stc_b := t_b.subTypeCode;
  VAR lstc_b := t_b.lastSubTypeCode;
  BEGIN
    RETURN (stc_b <= stc_a AND stc_a <= lstc_b);
  END IsSubtype;

PROCEDURE Supertype (tc: Typecode): Typecode =
  VAR t := Get (tc);
  BEGIN
    IF (t.parent = NIL)
      THEN RETURN NoSuchType;
      ELSE RETURN t.parent.typecode;
    END;
  END Supertype;

PROCEDURE IsTraced (tc: Typecode): BOOLEAN =
  VAR t := Get (tc);
  BEGIN
    RETURN t.traced # 0;
  END IsTraced;

PROCEDURE Get (tc: Typecode): RT0.TypeDefn =
  VAR p: TypePtr;
  BEGIN
    p := RT0u.types + tc * ADRSIZE (RT0.TypeDefn);
    IF (tc < 0) OR (tc >= RT0u.nTypes) OR p^ = NIL THEN BadType (tc); END;
    RETURN p^;
  END Get;

PROCEDURE GetNDimensions (tc: Typecode): CARDINAL =
  VAR t := Get (tc);
  BEGIN
    RETURN t.nDimensions;
  END GetNDimensions;

PROCEDURE TypeName (ref: REFANY): TEXT =
  VAR t := Get (TYPECODE (ref));
  BEGIN
    RETURN TypeDefnToName (t);
  END TypeName;

PROCEDURE TypecodeName (tc: Typecode): TEXT =
  VAR t := Get (tc);
  BEGIN
    RETURN TypeDefnToName (t);
  END TypecodeName;

PROCEDURE TypeDefnToName (t: RT0.TypeDefn): TEXT =
  BEGIN
    IF (t.name = NIL) THEN RETURN "<anon type>"; END;
    RETURN M3toC.CopyStoT (LOOPHOLE (t.name, Ctypes.char_star));
  END TypeDefnToName;

(*--------------------------------------------------- UID -> typecell map ---*)

PROCEDURE FindType (id: INTEGER): RT0.TypeDefn =
  BEGIN
    RETURN ReallyFindType(type_ids, n_type_ids, id);
  END FindType;

PROCEDURE InitFindType (id: INTEGER): RT0.TypeDefn =
  BEGIN
    RETURN ReallyFindType(RTLinker.type_info.type_ids, 
                          RTLinker.type_info.n_type_ids, 
                          id);
  END InitFindType; 

PROCEDURE ReallyFindType (base: ADDRESS; hi: CARDINAL;
                          id: INTEGER): RT0.TypeDefn =
  VAR
    lo   : CARDINAL := 0;
    mid  : CARDINAL; 
    p    : UNTRACED REF IDMap;
  BEGIN
    WHILE (lo < hi) DO
      mid := (lo + hi) DIV 2;
      p := base + mid * ADRSIZE (p^);
      IF (id < p.uid)
        THEN hi := mid;
        ELSE lo := mid + 1;
      END;
    END;
    IF (lo > 0) THEN DEC (lo) END;
    p := base + lo * ADRSIZE (p^);
    IF (p.uid # id) THEN RETURN NIL END;
    RETURN p.defn;
  END ReallyFindType; 

(*-------------------------------------------------------- initialization ---*)

VAR
  init_done := FALSE;
  null  : RT0.TypeDefn;
  text  : RT0.TypeDefn;
  root  : RT0.TypeDefn;
  uroot : RT0.TypeDefn;

PROCEDURE Init (): BOOLEAN =
  BEGIN
    init_done := TRUE;
    IF NOT RegisterTypes () THEN RETURN FALSE; END;
    IF NOT CheckOpaques () THEN RETURN FALSE; END;
    IF NOT CheckBrands () THEN RETURN FALSE; END;
    IF NOT FindChildren () THEN RETURN FALSE; END;
    IF NOT CheckParents () THEN RETURN FALSE; END;
    AssignTypecodes ();
    IF NOT FixLinks () THEN RETURN FALSE; END;
    IF NOT FixSizes () THEN RETURN FALSE; END;
    CallSetupProcs ();
    IF NOT CheckRevelations () THEN RETURN FALSE; END;
    RTHeapRep.CheckTypes ();
    RETURN TRUE;
  END Init;

PROCEDURE RegisterTypes (): BOOLEAN =
  (* "register" each typecell with a distinct temporary typecode *)
  VAR
    mi  : RT0.ModulePtr;
    t   : RT0.TypeDefn;
    ncnt, cnt, key : INTEGER;
    tp, x, y, z : TypePtr;
  BEGIN
    old_nTypes := RT0u.nTypes;

    (* count the typecells *)
    cnt := 0;
    FOR i := 0 TO RTLinker.info.n_modules - 1 DO
      mi := GetModule (i);
      t := mi.type_cells;
      WHILE (t # NIL) DO 
        INC (cnt); 
        t := t.next; 
      END;
    END;

    (* count the new typecells *)
    ncnt := 0;
    FOR i := RTLinker.n_initialized TO RTLinker.info.n_modules - 1 DO
      mi := GetModule (i);
      t := mi.type_cells;
      WHILE (t # NIL) DO 
        INC (ncnt); 
        t.typecode := LAST (RT0.Typecode); 
        IF t.name = NIL THEN
          PutText("ERROR >> RTType: anonymous type found: ");
          PutHex(t.selfID); PutText(" in ");
          PutString(mi.file); PutText("\n");
          RETURN FALSE;
        END;
        t := t.next; 
      END;
    END;

    (* allocate the space *)
    RTLinker.type_info.nTypes      := cnt;
    RTLinker.type_info.types       := Cstdlib.malloc(cnt * BYTESIZE (t));

    IF RT0u.alloc_cnts = NIL THEN
      IF cnt > MAX_NUMBER_TYPES THEN
        RTIO.PutText("ERROR >> too many types (check RTType.m3)\n");
        RTOS.Crash();
      END;
      RT0u.alloc_cnts  := Cstdlib.malloc(MAX_NUMBER_TYPES * BYTESIZE(INTEGER));
      RT0u.alloc_bytes := Cstdlib.malloc(MAX_NUMBER_TYPES * BYTESIZE(INTEGER));
      RTMisc.Zero (RT0u.alloc_cnts,  MAX_NUMBER_TYPES * BYTESIZE(INTEGER));
      RTMisc.Zero (RT0u.alloc_bytes, MAX_NUMBER_TYPES * BYTESIZE(INTEGER));
    END;

    (*
    RTLinker.type_info.alloc_cnts  := Cstdlib.malloc(cnt * BYTESIZE (INTEGER));
    RTLinker.type_info.alloc_bytes := Cstdlib.malloc(cnt * BYTESIZE (INTEGER));
    *)

    WITH size = cnt * BYTESIZE (INTEGER) DO
      RTLinker.type_info.subTypeCode     := Cstdlib.malloc(size);
      RTLinker.type_info.lastSubTypeCode := Cstdlib.malloc(size);
    END;
    VAR st := RTLinker.type_info.subTypeCode;
    BEGIN
      FOR i := 0 TO cnt-1 DO
        st^ := LAST (RT0.Typecode);
        INC(st, ADRSIZE(st^));
      END;
    END;

    (* initialize the allocation counts *)
    (* FIXME: should copy the old counters *)
    (*
    RTMisc.Zero (RTLinker.type_info.alloc_cnts,  cnt * BYTESIZE (INTEGER));
    RTMisc.Zero (RTLinker.type_info.alloc_bytes, cnt * BYTESIZE (INTEGER));
    *)

    (* collect pointers to all the typecells *)
    tp := RTLinker.type_info.types;
    FOR i := 0 TO RTLinker.info.n_modules - 1 DO
      mi := GetModule (i);
      t := mi.type_cells;
      WHILE (t # NIL) DO
        tp^ := t;  INC (tp, ADRSIZE (t));
        t := t.next;
      END;
    END;

    (* sort the cells by uid *)
    x := RTLinker.type_info.types;
    FOR i := 1 TO cnt-1 DO
      tp := x + i * ADRSIZE (t);
      t := tp^;
      key := t.selfID;
      y := x + (i - 1) * ADRSIZE (t);
      WHILE (y >= x) AND (y^.selfID > key) DO
        z := y + ADRSIZE (t);
        z^ := y^;
        DEC (y, ADRSIZE (t));
      END;
      z := y + ADRSIZE (t);
      z^ := t;
    END;

    (* remove duplicates, but keep names *)
    cnt := 1;
    x := RTLinker.type_info.types;
    y := x;
    FOR i := 1 TO RTLinker.type_info.nTypes-1 DO
      INC (y, ADRSIZE (t));
      IF x^.selfID = y^.selfID AND x^.linkProc = y^.linkProc THEN
        (* A duplicate, if we don't have one yet, save the name *)
        (* Two types are initialized by different procedures are different *)
        (* because they are dependent on the implementation as opposed to *)
        (* representation, for example, they are objects types with  methods *)
        IF (x^.name = NIL) THEN x^.name := y^.name; END;
      ELSE (* a new typecell *)
        INC (cnt);
        INC (x, ADRSIZE (t));
        x^ := y^;
      END;
    END;
(* ??? *)
    RTLinker.type_info.nTypes := cnt;
(* ??? *)
    RETURN TRUE;
  END RegisterTypes;

PROCEDURE CheckOpaques (): BOOLEAN =
  (* build the UID->Defn maps including the opaque types *)
  VAR
    cnt : INTEGER;
    mi  : RT0.ModulePtr;
    t   : RT0.TypeDefn;
    r   : RT0.RevPtr;
    s, v: UNTRACED REF IDMap;
    tp  : TypePtr;
  BEGIN
    (* count the opaques *)
    cnt := RTLinker.type_info.nTypes;
    FOR i := 0 TO RTLinker.info.n_modules - 1 DO
      mi := GetModule (i);
      r := mi.full_rev;
      IF (r # NIL) THEN
        WHILE (r.lhs_id # 0) DO INC (cnt);  INC (r, ADRSIZE (r^)); END;
      END;
    END;

    (* allocate the space *)
    RTLinker.type_info.n_type_ids := cnt;
    RTLinker.type_info.type_ids   := Cstdlib.malloc (cnt * BYTESIZE (IDMap));

    (* initialize the map with the concrete typecells *)
    tp := RTLinker.type_info.types;
    s  := RTLinker.type_info.type_ids;
    FOR i := 0 TO RTLinker.type_info.nTypes-1 DO
      t := tp^;
      s.uid  := t.selfID;
      s.defn := t;
      INC (tp, ADRSIZE (tp^));
      INC (s, ADRSIZE (s^));
    END;
    RTLinker.type_info.n_type_ids := RTLinker.type_info.nTypes;

    (* finally, add each of the opaque types *)
    FOR i := 0 TO RTLinker.info.n_modules - 1 DO
      mi := GetModule (i);
      r := mi.full_rev;
      IF (r # NIL) THEN
        WHILE (r.lhs_id # 0) DO
          t := InitFindType (r.lhs_id);
          (* IF (t # NIL) THEN DuplicateLHS (mi, r, t) END;*)
          t := InitFindType (r.rhs_id);
          IF (t = NIL) THEN 
            UndefinedRHS (mi, r);
            RETURN FALSE;
          END;

          (* insert the new entry *)
          v := RTLinker.type_info.type_ids +
                   RTLinker.type_info.n_type_ids * ADRSIZE (v^);
          s := v - ADRSIZE (v^);
          WHILE (s >= RTLinker.type_info.type_ids) AND (s.uid > r.lhs_id) DO
            v^ := s^;
            DEC (v, ADRSIZE (v^));
            DEC (s, ADRSIZE (s^));
          END;
          v.uid  := r.lhs_id;
          v.defn := t;
          INC (RTLinker.type_info.n_type_ids);

          INC (r, ADRSIZE (r^));
        END;
      END;
    END;
    RETURN TRUE;
  END CheckOpaques;

PROCEDURE CheckBrands (): BOOLEAN =
  (* ensure that all brands are distinct *)
  VAR
    t, a, b : RT0.TypeDefn;
    tp      : TypePtr;
    hash    : INTEGER;
    buckets := ARRAY [0..292] OF RT0.TypeDefn {NIL, ..};
  BEGIN
    (* Hash each type with a non-nil brand into the table
       using the type's sibling pointer to resolve collisions. *)
    tp := RTLinker.type_info.types;
    FOR i := 0 TO RTLinker.type_info.nTypes-1 DO
      t := tp^;
      IF (t.brand # NIL) THEN
        hash := HashString (t.brand) MOD NUMBER (buckets);
        t.sibling := buckets[hash];
        buckets[hash] := t;
      END;
      INC (tp, ADRSIZE (tp^));
    END;

    (* Run the naive O(n^2) check on each hash bucket. *)
    FOR i := 0 TO LAST (buckets) DO
      a := buckets[i];
      WHILE (a # NIL) DO
        b := a.sibling;
        WHILE (b # NIL) DO
          (* FIXME: check brands
          IF Cstring.strcmp (LOOPHOLE(a.brand, Ctypes.char_star),
                             LOOPHOLE(b.brand, Ctypes.char_star)) = 0 THEN
            StartError ();
            PutText    ("Two types have the same brand: \"");
            PutString  (a.brand);
            PutText    ("\"\n***    ");
            PutType    (a);
            PutText    ("\n***    ");
            PutType    (b);
            EndError   ();
            RETURN FALSE; 
          END;
          *)
          b := b.sibling;
        END;
        a := a.sibling;
      END;
    END;

    (* Reset the sibling pointers. *)
    tp := RTLinker.type_info.types;
    FOR i := 0 TO RTLinker.type_info.nTypes-1 DO
      tp^.sibling := NIL;
      tp^.children := NIL; (* SPIN *)
      INC (tp, ADRSIZE (tp^));
    END;
    RETURN TRUE;
  END CheckBrands;


PROCEDURE HashString (cp: UNTRACED REF CHAR): INTEGER =
  VAR hash := 0;
  BEGIN
    WHILE (cp^ # '\000') DO
      hash := Word.Plus (Word.LeftShift (hash, 1), ORD (cp^));
      INC (cp, BYTESIZE (cp^));
    END;
    RETURN hash;
  END HashString;

PROCEDURE FindChildren (): BOOLEAN =
  VAR tp: TypePtr;  t, p: RT0.TypeDefn;
  BEGIN
    tp := RTLinker.type_info.types;
    FOR i := 0 TO RTLinker.type_info.nTypes-1 DO
      t := tp^;
      IF (t.parentID # 0) THEN
        p := InitFindType (t.parentID);
        IF (p = NIL) THEN 
          BadParent (t);
          RETURN FALSE;
        END;
        t.parent := p;
        t.sibling := p.children;
        p.children := t;
      END;
      INC (tp, ADRSIZE (tp^));
    END;
    RETURN TRUE;
  END FindChildren;

PROCEDURE CheckParents (): BOOLEAN =
  VAR tp: TypePtr;  t, u: RT0.TypeDefn;
  BEGIN
    tp := RTLinker.type_info.types;
    FOR i := 0 TO RTLinker.type_info.nTypes-1 DO
      t := tp^;  u := t;
      WHILE (u # NIL) AND (t # NIL) DO
        t := t.parent;
        u := u.parent;
        IF (u = NIL) THEN EXIT; END;
        u := u.parent;
        IF (t = u) THEN 
          ParentCycle (tp^);
          RETURN FALSE;
        END;
      END;
      INC (tp, ADRSIZE (tp^));
    END;
    RETURN TRUE;
  END CheckParents;

PROCEDURE AssignTypecodes () =
  VAR
    tp, up        : TypePtr;
    t, u          : RT0.TypeDefn;
    next_typecode : INTEGER;
    next_subtype  : INTEGER;
  BEGIN
    (* find the types with reserved typecodes *)
    null  := InitFindType (16_48ec756e);
    text  := InitFindType (16_50f86574);
    root  := InitFindType (16_ffffffff9d8fb489);
    uroot := InitFindType (16_ffffffff898ea789);

    (* assign the fixed typecodes *)
    null.typecode        := RT0.NilTypecode;
    null.subTypeCode     := RT0.NilTypecode;
    null.lastSubTypeCode := RT0.NilTypecode;

    text.typecode        := RT0.TextTypecode;
    text.subTypeCode     := RT0.TextTypecode;
    text.lastSubTypeCode := RT0.TextTypecode;

    (* start counting subtypes *)
    next_subtype  := MAX (RT0.NilTypecode, RT0.TextTypecode) + 1;
    next_typecode := MAX (old_nTypes, next_subtype);

    (* assign the OBJECT typecodes *)
    AssignObjectTypecode (root, next_typecode, next_subtype);
    AssignObjectTypecode (uroot, next_typecode, next_subtype);

    (* assign the remaining REF typecodes *)
    tp := RTLinker.type_info.types;
    FOR i := 0 TO RTLinker.type_info.nTypes-1 DO
      t := tp^;
      IF (t.typecode = LAST (RT0.Typecode)) THEN
        t.typecode := next_typecode;
        INC (next_typecode);
      END;
      IF GetTmpSubTypeCode(t.typecode)^ = LAST (RT0.Typecode) THEN
        GetTmpSubTypeCode(t.typecode)^ := next_subtype;
        GetTmpLastSubTypeCode(t.typecode)^ := next_subtype;
      END;
      INC(next_subtype);
      INC (tp, ADRSIZE (tp^));
    END;

    <* ASSERT next_typecode = RTLinker.type_info.nTypes *>

    (* shuffle the typecells into their correct slots *)
    tp := RTLinker.type_info.types;
    FOR i := 0 TO RTLinker.type_info.nTypes-1 DO
      t := tp^;
      WHILE (t.typecode # i) DO
        up := RTLinker.type_info.types + t.typecode * ADRSIZE (up^);
        IF t.typecode = up^.typecode THEN
          PutText("ERROR >> type information corrupted in reinitialization\n");
          RTOS.Crash();
        END;

        u := up^;
        up^ := t;
        t := u;
      END;
      tp^ := t;
      INC (tp, ADRSIZE (tp^));
    END;
  END AssignTypecodes;

PROCEDURE AssignObjectTypecode (t: RT0.TypeDefn;  
                                VAR next: INTEGER;
                                VAR next_subtype: INTEGER) =
  VAR u: RT0.TypeDefn;
      new: BOOLEAN := FALSE;
  BEGIN
    IF t.typecode = LAST (RT0.Typecode) THEN
      t.typecode := next;  
      INC (next);
      new := TRUE;
    END;
    GetTmpSubTypeCode(t.typecode)^ := next_subtype;
    INC (next_subtype);

    u := t.children;
    WHILE (u # NIL) DO
      AssignObjectTypecode (u, next, next_subtype);
      u := u.sibling;
    END;
    GetTmpLastSubTypeCode(t.typecode)^ := next_subtype-1;
  END AssignObjectTypecode;

PROCEDURE FixLinks (): BOOLEAN =
  VAR
    mi   : RT0.ModulePtr;
    t, u : UNTRACED REF RT0.TypeLink;
    defn : RT0.TypeDefn;
  BEGIN
    FOR i := RTLinker.n_initialized TO RTLinker.info.n_modules - 1 DO
      mi := GetModule (i);
      t := mi.type_cell_ptrs;
      WHILE (t # NIL) DO
        u := t.next;
        defn := InitFindType (t.type);
        IF (defn = NIL) THEN
          BadTypeId (mi, t.type);
          RETURN FALSE;
        END;
        t.next := defn;
        t.type := defn.typecode;
        t := u;
      END;
    END;
    RETURN TRUE;
  END FixLinks;

PROCEDURE FixSizes (): BOOLEAN =
  (* fix the data(method) sizes and offsets *)
  VAR t: RT0.TypeDefn;  tp: TypePtr;
  BEGIN
    (* make sure that all the REF types are some multiple of header words *)
    tp := RTLinker.type_info.types;
    FOR i := 0 TO RTLinker.type_info.nTypes-1 DO
      t := tp^;
      IF t.typecode >= old_nTypes
        AND (t.parent = NIL)
        AND (t.children = NIL) THEN
        t.dataSize := RTMisc.Upper (t.dataSize, BYTESIZE (RTHeapRep.Header));
      END;
      INC (tp, ADRSIZE (tp^));
    END;

    (* fix the objects *)
    IF NOT FixObjectSizes (root) THEN RETURN FALSE; END;
    IF NOT FixObjectSizes (uroot) THEN RETURN FALSE; END;
    RETURN TRUE;
  END FixSizes;

PROCEDURE FixObjectSizes (t: RT0.TypeDefn): BOOLEAN =
  VAR u: RT0.TypeDefn;
  BEGIN
    IF t.typecode >= old_nTypes THEN
      (* fix my sizes *)
      u := t.parent;
      IF (u # NIL) THEN
        (* XXX we use elementSize to compute offset, so offset does
           not get aligned to header boundaries.  This allows us to 
           have headers larger than a word.
        *)
        t.dataOffset := RTMisc.Upper (u.elementSize, t.dataAlignment);
        INC (t.dataSize, t.dataOffset);
        t.dataAlignment := MAX (t.dataAlignment, u.dataAlignment);
        t.methodOffset := u.methodSize;
        INC (t.methodSize, t.methodOffset);
      END;
      t.elementSize := t.dataSize; (* unaligned type data size *)
      t.dataSize := RTMisc.Upper (t.dataSize, BYTESIZE (RTHeapRep.Header));
      
      (* allocate my default method list *)
      t.defaultMethods := Cstdlib.malloc (t.methodSize);
      IF (t.defaultMethods = NIL) THEN
        StartError ();
        PutText ("unable to allocate method suite for ");
        PutType (t);
        EndError ();
        RETURN FALSE;
      END;
    END;

    (* fix my children *)
    u := t.children;
    WHILE (u # NIL) DO
      IF NOT FixObjectSizes (u) THEN RETURN FALSE; END;
      u := u.sibling;
    END;
    RETURN TRUE;
  END FixObjectSizes;

PROCEDURE CallSetupProcs () =
  VAR t: RT0.TypeDefn;  tp: TypePtr;
  BEGIN
    (* set up the REF types *)
    tp := RTLinker.type_info.types;
    FOR i := 0 TO RTLinker.type_info.nTypes-1 DO
      t := tp^;
      IF t.typecode >= old_nTypes THEN
        IF (t.parent = NIL) AND (t.children = NIL) AND (t.linkProc # NIL) THEN
          t.linkProc (t);
        END;
      END;
      INC (tp, ADRSIZE (tp^));
    END;

    (* set up the objects *)
    SetupObject (root);
    SetupObject (uroot);
  END CallSetupProcs;

PROCEDURE SetupObject (t: RT0.TypeDefn) =
  VAR u: RT0.TypeDefn;  a: UNTRACED REF ADDRESS;
  BEGIN
    IF t.typecode >= old_nTypes THEN
      (* initialize my method suite from my parent *)
      u := t.parent;
      IF (u # NIL) THEN
        RTMisc.Copy (u.defaultMethods, t.defaultMethods, u.methodSize);
      END;
      LOOPHOLE (t.defaultMethods, UNTRACED REF INTEGER)^ := t.typecode;
      
      (* initialize any remaining methods to the undefined procedure *)
      a := t.defaultMethods + ADRSIZE (ADDRESS);
      FOR j := 1 TO t.methodSize DIV BYTESIZE (ADDRESS) - 1 DO
        IF (a^ = NIL) THEN a^ := LOOPHOLE (UndefinedMethod, ADDRESS) END;
        INC (a, ADRSIZE (ADDRESS));
      END;
      
      (* call my setup proc *)
      IF (t.linkProc # NIL) THEN t.linkProc (t) END;
    END;

    (* set up my children *)
    u := t.children;
    WHILE (u # NIL) DO
      SetupObject (u);
      u := u.sibling;
    END;
  END SetupObject;

PROCEDURE CheckRevelations (): BOOLEAN =
  VAR
    mi  : RT0.ModulePtr;
    r   : RT0.RevPtr;
    lhs : RT0.TypeDefn;
    rhs : RT0.TypeDefn;
  BEGIN
    FOR i := 0 TO RTLinker.info.n_modules - 1 DO
      mi := GetModule (i);
      r := mi.partial_rev;
      IF (r # NIL) THEN
        WHILE (r.lhs_id # 0) DO
          lhs := InitFindType (r.lhs_id);
          rhs := InitFindType (r.rhs_id);
          IF (lhs = NIL) OR (rhs = NIL)
            OR (lhs.subTypeCode < rhs.subTypeCode)
            OR (rhs.lastSubTypeCode < lhs.subTypeCode) 
           THEN
            BadRevelation (mi, r, lhs, rhs);
            RETURN FALSE;
          END;
          INC (r, ADRSIZE (r^));
        END;
      END;
    END;
    RETURN TRUE;
  END CheckRevelations;

(*-------------------------------------------------------- runtime errors ---*)

PROCEDURE UndefinedMethod () =
  BEGIN
    RTMisc.FatalError (NIL, 0, "attempted invocation of undefined method");
  END UndefinedMethod;

PROCEDURE BadType (tc: Typecode) =
  BEGIN
    RTMisc.FatalErrorI ("improper typecode: ", tc);
  END BadType;

(*----------------------------------------------------------- init errors ---*)

PROCEDURE StartError () =
  BEGIN
    PutText ("\n\n***\n*** ");
  END StartError;

PROCEDURE EndError () =
  BEGIN
    PutText ("\n***\n");
    PutText ("ERROR >> unable to initialize runtime types");
    Flush ();
  END EndError;

PROCEDURE BadTypeId (mi: RT0.ModulePtr;  id: INTEGER) =
  BEGIN
    StartError ();
    PutText    ("unable to resolve type id: ");
    PutHex     (id);
    PutText    ("\n***    in ");
    PutModule  (mi);
    EndError   ();
  END BadTypeId;

<* UNUSED *>
PROCEDURE DuplicateLHS (mi: RT0.ModulePtr;  r: RT0.RevPtr;  t: RT0.TypeDefn) =
  BEGIN
    StartError ();
    PutText    ("opaque type redefined: ");
    PutText    ("\n***    REVEAL _t");
    PutHex     (r.lhs_id);
    PutText    (" = _t");
    PutHex     (r.rhs_id);
    PutText    ("\n***    in ");
    PutModule  (mi);
    PutText    ("\n***    but, already = ");
    PutType    (t);
    EndError   ();
  END DuplicateLHS;

PROCEDURE UndefinedRHS (mi: RT0.ModulePtr;  r: RT0.RevPtr) =
  BEGIN
    StartError ();
    PutText    ("opaque type revealed as undefined type: ");
    PutText    ("\n***    REVEAL _t");
    PutHex     (r.lhs_id);
    PutText    (" = _t");
    PutHex     (r.rhs_id);
    PutText    ("\n***    in ");
    PutModule  (mi);
    EndError   ();
  END UndefinedRHS;

PROCEDURE BadParent (t: RT0.TypeDefn) =
  BEGIN
    StartError ();
    PutText    ("super type undefined:\n***    child = ");
    PutType    (t);
    PutText    ("\n***    parent = _t");
    PutHex     (t.parentID);
    EndError   ();
  END BadParent;

PROCEDURE ParentCycle (t: RT0.TypeDefn) =
  VAR u: RT0.TypeDefn;
  BEGIN
    StartError ();
    PutText    ("illegal cycle in super types:\n***    child  = ");
    PutType    (t);
    u := t.parent;
    WHILE (u # NIL) DO
      PutText    ("\n***    parent = ");
      PutType    (u);
      IF (u = t) THEN EXIT; END;
      u := u.parent;
    END;
    EndError   ();
  END ParentCycle;

PROCEDURE BadRevelation (mi: RT0.ModulePtr;  r: RT0.RevPtr;
                         lhs, rhs: RT0.TypeDefn) =
  BEGIN
    StartError ();
    PutText    ("inconsistent partial revelation: ");
    PutText    ("\n***    REVEAL _t");
    PutHex     (r.lhs_id);
    PutText    (" <: _t");
    PutHex     (r.rhs_id);
    PutText    ("\n***           ");
    PutType    (lhs);
    PutText    (" <: ");
    PutType    (rhs);
    PutText    ("\n***    in ");
    PutModule  (mi);
    EndError   ();
  END BadRevelation;

(*---------------------------------------------------- internal debugging ---*)

PROCEDURE ShowTypes (full := TRUE) =
  VAR t: RT0.TypeDefn;
  BEGIN
    PutText ("Here are the types: nTypes = ");
    PutInt  (RTLinker.type_info.nTypes);
    PutText ("\n");
    FOR i := 0 TO RTLinker.type_info.nTypes-1 DO
      t := Get (i);
      IF t # NIL THEN
        PutType (t, full); PutText ("\n");
        IF full THEN
          PutText ("  data   ");
          PutText ("  S= "); PutInt (t.dataSize);
          PutText ("  A= "); PutInt (t.dataAlignment);
          PutText ("  O= "); PutInt (t.dataOffset);
          PutText ("\n");
          IF (t.methodSize # 0) OR (t.methodOffset # 0) THEN
            PutText ("  method ");
            PutText ("  S= ");  PutInt (t.methodSize);
            PutText ("  O= ");  PutInt (t.methodOffset);
            PutText ("\n");
          END;
          IF (t.nDimensions # 0) OR (t.elementSize # 0) THEN
            PutText (" array   ");
            PutText ("  D= ");  PutInt (t.nDimensions);
            PutText ("  S= ");  PutInt (t.elementSize);
            PutText ("\n");
          END;
        END;
      END;
    END;
    Flush ();
    EVAL ShowTypes; (* to prevent an "unused symbol" warning *)
  END ShowTypes;

PROCEDURE PutType (t: RT0.TypeDefn; full: BOOLEAN := TRUE) =
  BEGIN
    IF full THEN
      PutText ("[");
      PutAddr (t);
    END;

    IF (t # NIL) THEN
      IF full THEN
        PutText (" _t");
        PutHex  (t.selfID);
        PutText (" ");
      END;

      PutText (" typecode= ");
      PutInt  (t.typecode, 17);
      PutText (" subtypes= [ ");
      PutInt  (t.subTypeCode, 3);
      PutText (" .. ");
      PutInt  (t.lastSubTypeCode, 3);
      PutText (" ]");

      IF (t.name # NIL) THEN
        PutText   ("  ");
        PutString (t.name);
      END;
    END;

    IF full THEN
      PutText ("]");
    END;
  END PutType;

PROCEDURE PutModule (mi: RT0.ModulePtr) =
  BEGIN
    IF (mi.file = NIL)
      THEN PutText ("<unknown file name for a module>");
      ELSE PutString (mi.file);
    END;
  END PutModule;

PROCEDURE GetModule (m: INTEGER): RT0.ModulePtr = 
  VAR 
    p : UNTRACED REF (*ARRAY OF*) RT0.ModulePtr := RTLinker.info.modules;
  BEGIN
    IF (m >= RTLinker.info.n_modules) THEN
      RTMisc.FatalErrorI ("improper module index: ", m);
    END;
    p := p + m * ADRSIZE (RT0.ModulePtr);
    RETURN p^;
  END GetModule;

PROCEDURE Update () =
  VAR t: TypePtr;
  BEGIN
    WITH ti = RTLinker.type_info DO
      t := ti.types;
      FOR i := 0 TO ti.nTypes-1 DO
        t^.subTypeCode := GetTmpSubTypeCode(t^.typecode)^;
        t^.lastSubTypeCode := GetTmpLastSubTypeCode(t^.typecode)^;
        INC(t, ADRSIZE(t^));
      END;
      type_ids := ti.type_ids;
      n_type_ids := ti.n_type_ids;
      RTTypeSecurity.Reinitialize(ti.nTypes);
    END;
  END Update;

PROCEDURE GetTmpSubTypeCode(i: INTEGER): UNTRACED REF INTEGER =
  BEGIN
    IF i < 0 OR i >= RTLinker.type_info.nTypes THEN
      PutText("ERROR >> out of bound index in RTType.GetTmpSubTypeCode\n");
      PutInt(i); PutText(" ");PutInt(RTLinker.type_info.nTypes); PutText("\n");
    END;
    RETURN RTLinker.type_info.subTypeCode + i * ADRSIZE(INTEGER);
  END GetTmpSubTypeCode;

PROCEDURE GetTmpLastSubTypeCode(i: INTEGER): UNTRACED REF INTEGER =
  BEGIN
    IF i < 0 OR i >= RTLinker.type_info.nTypes THEN
      PutText("ERROR >> out of bound index in RTType.GetTmpLastSubTypeCode\n");
      PutInt(i); PutText(" ");PutInt(RTLinker.type_info.nTypes); PutText("\n");
    END;
    RETURN RTLinker.type_info.lastSubTypeCode + i * ADRSIZE(INTEGER);
  END GetTmpLastSubTypeCode;

BEGIN
END RTType.


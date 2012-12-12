(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 16-Dec-97  Przemek Pardyak (pardy) at the University of Washington
 *	Added "{" and "}" to denote atomic parts of a command.
 *
 * 05-May-96  Charles Garrett (garrett) at the University of Washington
 *	Aded the target variable which is set to either ALPHA_SPIN or
 *	 ALPHA_SPIN_PROF to indicate what target was used to build the
 *	 kernel. This variable is used by scripts to load extensions that
 *	 were built for the same target.
 *
 * 22-Feb-96  Charles Garrett (garrett) at the University of Washington
 *	Added variable "loadpath" which specifies the root directory to
 *	 search when loading extensions.
 *
 * 27-Dec-95  Emin Gun Sirer (egs) at the University of Washington
 *	Whisted. Globbing support.
 *)
MODULE Glob;

IMPORT IO, ASCII, Text, TextTbl, TextExtras, ParseParams;

REVEAL T = BRANDED REF RECORD
	tbl: TextTbl.Default;
        parent: T;
        readOnly: BOOLEAN;
END;



TYPE WalkProcedure = PROCEDURE(cur: T): BOOLEAN;




PROCEDURE New(parent: T) : T =
  VAR c: T;
  BEGIN
    c := NEW(T);
    c.parent := parent;
    c.tbl := NEW(TextTbl.Default).init(50);
    c.readOnly := FALSE;
    RETURN c;
  END New;

PROCEDURE Freeze(ctxt: T): BOOLEAN =
  BEGIN
    IF ctxt^.readOnly THEN RETURN FALSE; END;
    ctxt^.readOnly := TRUE;
    RETURN TRUE;
  END Freeze;


(*
 * Walk up the context tree until no more parents, or the walker procedure
 * returns TRUE.  Return TRUE iff some walker procedure returns TRUE.
 *)

PROCEDURE WalkUp(ctxt: T; walker: WalkProcedure): BOOLEAN =
  VAR c: T;
  BEGIN
    c := ctxt;
    LOOP
      IF c = NIL THEN RETURN FALSE; END;
      IF walker(c) THEN RETURN TRUE; END;
      c := c^.parent;
    END;
  END WalkUp;
  


PROCEDURE Lookup(ctxt: T; from: TEXT): TEXT RAISES {Error} =
  VAR to: TEXT;

  PROCEDURE Find(cur: T) : BOOLEAN =
    BEGIN
      RETURN cur.tbl.get(from, to);
    END Find;
      
  BEGIN
    IF NOT WalkUp(ctxt, Find) THEN RAISE Error; END;
    RETURN to;
  END Lookup;


PROCEDURE GetVariable (ctxt: T; from: TEXT): TEXT =
  BEGIN
    TRY RETURN Lookup(ctxt, from); EXCEPT Error => RETURN NIL; END;
  END GetVariable;

PROCEDURE SetVariable (ctxt: T; from, to: TEXT) =
  BEGIN
    IF NOT ctxt.readOnly THEN EVAL ctxt.tbl.put(from, to); END;
  END SetVariable;

PROCEDURE GetVariableList (ctxt: T): REF ARRAY OF TEXT =
  VAR
    size: CARDINAL;
    a   : REF ARRAY OF TEXT;
    b   : REF ARRAY OF TEXT;

  PROCEDURE Count (cur: T): BOOLEAN =
    BEGIN
      size := size + cur.tbl.size();
      RETURN FALSE;              (* Keep iterating *)
    END Count;

  PROCEDURE Iterate (cur: T)  : BOOLEAN=
    VAR
      itr       := cur.tbl.iterate();
      k: CARDINAL;
      var: TEXT;
      val: TEXT;
    BEGIN
      WHILE itr.next(var, val) DO
        k := 0;
        LOOP
          IF a[k] = NIL THEN
            a[k] := var;          (* remember this shell variable *)
            INC(size);
          ELSIF Text.Equal(var, a[k]) THEN
            EXIT;                (* repeated in parent *)
          ELSE
            INC(k);
          END;
          IF k = NUMBER(a^) THEN EXIT; END;
        END;
      END;
      RETURN FALSE;              (* Keep iterating *)
    END Iterate;

  BEGIN
    EVAL WalkUp(ctxt, Count);
    a := NEW(REF ARRAY OF TEXT, size);
    size := 0;
    EVAL WalkUp(ctxt, Iterate);

    IF size = NUMBER(a^) THEN
      RETURN a;
    ELSE
      b := NEW(REF ARRAY OF TEXT, size);
      b^ := SUBARRAY(a^, 0, size);
      (* drop a on the floor *)
      RETURN b;
    END;
  END GetVariableList;


  
PROCEDURE Translate (ctxt: T; t: TEXT): TEXT RAISES {Error} =
  VAR
    from: CARDINAL;
    to  : CARDINAL;
    i   : INTEGER;
    t1  : TEXT;
    t2  : TEXT;
    t3  : TEXT;
    t4  : TEXT;
    magic: CHAR;
  BEGIN
    from := 0;
    IF NOT TextExtras.FindCharSet(t, MagicCharacters, from) THEN
      RETURN t;
    ELSE
      magic := Text.GetChar(t, from);
    END;

    t1 := Text.Sub(t, 0, from);  (* everything up to the $ *)
    t2 := Text.Sub(t, from + 1); (* everything following the $ *)
    to := 0;

    IF Text.FindChar(t2, '{') = 0 THEN
      i := Text.FindChar(t2, '}');
      IF i = -1 THEN
        IO.Put(t2 & ": Unbalanced '{'\n");
        RAISE Error;
      END;
      t3 := Text.Sub(t2, i+1); (* everything following the variable and '}' *)
      t2 := Text.Sub(t2, 1, i-1); (* the variable *)
      IO.Put("FOUND: " & t3 & " " & t2 & "\n");
    ELSIF TextExtras.FindCharSet(t2, ASCII.All-ASCII.AlphaNumerics, to) THEN
      t3 := Text.Sub(t2, to);    (* everything following the variable *)
      t2 := Text.Sub(t2, 0, to); (* the variable *)
    ELSE
      t3 := NIL;                 (* nothing follows the variable *)
    END;

    TRY
      IF magic = SubChar THEN
          t2 := Lookup(ctxt, t2);          (* convert *)
      ELSIF magic = HomeChar THEN
        t2 := Lookup(ctxt, "home");
      END;
    EXCEPT
      Error => IO.Put(t2 & ": Undefined variable\n"); RAISE Error;
    END;

    IF t2 = NIL THEN t2 := "NIL"; END;
    IF t3 # NIL THEN t4 := t2 & Translate(ctxt, t3); ELSE t4 := t2; END;
    IF t1 # NIL THEN RETURN t1 & t4; ELSE RETURN t4; END;

  END Translate;

PROCEDURE Substitute (ctxt : T;
		      pp: ParseParams.T): ParseParams.T RAISES {Error} =
  BEGIN
    FOR i := FIRST(pp.arg^) TO LAST(pp.arg^) DO
      pp.arg^[i] := Translate(ctxt, pp.arg^[i]);
    END;
    RETURN pp;
  END Substitute;


PROCEDURE IsTrue(ctxt: T; t: TEXT) : BOOLEAN =
  BEGIN
    TRY
      (* If a shell variable exists, and it's not NIL, then... *)
      RETURN NOT Text.Equal(Lookup(ctxt, t), "NIL");
    EXCEPT Error => RETURN FALSE;
    END;
  END IsTrue;
    
BEGIN
END Glob.

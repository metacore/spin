(*
 *
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 *)

(*
 * HISTORY
 *
 *)


UNSAFE MODULE Expression EXPORTS Expression, ExpressionPrivate;

IMPORT Wr, Fmt, Stdio, Word, Text, Thread;
IMPORT Ir;
IMPORT Utils;

<* FATAL Thread.Alerted, Wr.Failure *>

CONST
  DebugJoin = TRUE;
  Sanity = TRUE;

REVEAL
  T = Public BRANDED OBJECT
  OVERRIDES
    eval := Self;
  END;

  Immed = T OBJECT value : INTEGER; END BRANDED OBJECT
  OVERRIDES
    toText := ImmedToText;
    copy := CopyImmed;
  END;

  Arg = T OBJECT num : CARDINAL; END BRANDED OBJECT
  OVERRIDES
    toText := ArgToText;
    copy := CopyArg;
  END;

  SP = T BRANDED OBJECT
  OVERRIDES
    toText := SPToText;
    copy := Self;
  END;

  Reg = T OBJECT
    reg : Ir.Register;
  END BRANDED OBJECT
  OVERRIDES
    toText := RegToText;
    copy := CopyReg;
  END;

  Compound = T OBJECT
    op   : Opcode;
    left, right : T;
  END BRANDED OBJECT
  OVERRIDES
    canonicalize := Canonicalize;
    optimize := Optimize;
    eval := CompoundEval;
    toText := CompoundToText;
    copy := CopyCompound;
  END;

  Top = T BRANDED OBJECT
  OVERRIDES
    toText := TopToText;
    copy := Self;
  END;

  Bottom = T BRANDED OBJECT
  OVERRIDES
    toText := BottomToText;
    copy := Self;
  END;

  Call = T OBJECT
    target : INTEGER;
    last : CARDINAL := 5;
    args : ARRAY [0..5] OF T;
  END BRANDED OBJECT
  OVERRIDES
    toText := CallToText;
    copy := CopyCall;
  END;

  Return = T BRANDED OBJECT
  OVERRIDES
    toText := ReturnToText;
    copy := Self;
  END;

  (* multiple values from different guards *)
  Wildcard = WildcardPublic OBJECT
    values: ImmedArray;
    next  : CARDINAL;
  END BRANDED OBJECT
  OVERRIDES
    addValue := WildAdd;
    addNegateValue := WildAddNegate;
    toText := WildToText;
    copy := CopyWildcard;
  END;

  (* merge a la SSA *)
  Phi = T OBJECT METHODS
    addExpr (t: T);
  END OBJECT
    exprs: TArray;
    next : CARDINAL;
  END BRANDED OBJECT
  OVERRIDES
    toText := PhiToText;
    addExpr := PhiAdd;
    copy := CopyPhi;
  END;

(*          creators           *)

PROCEDURE NewCall (target: INTEGER;
                   a0, a1, a2, a3, a4, a5: T;
                   number: CARDINAL := 6) : Call =
  VAR nc: Call := NEW (Call);
  BEGIN
    nc.last := number-1;
    nc.target := target;
    nc.args[0] := a0;
    nc.args[1] := a1;
    nc.args[2] := a2;
    nc.args[3] := a3;
    nc.args[4] := a4;
    nc.args[5] := a5;
    RETURN nc;
  END NewCall;


PROCEDURE NewWild () : Wildcard =
  VAR
    w := NEW (Wildcard);
  BEGIN
    w.values := NEW (ImmedArray, 10);
    w.next := 0;
    RETURN w;
  END NewWild;



PROCEDURE NewPhi () : Phi =
  VAR
    w := NEW (Phi);
  BEGIN
    w.exprs := NEW (TArray, 10);
    w.next := 0;
    RETURN w;
  END NewPhi;


(* methods *)

PROCEDURE WildAdd (w: Wildcard; x: Immed; offset: CARDINAL) =
  VAR
    new : ImmedArray;
  BEGIN
    IF w.next = LAST (w.values^) THEN
      new := NEW (ImmedArray, 2*w.next);

      SUBARRAY (new^, 0, w.next+1) := w.values^;
      w.values := new;
    END;
    w.values[w.next].immed := x;
    w.values[w.next].mark := FALSE;
    w.values[w.next].offset := offset;
    INC (w.next);
  END WildAdd;


PROCEDURE WildAddNegate (w: Wildcard; x: Immed; offset: CARDINAL) =
  VAR
    new : ImmedArray;
  BEGIN
    IF w.next = LAST (w.values^) THEN
      new := NEW (ImmedArray, 2*w.next);

      SUBARRAY (new^, 0, w.next+1) := w.values^;
      w.values := new;
    END;
    w.values[w.next].immed := x;
    w.values[w.next].mark := TRUE;
    w.values[w.next].offset := offset;
    INC (w.next);
  END WildAddNegate;


PROCEDURE PhiAdd (p: Phi; x: T) =
  VAR
    new : TArray;
  BEGIN
    IF p.next = LAST (p.exprs^) THEN
      new := NEW (TArray, 2*p.next);
      FOR i := 0 TO p.next DO
        new[i] := p.exprs[i];
      END;
      p.exprs := new;
    END;
    p.exprs[p.next] := x;
    INC (p.next);
  END PhiAdd;


(* merging expressions *)

PROCEDURE Join (e1, e2: T; count: INTEGER) : T RAISES {NoMatch,Problem} =
  (* assumption: wildcards should always be on the left *)
  VAR
    newT : T;
  BEGIN
    TRY
      newT := JoinInternal (e1, e2, FALSE, count, FALSE);
    EXCEPT
      NoMatch =>
      IF DebugJoin THEN
        Wr.PutText (Stdio.stdout, "Join failed, e1 is:\n");
        Print (e1);
        Wr.PutText (Stdio.stdout, "Join failed, e2 is:\n");
        Print (e2);
      END;
      RAISE NoMatch;
    END;
    RETURN newT;
  END Join;

PROCEDURE JoinInternal (e1, e2: T; inCompare: BOOLEAN; count: INTEGER;
                        negate: BOOLEAN)
  : T RAISES {NoMatch,Problem} =
  BEGIN
    (* stupid cases *)
    IF e1 = NIL THEN RETURN e2; END;
    IF e2 = NIL THEN RETURN e1; END;
    IF e1 = e2 THEN RETURN e1; END;
    IF e1 = TopValue THEN RETURN e2; END;
    IF e2 = TopValue THEN RETURN e1; END;

    (* make sure that the expressions "look" the same *)
    IF TYPECODE (e2) = TYPECODE (Wildcard) THEN RAISE Problem; END;
    IF TYPECODE (e1) # TYPECODE (Wildcard) AND
      TYPECODE (e1) # TYPECODE (e2) THEN
      IF DebugJoin THEN
        Utils.Error ("Bailing in typecode");
      END;
      RAISE NoMatch;
    END;

    TYPECASE e1 OF
    | Arg (e1a) =>
      WITH e2a = NARROW (e2, Arg) DO
        IF e1a.num = e2a.num THEN
          RETURN e1;
        ELSE
          IF DebugJoin THEN
            Utils.Error ("Bailing in args");
          END;
          RAISE NoMatch;
        END;
      END;
    | Immed (e1i) =>
      WITH e2i = NARROW (e2, Immed) DO
        IF NOT inCompare THEN
          IF e1i.value = e2i.value THEN
            RETURN e1;
          ELSE
            IF DebugJoin THEN
              Utils.Error ("Bailing in Immed");
            END;
            RAISE NoMatch;
          END;
        ELSE
          VAR
            newWild := NewWild ();
          BEGIN
            newWild.addValue (e1i, count-1);
            IF negate THEN
              newWild.addNegateValue (e2i, count);
            ELSE
              newWild.addValue (e2i, count);
            END;
            newWild.edge := e1i.edge;
            newWild.def := e1i.def;
            RETURN newWild;
          END;
        END;
      END;
    | Phi (e1p) =>
      WITH e2p = NARROW (e2, Phi) DO
        IF e1p.next # e2p.next THEN
          IF DebugJoin THEN Utils.Error ("Bailing in Phi last"); END;
          RAISE NoMatch;
        END;
        VAR new := NewPhi ();
        BEGIN
          FOR i := 0 TO e1p.next-1 DO
            new.addExpr (Join (e1p.exprs[i], e2p.exprs[i], count));
            IF new.exprs[i] = NIL THEN
              new := NIL; (* make new Phi object unreachable *)
              IF DebugJoin THEN Utils.Error ("Bailing in Phi element"); END;
              RAISE NoMatch;
            END;
          END;
          RETURN new;
        END;
      END;
    | Call (e1c) =>
      WITH e2c = NARROW (e2, Call) DO
        IF e1c.last # e2c.last OR e1c.target # e2c.target THEN
          IF DebugJoin THEN Utils.Error ("Bailing in Call count/target"); END;
          RAISE NoMatch;
        END;
        VAR new := NEW (Call);
        BEGIN
          (* TODO: once we know the exact number of args from the
             procedure type information, then we can make this more
             precise *)
          FOR i := 0 TO e1c.last DO
            new.args[i] := Join (e1c.args[i], e2c.args[i], count);
            IF new.args[i] = NIL THEN
              new := NIL; (* make object unreachable *)
              IF DebugJoin THEN Utils.Error ("Bailing in Call argument"); END;
              RAISE NoMatch;
            END;
          END;
          new.target := e1c.target;
          new.last := e1c.last;
          RETURN new;
        END;
      END;
    | Wildcard (e1w) =>
      IF TYPECODE (e2) # TYPECODE (Immed) THEN
        IF DebugJoin THEN Utils.Error ("Bailing in Wildcard"); END;
        RAISE NoMatch;
      END;
      WITH e2w = NARROW (e2, Immed) DO
        IF negate THEN
          e1w.addNegateValue (e2w, count);
        ELSE
          e1w.addValue (e2w, count);
        END;
        RETURN e1;
      END;
    | Reg (e1r) =>
      WITH e2r = NARROW (e2, Reg) DO
        IF e1r.reg = e2r.reg THEN
          RETURN e1;
        ELSE
          IF DebugJoin THEN Utils.Error ("Bailing in Reg"); END;
          RAISE NoMatch;
        END;
      END;
    | Compound (e1c) =>
      WITH e2c = NARROW (e2, Compound) DO
        VAR
          new := NEW (Compound);
          inCompare :=
              (e1c.op >= Opcode.cmpeq AND e1c.op <= Opcode.cmpult) OR
              (e1c.op >= Opcode.cmpne AND e1c.op <= Opcode.cmpgt);
        BEGIN
          IF e1c.op # e2c.op THEN
            IF (e1c.op = Opcode.cmpne AND e2c.op = Opcode.cmpeq) OR
              (e1c.op = Opcode.cmpeq AND e2c.op = Opcode.cmpne) THEN
              
              (* hackish *)
              new.left := JoinInternal (e1c.left, e2c.left, inCompare, count, TRUE);
              IF new.left = NIL THEN
                new := NIL; (* make it unreachable *)
                IF DebugJoin THEN
                  Utils.Error ("Bailing in Compound.left");
                  Print (e1c.left);
                  Print (e2c.left);
                END;
                RAISE NoMatch;
              END;

              new.right := JoinInternal (e1c.right, e2c.right, inCompare, count, TRUE);
              IF new.right = NIL THEN
                new := NIL; (* make it unreachable *)
                IF DebugJoin THEN Utils.Error ("Bailing in Compound.right"); END;
                RAISE NoMatch;
              END;

              new.op := e1c.op;
              new.def := e1c.def;
              RETURN new;
            ELSE
              IF DebugJoin THEN
                Utils.Error ("Bailing in Compound op");
                
                Print (e1c);
                Print (e2c);
              END;
              RAISE NoMatch;
            END;
          ELSE (* e1c.op = e2c.op *)
            (* left must match *)
            new.left := JoinInternal (e1c.left, e2c.left, inCompare, count, negate);
            IF new.left = NIL THEN
              new := NIL;
              IF DebugJoin THEN Utils.Error ("Bailing in Compound.left"); END;
              RAISE NoMatch;
            END;

            new.right := JoinInternal (e1c.right, e2c.right, inCompare, count, negate);
            IF new.right = NIL THEN
              new := NIL; (* make it unreachable *)
              IF DebugJoin THEN Utils.Error ("Bailing in Compound.right"); END;
              RAISE NoMatch;
            END;
            
            (* initialize back links to code *)
            new.def := e1.def;
            new.edge := e1.edge; 
           
            new.op := e1c.op;
            RETURN new;
          END;
        END;
      END;
    ELSE
      (* uh oh *)
      Utils.Error ("Problem with e1: " & ToText (e1) & " e2: " & ToText (e2));
      RAISE Problem;
    END;
  END JoinInternal;

(* remove WILDCARD (x, x, x, ...) nodes *)
PROCEDURE Collapse (e: T) : CARDINAL RAISES {Problem} =
  (* modifies: e *)

  VAR
    count := 0;

  (* nested to pass down count *)
  PROCEDURE CollapseInternal (e: T) : T RAISES {Problem} =
    VAR
      imm : Immed;
      mark : BOOLEAN;
      test : BOOLEAN;
    BEGIN
      IF e = NIL THEN RETURN NIL; END;
      TYPECASE e OF
      | Arg, Immed, Reg, Top, Bottom =>
        RETURN e;
      | Phi (ep) =>
        FOR i := 0 TO ep.next-1 DO
          ep.exprs[i] := CollapseInternal (ep.exprs[i]);
        END;
      | Call (ec) =>
        FOR i := 0 TO ec.last DO
          ec.args[i] := CollapseInternal (ec.args[i]);
        END;
      | Wildcard (ew) =>
        test := TRUE;
        imm := ew.values[0].immed;
        mark := ew.values[0].mark;

        (* compare all of the values *)
        FOR i := 1 TO ew.next-1 DO
          IF imm.value # ew.values[i].immed.value OR mark # ew.values[i].mark THEN
            test := FALSE;
            EXIT;
          END;
        END;
        
        (* if the values are the same, collapse the wildcard *)
        IF test THEN
          RETURN imm;
        END;

        (* there is still a wildcard *)
        INC (count);
      | Compound (ec) =>
        ec.left := CollapseInternal (ec.left);
        ec.right :=  CollapseInternal (ec.right);
      ELSE
        (* uh oh *)
        Utils.Error ("Problem with e: " & ToText (e));
        RAISE Problem;
      END;

      RETURN e;
    END CollapseInternal;

  BEGIN
    EVAL CollapseInternal (e);
    RETURN count;
  END Collapse;


PROCEDURE Match (e1, e2: T) : T RAISES {Problem} =
  BEGIN
    IF e1 = e2 THEN RETURN e1; END;
    IF TYPECODE (e1) = TYPECODE (Top) THEN RETURN e2; END;
    IF TYPECODE (e2) = TYPECODE (Top) THEN RETURN e1; END;
    IF TYPECODE (e1) # TYPECODE (e2) THEN RETURN NIL; END;
    TYPECASE e1 OF
    | Arg (e1a) =>
      WITH e2a = NARROW (e2, Arg) DO
        IF e1a.num = e2a.num THEN
          RETURN e1;
        ELSE
          RETURN NIL;
        END;
      END;
    | Immed (e1i) =>
      WITH e2i = NARROW (e2, Immed) DO
        IF e1i.value = e2i.value THEN
          RETURN e1;
        ELSE
          RETURN NIL;
        END;
      END;
    | Reg (e1r) =>
      WITH e2r = NARROW (e2, Reg) DO
        IF e1r.reg = e2r.reg THEN
          RETURN e1;
        ELSE
          RETURN NIL;
        END;
      END;
    | Compound (e1c) =>
      WITH e2c = NARROW (e2, Compound) DO
        (* compounds better not have TOP as their elements *)
        <* ASSERT TYPECODE (e1c.left) # TYPECODE (Top) *>
        <* ASSERT TYPECODE (e2c.left) # TYPECODE (Top) *>
        <* ASSERT TYPECODE (e1c.right) # TYPECODE (Top) *>
        <* ASSERT TYPECODE (e2c.right) # TYPECODE (Top) *>

        IF e1c.op = e2c.op AND Match (e1c.left, e2c.left) # NIL AND
          Match (e1c.right, e2c.right) # NIL THEN
          RETURN e1;
        ELSE
          RETURN NIL;
        END;
      END;
    | Call (e1c) =>
      WITH e2c = NARROW (e2, Call) DO
        IF e1c.target # e2c.target THEN RETURN NIL; END;
        IF e1c.last # e2c.last THEN RETURN NIL; END;

        FOR i := 0 TO 5 DO
          IF Match (e1c.args[i], e2c.args[i]) = NIL THEN
            RETURN NIL;
          END;
        END;
        RETURN e1;
      END;
    ELSE
      (* uh oh *)
      RAISE Problem;
    END;
  END Match;


(* take multiple reaching definitions and create a join point *)
PROCEDURE Merge (e1, e2: T) : T RAISES {Problem} =
  VAR newPhi : Phi;
  BEGIN
    IF e1 = NIL THEN RETURN e2; END;

    (* rhs cannot be Phi *)
    IF TYPECODE (e2) = TYPECODE (Phi) THEN RAISE Problem; END;

    TYPECASE e1 OF
    | Phi (e1Phi) =>
      e1Phi.addExpr (e2);
      RETURN e1;
    ELSE
      newPhi := NewPhi ();
      newPhi.addExpr (e1);
      newPhi.addExpr (e2);
      RETURN newPhi;
    END;
  END Merge;


(*               optimize:
                 do constant folding, other optimizations
 *)

PROCEDURE Optimize (e: Compound) : T RAISES {Problem} =
  VAR
    newE : T;
  BEGIN

    (* minor optimizations to canonicalize *)
    IF e.op = Opcode.cmovne THEN
      TYPECASE e.left OF
      | Compound (leftc) =>
        TYPECASE e.right OF
        | Immed (righti) =>
          IF righti.value = 0 AND leftc.op = Opcode.cmpeq THEN
            e := Copy (leftc);
            e.op := Opcode.cmpne;
            e.def := leftc.def;
          END;
        ELSE
        END;
      ELSE
      END;
    END;

    IF e.op = Opcode.xor THEN
      TYPECASE e.left OF
      | Compound (leftc) =>
        TYPECASE e.right OF
        | Immed (righti) =>
          IF righti.value = 1 AND leftc.op = Opcode.cmpeq THEN
            e := Copy (leftc);
            e.op := Opcode.cmpne;
            e.def := leftc.def;
          END;
        ELSE
        END;
      ELSE
      END;
    END;

    newE := Fold (e);
    RETURN newE;
  END Optimize;


PROCEDURE Fold (e: Compound) : T RAISES {Problem} =
  VAR
    left, right: INTEGER;
    newI: Immed;
  BEGIN
    IF TYPECODE (e.left) = TYPECODE (Immed) THEN
      left := NARROW (e.left, Immed).value;
    ELSE
      RETURN e;
    END;
    IF TYPECODE (e.right) = TYPECODE (Immed) THEN
      right := NARROW (e.right, Immed).value;
    ELSE
      RETURN e;
    END;

    newI := NEW (Immed);

    CASE e.op OF
    | Opcode.addq => newI.value := left + right;
    | Opcode.subq => newI.value := left - right;
    | Opcode.mulq => newI.value := left * right;
    | Opcode.adduq => newI.value := Word.Plus (left, right);
    | Opcode.subuq => newI.value := Word.Minus (left, right);
    | Opcode.muluq => newI.value := Word.Times (left, right);
    | Opcode.cmpule => newI.value := ORD (Word.LE (left, right));
    | Opcode.cmpult => newI.value := ORD (Word.LT (left, right));
    | Opcode.and => newI.value := Word.And (left, right);
    | Opcode.andnot => newI.value := Word.And (left, Word.Not (right));
    | Opcode.or => newI.value := Word.Or (left, right);
    | Opcode.ornot => newI.value := Word.Or (left, Word.Not (right));
    | Opcode.xor => newI.value := Word.Xor (left, right);
    | Opcode.xornot => newI.value := Word.Xor (left, Word.Not (right));
    | Opcode.sll => newI.value := Word.Shift (left, right);
    | Opcode.sra =>
      newI.value := Word.Shift (left, -right);
      IF left < 0 THEN
        newI.value := Word.Insert (newI.value, -1, 64-right, right);
      END;
    | Opcode.srl => newI.value := Word.Shift (left, -right);
    | Opcode.cmpbge =>
      newI.value := 0;
      FOR i := 0 TO 7 DO
        IF VIEW (left, ARRAY [0..7] OF [0..255])[i] <
          VIEW (right, ARRAY [0..7] OF [0..255])[i]
         THEN
          newI.value := Word.Or (newI.value, Word.Shift (1, i));
        END;
      END;
    | Opcode.extbl, Opcode.extwl, Opcode.extll, Opcode.extql, Opcode.extwh, Opcode.extlh, Opcode.extqh, Opcode.insbl, Opcode.inswl, Opcode.insll, Opcode.insql, Opcode.inswh, Opcode.inslh, Opcode.insqh, Opcode.mskbl, Opcode.mskwl, Opcode.mskll, Opcode.mskql, Opcode.mskwh, Opcode.msklh, Opcode.mskqh, Opcode.zap, Opcode.zapnot =>
      RAISE Problem;
    | Opcode.ldq,
      Opcode.ldq_u,
      Opcode.ldl,
      Opcode.ldh,
      Opcode.ldb =>
      RETURN e;
    | Opcode.cmpeq => newI.value := ORD (left = right);
    | Opcode.cmple => newI.value := ORD (left <= right);
    | Opcode.cmplt => newI.value := ORD (left < right);
    | Opcode.cmpne => newI.value := ORD (left # right);
    | Opcode.cmpge => newI.value := ORD (left >= right);
    | Opcode.cmpgt => newI.value := ORD (left > right);
    | Opcode.odd => newI.value := left MOD 2;
    | Opcode.even => newI.value := (left+1) MOD 2;
    | Opcode.logand .. Opcode.logor =>
    ELSE
      RAISE Problem;
    END;

    RETURN newI;
  END Fold;


(* change (COMPARE (- x C1) 0) -> (COMPARE x C1) *)
PROCEDURE Canonicalize (e: Compound) : T =
  VAR 
    left : Compound;
    c1, c2 : INTEGER;
    newC : Compound;
    newI : Immed;
  BEGIN
    IF TYPECODE (e.left) = TYPECODE (Compound) THEN
      left := NARROW (e.left, Compound);
    ELSE
      RETURN e;
    END;

    IF left.op # Opcode.subq THEN RETURN e; END;
    IF TYPECODE (left.right) = TYPECODE (Immed) THEN
      c1 := NARROW (left.right, Immed).value;
    ELSE
      RETURN e;
    END;

    IF TYPECODE (e.right) = TYPECODE (Immed) THEN
      c2 := NARROW (e.right, Immed).value;
    ELSE
      RETURN e;
    END;

    (* only get rid of 0 on the rhs *)
    IF c2 # 0 THEN RETURN e; END;

    CASE e.op OF
    | Opcode.cmpeq,
      Opcode.cmple,
      Opcode.cmplt,
      Opcode.cmpne,
      Opcode.cmpge,
      Opcode.cmpgt =>

      newC := NEW (Compound);
      newC.op := e.op;
      newC.left := left.left;
      newC.def := e.def;

      newI := NEW (Immed);
      newI.value := c1 + c2;
      newI.def := left.def;

      newC.right := newI;
    ELSE
      RETURN e;
    END;

    RETURN newC;
  END Canonicalize; 


  (* ToText routines *)


PROCEDURE ToText (e: T) : TEXT RAISES {Problem} =
  BEGIN
    IF e = NIL THEN RETURN ""; END;
    RETURN e.toText ();
  END ToText;


PROCEDURE ImmedToText (e: Immed) : TEXT =
  BEGIN
    RETURN "val: " & Fmt.Int (e.value) & " [" & Fmt.Int (e.def) & "]";
  END ImmedToText;

PROCEDURE RegToText (e: Reg) : TEXT =
  BEGIN
    RETURN "reg" & Fmt.Int (e.reg);
  END RegToText;

PROCEDURE OpNames (o: Opcode) : TEXT RAISES {NoOp} =
  BEGIN
    CASE o OF
    | Opcode.cmpne => RETURN "cmpne";
    | Opcode.cmpge => RETURN "cmpge";
    | Opcode.cmpgt => RETURN "cmpgt";
    | Opcode.odd => RETURN "odd";
    | Opcode.even => RETURN "even";
    | Opcode.logand => RETURN "logical-and";
    | Opcode.logor => RETURN "logical-or";
    ELSE
      RETURN Ir.InstructionNames [UnOp (o)];
    END;
  END OpNames;

PROCEDURE CompoundToText (e: Compound) : TEXT RAISES {Problem} =
  VAR
    tmp : TEXT;
  BEGIN
    IF Sanity AND (e.left = e OR e.right = e) THEN RAISE Problem; END;

    TRY
      tmp :=
          "(" & OpNames (e.op) & " [" & Fmt.Int (e.def) & "]\n" & ToText (e.left) & "\n" & ToText (e.right) & ")";
    EXCEPT
      NoOp => RAISE Problem;
    END;

    RETURN tmp;
  END CompoundToText;

PROCEDURE TopToText (<* UNUSED *> e: Top) : TEXT =
  BEGIN
    RETURN "Top";
  END TopToText;

PROCEDURE BottomToText (<* UNUSED *> e: Bottom) : TEXT =
  BEGIN
    RETURN "Bottom";
  END BottomToText;

PROCEDURE ArgToText (e: Arg) : TEXT =
  BEGIN
    RETURN "Arg " & Fmt.Int (e.num);
  END ArgToText;

PROCEDURE SPToText (<* UNUSED *> e: SP) : TEXT =
  BEGIN
    RETURN "SP";
  END SPToText;

PROCEDURE CallToText (e: Call) : TEXT RAISES {Problem} =
  VAR t, next : TEXT;
  BEGIN
    t := "(Call " & Fmt.Int(e.target) & " ";
    FOR i := 0 TO 5 DO
      next := ToText (e.args[i]);
      IF NOT Text.Equal (next, "") THEN
        t := t & next;
        IF i # 5 THEN t := t & ","; END;
      END;
    END;
    t := t & "[" & Fmt.Int (e.def) & "])";
    RETURN t;
  END CallToText;

PROCEDURE ReturnToText (<* UNUSED *> e: Return) : TEXT =
  BEGIN
    RETURN "Return address";
  END ReturnToText;

PROCEDURE WildToText (w: Wildcard) : TEXT =
  VAR t: TEXT;
  BEGIN
    t := "(WILD ";
    FOR i := 0 TO w.next-1 DO
      IF w.values[i].mark THEN
        t := t & "(NOT " & Fmt.Int (w.values[i].immed.value) & " " & Fmt.Int (w.values[i].offset) & ") ";
      ELSE
        t := t & "(" & Fmt.Int (w.values[i].immed.value) & " " & Fmt.Int (w.values[i].offset) & ") ";
      END;
    END;
    t := t & ")";
    RETURN t;
  END WildToText;

PROCEDURE PhiToText (p: Phi) : TEXT RAISES {Problem} =
  VAR t: TEXT;
  BEGIN
    t := "(PHI\n";
    FOR i := 0 TO p.next-1 DO
      t := t & "P" & Fmt.Int (i) & " " & ToText (p.exprs[i]) & "\n";
    END;
    t := t & ")";
    RETURN t;
  END PhiToText;


(* evaluation functions *)

PROCEDURE Eval (t: T) : T RAISES {Problem} =
  BEGIN
    IF t = NIL THEN
      RETURN NIL;
    ELSE
      RETURN t.eval ();
    END;
  END Eval;


PROCEDURE CompoundEval (e: Compound) : T RAISES {Problem} =
  VAR
    eleft, eright: T;
    left, right: INTEGER;
    newI: Immed;
  BEGIN
    eleft := Eval (e.left);
    eright := Eval (e.right);

    IF TYPECODE (eleft) = TYPECODE (Immed) THEN
      left := NARROW (eleft, Immed).value;
    ELSE
      RETURN e;
    END;
    IF TYPECODE (eright) = TYPECODE (Immed) THEN
      right := NARROW (eright, Immed).value;
    ELSE
      RETURN e;
    END;

    CASE e.op OF
    | Opcode.ldq =>
      newI := NEW (Immed);
      newI.value := LOOPHOLE (left+right, UNTRACED REF Word.T)^;
      RETURN newI;
    | Opcode.ldq_u =>
      (* The unaligned load instruction zeroes out the low order 3-bits
         of its address. *)
      newI := NEW (Immed);
      newI.value := LOOPHOLE (Word.And(left+right, Word.Not(7)), 
                              UNTRACED REF Word.T)^;
      RETURN newI;
    | Opcode.ldl =>
      newI := NEW (Immed);
      newI.value := LOOPHOLE (left+right, UNTRACED REF Word.T)^ MOD
      Word.Shift (1, 32);
      RETURN newI;
    | Opcode.ldh =>
      newI := NEW (Immed);
      newI.value := LOOPHOLE (left+right, UNTRACED REF Word.T)^ MOD
      Word.Shift (1, 16);
      RETURN newI;
    | Opcode.ldb =>
      newI := NEW (Immed);
      newI.value := LOOPHOLE (left+right, UNTRACED REF Word.T)^ MOD
      Word.Shift (1, 8);
      RETURN newI;
    ELSE
      (* all of this stuff is already handled above *)
      RETURN Fold (e);
    END;

  END CompoundEval;

  
  (* top-level methods on T *)

PROCEDURE Self (e: T) : T =
  BEGIN
    RETURN e;
  END Self;


  (* convert between Ir.Opcode and Opcode *)

PROCEDURE Op (o: Ir.Opcode) : Opcode RAISES {Problem} =
  BEGIN
    CASE o OF
    | Ir.Opcode.lda =>
      RETURN Opcode.addq;
    | Ir.Opcode.addq .. Ir.Opcode.zapnot =>
      RETURN VAL (ORD(o)-ORD(Ir.Opcode.addq)+ORD(Opcode.addq), Opcode);
    | Ir.Opcode.ldq .. Ir.Opcode.ldb =>
      RETURN VAL (ORD(o)-ORD(Ir.Opcode.ldq)+ORD(Opcode.ldq), Opcode);
    | Ir.Opcode.addqi .. Ir.Opcode.zapnoti =>
      RETURN VAL (ORD(o)-ORD(Ir.Opcode.addqi)+ORD(Opcode.addq), Opcode);
    ELSE
      RAISE Problem;
    END;
  END Op;

PROCEDURE UnOp (o: Opcode) : Ir.Opcode RAISES { NoOp } =
  BEGIN
    CASE o OF
    | Opcode.addq .. Opcode.zapnot =>
      RETURN VAL (ORD(o)-ORD(Opcode.addq)+ORD(Ir.Opcode.addq), Ir.Opcode);
    | Opcode.ldq .. Opcode.ldb =>
      RETURN VAL (ORD(o)-ORD(Opcode.ldq)+ORD(Ir.Opcode.ldq), Ir.Opcode);
    ELSE
      RAISE NoOp;
    END;
  END UnOp;

PROCEDURE Print (t: T; s: Wr.T := NIL) RAISES {Problem} =
  BEGIN
    IF s = NIL THEN s := Stdio.stdout; END;
    
    IF t = NIL THEN
      Wr.PutText (s, "NIL\n");
    ELSE
      Wr.PutText (s, t.toText () & "\n");
    END;
  END Print;


PROCEDURE LogicalOr (e1, e2: T) : T =
  BEGIN
    IF e1 = NIL THEN
      RETURN e2;
    ELSIF e2 = NIL THEN
      RETURN e1;
    ELSE
      RETURN NEW (Compound, op := Opcode.logor, left := e1, right := e2);
    END;
  END LogicalOr;


PROCEDURE LogicalAnd (e1, e2: T) : T =
  BEGIN
    IF e1 = NIL THEN
      RETURN e2;
    ELSIF e2 = NIL THEN
      RETURN e1;
    ELSE
      RETURN NEW (Compound, op := Opcode.logand, left := e1, right := e2);
    END;
  END LogicalAnd;

(* copy methods *)

PROCEDURE Copy (t: T) : T =
  BEGIN
    IF t = NIL THEN
      RETURN NIL;
    ELSE
      RETURN t.copy ();
    END;
  END Copy;

PROCEDURE CopyImmed (t: Immed) : T =
  VAR
    newT := NEW (Immed);
  BEGIN
    newT.value := t.value;
    RETURN newT;
  END CopyImmed;

PROCEDURE CopyArg (t: Arg) : T =
  VAR
    newT := NEW (Arg);
  BEGIN
    newT.num := t.num;
    RETURN newT;
  END CopyArg;

PROCEDURE CopyReg (t: Reg) : T =
  VAR
    newT := NEW (Reg);
  BEGIN
    newT.reg := t.reg;
    RETURN newT;
  END CopyReg;

PROCEDURE CopyCompound (t: Compound) : T =
  VAR
    newT := NEW (Compound);
  BEGIN
    newT.op := t.op;
    newT.left := Copy (t.left);
    newT.right := Copy (t.right);
    RETURN newT;
  END CopyCompound;

PROCEDURE CopyCall (t: Call) : T =
  VAR
    newT := NEW (Call);
  BEGIN
    newT.target := t.target;
    newT.last := t.last;
    FOR i := 0 TO 5 DO
      newT.args[i] := Copy (t.args[i]);
    END;
    RETURN newT;
  END CopyCall;

PROCEDURE CopyWildcard (t: Wildcard) : T =
  VAR
    newT := NEW (Wildcard);
  BEGIN
    newT.values := NEW (ImmedArray, NUMBER (t.values^));
    newT.values^ := t.values^;
    newT.next := t.next;
    RETURN newT;
  END CopyWildcard;

PROCEDURE CopyPhi (t: Phi) : T =
  VAR
    newT := NEW (Phi);
  BEGIN
    newT.exprs := NEW (TArray, NUMBER (t.exprs^));
    FOR i := 0 TO LAST (t.exprs^) DO
      newT.exprs[i] := Copy (t.exprs[i]);
    END;
    newT.next := t.next;
    RETURN newT;
  END CopyPhi;
    
    

BEGIN
  Zero := NEW (Immed, value := 0);
  TopValue := NEW (Top);
  BottomValue := NEW (Bottom);
END Expression.


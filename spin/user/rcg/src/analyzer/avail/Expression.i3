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

INTERFACE Expression;

IMPORT Wr;
IMPORT Ir;

(* raised when invariants are not held *)
EXCEPTION Problem;

(* had better match Ir.i3 *)
TYPE
  Opcode = {
            addq, subq, mulq, adduq, subuq, muluq,
            cmpeq, cmple, cmplt, cmpule, cmpult,
            and, andnot, or, ornot, xor, xornot,
            sll, sra, srl,
            cmpbge,
            extbl, extwl, extll, extql,
            extwh, extlh, extqh,
            insbl, inswl, insll, insql,
            inswh, inslh, insqh,
            mskbl, mskwl, mskll, mskql,
            mskwh, msklh, mskqh,
            cmoveq, cmovlbc, cmovlbs, cmovge,
            cmovlt, cmovgt, cmovne, cmovle,
            s4addl, s4subl, s4addq, s4subq,
            s8addl, s8subl, s8addq, s8subq,
            zap, zapnot,
            (* load/store ops *)
            ldq, ldq_u, ldl, ldh, ldb,

            (* extra ops *)
            cmpne, cmpge, cmpgt,
            odd, even,

            (* logical ops *)
            logand, logor
           };

  Public = OBJECT
    edge := -1;
    def := -1;
  METHODS
    eval () : T RAISES {Problem};
    toText () : TEXT RAISES {Problem};
    optimize () : T RAISES {Problem};
    canonicalize () : T;
    copy () : T;
  END;

  T <: Public;

  Immed <: T OBJECT
    value : INTEGER;
  END;

  Arg <: T OBJECT
    num : CARDINAL;
  END;

  SP <: T;

  Reg <: T OBJECT
    reg : Ir.Register;
  END;

  Compound <: T OBJECT
    op   : Opcode;
    left : T;
    right: T;
  END;

  (* lattice endpoints *)
  Top <: T;
  Bottom <: T;

  Call <: T OBJECT
    target : INTEGER;
    last : CARDINAL := 5;
    args : ARRAY [0..5] OF T;
  END;

  Return <: T;

  (* multiple values from different guards *)
  WildcardPublic = T OBJECT METHODS
    addValue (x: Immed; offset: CARDINAL);
    addNegateValue (x: Immed; offset: CARDINAL);
  END;
  Wildcard <: WildcardPublic;

  (* merge point *)
  Phi <: T OBJECT METHODS
    addExpr (t: T);
  END;

VAR (* constant *)
  Zero : Immed;
  TopValue : Top;
  BottomValue: Bottom;

(* raised when there is no reverse translation *)
EXCEPTION NoOp;

(* convert between Opcode and Ir.Opcode *)
PROCEDURE Op (o: Ir.Opcode) : Opcode RAISES {Problem};
PROCEDURE UnOp (o: Opcode) : Ir.Opcode RAISES { NoOp };

EXCEPTION NoMatch;

(* combine two expressions that match except for
   immediate values, creating wildcards when necessary
   wildcard nodes must be only be in e1, and not in e2
   
   count is the number of joins that happened already,
   so that wildcards can be handled correctly
*)
PROCEDURE Join (e1, e2: T; count: INTEGER) : T RAISES {NoMatch,Problem};

(* remove any WILDCARD (x, x, x, ...) nodes *)
PROCEDURE Collapse (e: T) : CARDINAL RAISES {Problem};

(* predicate to compare two expressions to see if they are the same *)
PROCEDURE Match (e1, e2: T) : T RAISES {Problem};

(* create/merge Phi nodes
   Phi nodes must be on left, and not on right
*)
PROCEDURE Merge (e1, e2: T) : T RAISES {Problem};


(* creators *)
PROCEDURE NewCall (target: INTEGER;
                   a0, a1, a2, a3, a4, a5: T;
                   number: CARDINAL := 6) : Call;
PROCEDURE NewWild () : Wildcard;
PROCEDURE NewPhi () : Phi;

PROCEDURE Eval (t: T) : T RAISES {Problem};

PROCEDURE ToText (e: T) : TEXT RAISES {Problem};
PROCEDURE Print (t: T; s: Wr.T := NIL) RAISES {Problem};

PROCEDURE LogicalOr (e1, e2: T) : T;
PROCEDURE LogicalAnd (e1, e2: T) : T;

PROCEDURE Copy (t: T) : T;

END Expression.

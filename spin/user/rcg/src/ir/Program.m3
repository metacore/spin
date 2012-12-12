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

UNSAFE MODULE Program;

IMPORT Procedure;
IMPORT IntIntTbl, Ir, Word;
IMPORT InstructionTbl, InstructionSeq;

PROCEDURE NewT () : T =
  VAR t : T := NEW (T);
  BEGIN
    t.procs := NEW (ProcArray, 2);
    t.GPtable := NEW (IntIntTbl.Default).init(50);
    t.map := NEW (InstructionTbl.Default).init ();
    t.remaining := NEW (InstructionSeq.T).init ();
    RETURN t;
  END NewT;

PROCEDURE AddProc (t: T; p: Procedure.T) : CARDINAL =
  VAR newprocs : ProcArray;
  BEGIN
    IF t.last > LAST (t.procs^) THEN
      newprocs := NEW (ProcArray, 2*t.last);
      SUBARRAY (newprocs^, 0, NUMBER (t.procs^)) := t.procs^;
      t.procs := newprocs;
    END;

    t.procs [t.last] := p;
    INC (t.last);
    RETURN t.last-1;
  END AddProc;

(* 
 * Given a displacement from a particular old gp table, return the
 * displacement which the same value will have in the new table.
 * Either we have seen the combination of gp and offset before in
 * which case it is in the table, or else it is new to us and we have
 * to look up the value. 
 *)

PROCEDURE GetNewGPValue(prog: T; key: Word.T): Ir.MemoryDisplacement =
  VAR
    index : INTEGER;
  BEGIN
    IF NOT prog.GPtable.get(key, index) THEN
      index := prog.GPsize;
      EVAL prog.GPtable.put(key, index);
      INC(prog.GPsize);
    END;

    RETURN 8 * index;
  END GetNewGPValue;

BEGIN
END Program.


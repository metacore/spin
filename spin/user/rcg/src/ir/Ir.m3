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

UNSAFE MODULE Ir;

IMPORT Wr, Stdio, Fmt, Word, Thread;
IMPORT JumpTable;

<* FATAL Wr.Failure, Thread.Alerted *>


(* instruction manipulation *)

PROCEDURE Delete (VAR i: Instruction) =
  BEGIN
    i.op := Opcode.deleted;
  END Delete;


PROCEDURE CopyArray (ia: InstructionArray) : InstructionArray =
  (* FIX THIS: should copy an instruction array recursively *)
  VAR
    na := NEW (InstructionArray, NUMBER (ia^));
  BEGIN
    na^ := ia^;
    RETURN na;
  END CopyArray;


PROCEDURE AddInstructionsAfter (ia: InstructionArray; i: INTEGER; VAR inst: ARRAY OF Instruction) =
  BEGIN
    AddInstructionsAfter2 (ia[i], inst);
  END AddInstructionsAfter;


PROCEDURE AddInstructionsBefore (ia: InstructionArray; i: INTEGER; VAR inst: ARRAY OF Instruction) =
  BEGIN
    AddInstructionsBefore2 (ia[i], inst);
  END AddInstructionsBefore;


PROCEDURE AddInstructionsAfter2 (VAR current: Instruction; VAR inst: ARRAY OF Instruction) =
  VAR
    instSize := NUMBER (inst);
    newIA := NEW (InstructionArray, 1+instSize);
  BEGIN
    (* FIX ME -- does not work if old instruction was an indirect *)
    (* move instruction to indirect block *)
    WITH currentIndirect = LOOPHOLE (current, IndirectInstruction) DO
      newIA [0] := current;
      newIA [0].index := -1;

      currentIndirect.op := Opcode.indirect;
      currentIndirect.unused := 0;
      currentIndirect.indirect := newIA;
    END;

    SUBARRAY (newIA^, 1, instSize) := inst;
  END AddInstructionsAfter2;


PROCEDURE AddInstructionsBefore2 (VAR current: Instruction; VAR inst: ARRAY OF Instruction) =
  VAR
    instSize := NUMBER (inst);
    newIA := NEW (InstructionArray, 1+instSize);
  BEGIN
    (* FIX ME -- does not work if old instruction was an indirect *)
    (* move instruction to indirect block *)
    WITH currentIndirect = LOOPHOLE (current, IndirectInstruction) DO
      newIA [instSize] := current;
      newIA [instSize].index := -1;

      currentIndirect.op := Opcode.indirect;
      currentIndirect.unused := 0;
      currentIndirect.indirect := newIA;
    END;

    (* copy inst *)
    SUBARRAY (newIA^, 0, instSize) := inst;
  END AddInstructionsBefore2;

(* register manipulation *)

PROCEDURE RegisterIsPhysical (r:Register) : BOOLEAN =
  BEGIN
    RETURN r < 32;
  END RegisterIsPhysical;

PROCEDURE RegisterPhysicalNumber (r:Register) : [0..31] =
  BEGIN
    <* ASSERT r < 32 *>
    RETURN r;
  END RegisterPhysicalNumber;

PROCEDURE RegisterIsVirtual (r:Register) : BOOLEAN =
  BEGIN
    RETURN r >= 32 AND r <= Word.Shift (1, 14);
  END RegisterIsVirtual;

(* print routines *)

PROCEDURE PrintInstruction(inst: Instruction; s: Wr.T := NIL) RAISES {Problem} =
  BEGIN
    IF s = NIL THEN s := Stdio.stderr; END;
    CASE inst.op OF
    | Opcode.addqi .. Opcode.zapnoti =>

      (* ALU ops with immediate fields *)
      WITH aluOp = LOOPHOLE(inst, AluRIInstruction) DO
        Wr.PutText(s, InstructionNames[inst.op] & "\tr" & Fmt.Int(aluOp.r1) &
                      " " & Fmt.Int(aluOp.immed) & " " &
                      RegisterText(aluOp.rdest));
      END;

    | Opcode.addq .. Opcode.zapnot => 

      (* arithmetic ops with three registers *)
      WITH aluOp = LOOPHOLE(inst, AluRRInstruction) DO
        Wr.PutText(s, InstructionNames[inst.op] & "\t" &
                      RegisterText(aluOp.r1) & " " &
                      RegisterText(aluOp.r2) & " " &
                      RegisterText(aluOp.rdest));
      END;

    | Opcode.li =>

      (* immed instruction *)
      WITH immedOp = LOOPHOLE(inst, ImmedInstruction) DO
        Wr.PutText(s, InstructionNames[inst.op] & "\t" &
                      RegisterText(immedOp.rdest) & " " &
                      Fmt.Int(immedOp.immed));
      END;

    | Opcode.lda .. Opcode.stb =>

      (* load/store instruction *)
      WITH memOp = LOOPHOLE(inst, MemoryInstruction) DO
        Wr.PutText(s, InstructionNames[inst.op] & "\t" &
                      RegisterText(memOp.r1) & " " &
                      Fmt.Int(memOp.disp) &
                      "(" & RegisterText(memOp.raddr) & ")");
      END;

    | Opcode.beq .. Opcode.br =>

      (* jump instruction uses r1 by jumping to it *)
      WITH branchOp = LOOPHOLE(inst, BranchInstruction) DO
        Wr.PutText(s, InstructionNames[inst.op] & "\t" &
                      RegisterText(branchOp.r1) & " BB #" &
                      Fmt.Int(branchOp.target));
      END;

    | Opcode.jsr .. Opcode.ret =>

      (* call instruction uses r2 by jumping to it *)
      WITH callOp = LOOPHOLE(inst, RetInstruction) DO
        Wr.PutText(s, InstructionNames[inst.op] & "\t" &
                      RegisterText(callOp.r1) &
                      " (" & RegisterText(callOp.r2) & ")");
      END;

    | Opcode.copy =>

      (* copy one register to another *)
      WITH copyOp = LOOPHOLE(inst, CopyInstruction) DO
        Wr.PutText(s, InstructionNames[inst.op] & "\t" &
                      RegisterText(copyOp.rsrc) &
                      " " & RegisterText(copyOp.rdest));
      END;

    | Opcode.noop =>
      Wr.PutText(s, "noop");

    | Opcode.call =>
     
      (* call *)
      WITH callOp = LOOPHOLE (inst, CallInstruction) DO
        Wr.PutText (s, InstructionNames[inst.op] & "\t" &
                       Fmt.Int (callOp.target));
      END;

    | Opcode.fault =>

      Wr.PutText (s, "Fault");

    | Opcode.indirect =>
    
      WITH ii = LOOPHOLE (inst, IndirectInstruction).indirect DO
        PrintArray (ii, s);
      END;

    | Opcode.jt =>
    
      WITH jt = LOOPHOLE (inst, JtInstruction).table DO
        JumpTable.Print (jt, s);
      END;

    | Opcode.deleted =>

      Wr.PutText (s, "Deleted");

    ELSE
      (* need code to handle inserted code *)
      RAISE Problem;
    END;
    
    IF inst.index # -1 THEN Wr.PutText (s, "\t" & Fmt.Int (inst.index)); END;
    Wr.PutText (s, "\n");
  END PrintInstruction;


PROCEDURE PrintArray (ia: InstructionArray; s: Wr.T := NIL) RAISES {Problem} =
  BEGIN
    FOR i := FIRST (ia^) TO LAST (ia^) DO
      PrintInstruction (ia[i], s);
    END;
  END PrintArray;


PROCEDURE RegisterText (r: Register) : TEXT =
  BEGIN
    IF r < 32 THEN
      RETURN "r" & Fmt.Int (r);
    ELSE
      RETURN "v" & Fmt.Int (r);
    END;
  END RegisterText;


PROCEDURE MakeImmediate (op: Opcode) : Opcode
  RAISES {UnknownInstruction,AlreadyImmediate} =
  CONST
    RegToImmed = ORD(Opcode.addqi) - ORD(Opcode.addq);
  BEGIN
    CASE op OF 
    | Opcode.unknown =>
      RAISE UnknownInstruction;
    | Opcode.addq .. Opcode.zapnot =>
      RETURN VAL(ORD(op) + RegToImmed, Opcode);
    | Opcode.addqi .. Opcode.zapnoti =>
      RAISE AlreadyImmediate;
    ELSE
      RETURN op;
    END;
  END MakeImmediate;


PROCEDURE UnmakeImmediate (op: Opcode) : Opcode
  RAISES {UnknownInstruction,NotImmediate} =
  CONST
    ImmedToReg = ORD(Opcode.addqi) - ORD(Opcode.addq);
  BEGIN
    CASE op OF 
    | Opcode.unknown =>
      RAISE UnknownInstruction;
    | Opcode.addqi .. Opcode.zapnoti =>
      RETURN VAL(ORD(op) - ImmedToReg, Opcode);
    | Opcode.addq .. Opcode.zapnot =>
      RAISE NotImmediate;
    ELSE
      RETURN op;
    END;
  END UnmakeImmediate;

PROCEDURE JoinArrays (ia1, ia2: InstructionArray) : InstructionArray =
  VAR
    result: InstructionArray;
  BEGIN
    result := NEW (InstructionArray, NUMBER (ia1^) + NUMBER (ia2^));

    SUBARRAY (result^, 0, NUMBER (ia1^)) := ia1^;
    SUBARRAY (result^, NUMBER (ia1^), NUMBER (ia2^)) := ia2^;

    RETURN result;
  END JoinArrays;

BEGIN
END Ir.

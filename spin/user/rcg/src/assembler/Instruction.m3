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
 * 27-Oct-96  Wilson Hsieh (whsieh) at the University of Washington
 *	fix GetLit so that it does not sign-extend.  It zero-extends!
 *
 * 17-Oct-96  Wilson Hsieh (whsieh) at the University of Washington
 *	procedures now take T, not INTEGER
 *
 *)


UNSAFE MODULE Instruction;

IMPORT Word, Fmt, Wr, Stdio, Thread;
IMPORT Register, Utils;

<* FATAL Thread.Alerted, Wr.Failure *>

(******************************************************************************
 *
 * instruction fields
 *
 *****************************************************************************)

CONST
  REG_A_MASK        = 16_03e00000;
  REG_A_SHIFT       = 21;

  REG_B_MASK        = 16_001f0000;
  REG_B_SHIFT       = 16;

  REG_C_MASK        = 16_0000001f;
  REG_C_SHIFT       = 0;

  Opcode_MASK       = 16_fc000000;
  Opcode_SHIFT      = 26;

  FUNCTION_MASK     = 16_00000fe0;
  FUNCTION_SHIFT    = 5;

  DISP_MASK         = 16_0000ffff;
  DISP_SHIFT        = 0;

  BR_DISP_MASK      = 16_001fffff;
  BR_DISP_SHIFT     = 0;

  LIT_MASK          = 16_001fe000;
  LIT_SHIFT         = 13;

  NUMBER_MASK       = 16_03ffffff;
  NUMBER_SHIFT      = 0;

  LIT_FLAG_MASK     = 16_00001000;
  LIT_FLAG_SHIFT    = 12;

  BRANCH_PRED_MASK  = 16_0000c000;
  BRANCH_PRED_SHIFT = 14; 

  TARGET_MASK       = 16_00003fff;
  TARGET_SHIFT      = 0;

PROCEDURE SetInstField(VAR dst: T;
                       val, mask, shift: INTEGER) =
  BEGIN
    dst := Word.Or(Word.And(dst, Word.Not(mask)),
                   Word.And(Word.LeftShift(val,shift),mask));
  END SetInstField;

PROCEDURE GetInstField(dst: T; mask, shift: INTEGER)
          : INTEGER =
  BEGIN
    RETURN Word.RightShift(Word.And(dst, mask), shift);
  END GetInstField;


PROCEDURE SetOpcode(VAR dst: T; code: INTEGER) =
  BEGIN
    SetInstField(dst, code, Opcode_MASK, Opcode_SHIFT);
  END SetOpcode;

PROCEDURE SetOp(VAR dst: T; op: Opcode) =
  BEGIN
    SetInstField(dst, DecodeTable[op].op_val, Opcode_MASK, Opcode_SHIFT);
  END SetOp;

PROCEDURE GetOpcode(inst_val: T): INTEGER =
  BEGIN
    RETURN GetInstField(inst_val, Opcode_MASK, Opcode_SHIFT);
  END GetOpcode;

PROCEDURE SetBranchPred(VAR dst: T; pred: INTEGER) =
  BEGIN
    SetInstField(dst, pred, BRANCH_PRED_MASK, BRANCH_PRED_SHIFT);
  END SetBranchPred;

PROCEDURE GetBranchPred(inst_val: T): INTEGER =
  BEGIN
    RETURN GetInstField(inst_val, BRANCH_PRED_MASK, BRANCH_PRED_SHIFT);
  END GetBranchPred;

PROCEDURE SetTarget(VAR dst: T; target: INTEGER) =
  BEGIN
    SetInstField(dst, target, TARGET_MASK, TARGET_SHIFT);
  END SetTarget;

PROCEDURE GetTarget(inst_val: T): INTEGER =
  BEGIN
    RETURN GetInstField(inst_val, TARGET_MASK, TARGET_SHIFT);
  END GetTarget;

PROCEDURE SetLitFlag(VAR dst: T; flag: BOOLEAN) =
  BEGIN
    SetInstField(dst, ORD(flag), LIT_FLAG_MASK, LIT_FLAG_SHIFT);
  END SetLitFlag;

PROCEDURE GetLitFlag(inst_val: T): BOOLEAN =
  BEGIN
    RETURN VAL (GetInstField(inst_val, LIT_FLAG_MASK, LIT_FLAG_SHIFT), BOOLEAN);
  END GetLitFlag;

PROCEDURE SetNumber(VAR dst: T; num: INTEGER) =
  BEGIN
    SetInstField(dst, num, NUMBER_MASK, NUMBER_SHIFT);
  END SetNumber;

PROCEDURE GetNumber(inst_val: T): INTEGER =
  BEGIN
    RETURN GetInstField(inst_val, NUMBER_MASK, NUMBER_SHIFT);
  END GetNumber;

PROCEDURE SetFunction(VAR dst: T; fc: INTEGER) =
  BEGIN
    SetInstField(dst, fc, FUNCTION_MASK, FUNCTION_SHIFT);
  END SetFunction;

PROCEDURE GetFunction(inst_val: T): INTEGER =
  BEGIN
    RETURN GetInstField(inst_val, FUNCTION_MASK, FUNCTION_SHIFT);
  END GetFunction;

PROCEDURE SetRegA(VAR dst: T; reg: INTEGER) =
  BEGIN
    SetInstField(dst, reg, REG_A_MASK, REG_A_SHIFT);
  END SetRegA;

PROCEDURE GetRegA(inst_val: T): INTEGER =
  BEGIN
    RETURN GetInstField(inst_val, REG_A_MASK, REG_A_SHIFT);
  END GetRegA;

PROCEDURE SetRegB(VAR dst: T; reg: INTEGER) =
  BEGIN
    SetInstField(dst, reg, REG_B_MASK, REG_B_SHIFT);
  END SetRegB;

PROCEDURE GetRegB(inst_val: T): INTEGER =
  BEGIN
    RETURN GetInstField(inst_val, REG_B_MASK, REG_B_SHIFT);
  END GetRegB;

PROCEDURE SetRegC(VAR dst: T; reg: INTEGER) =
  BEGIN
    SetInstField(dst, reg, REG_C_MASK, REG_C_SHIFT);
  END SetRegC;

PROCEDURE GetRegC(inst_val: T): INTEGER =
  BEGIN
    RETURN GetInstField(inst_val, REG_C_MASK, REG_C_SHIFT);
  END GetRegC;

PROCEDURE SetDisp(VAR dst: T; disp: INTEGER) =
  BEGIN
    SetInstField(dst, disp, DISP_MASK, DISP_SHIFT);
  END SetDisp;

PROCEDURE GetDisp(inst_val: T): INTEGER =
  VAR
    val: INTEGER;
  BEGIN
    val := GetInstField(inst_val, DISP_MASK, DISP_SHIFT);

    IF Word.And (Word.Shift (1, 15), val) # 0 THEN
      val := Word.Insert (val, -1, 16, 48);
    END;
    RETURN val;
  END GetDisp;

PROCEDURE SetBranchDisp(VAR dst: T; disp: INTEGER) =
  BEGIN
    SetInstField(dst, disp, BR_DISP_MASK, BR_DISP_SHIFT);
  END SetBranchDisp;

PROCEDURE GetBranchDisp(inst_val: T): INTEGER =
  VAR disp : INTEGER;
  BEGIN
    disp := GetInstField (inst_val, BR_DISP_MASK, BR_DISP_SHIFT);
    IF Word.And (disp, Word.Shift (1, 20)) # 0 THEN
      disp := Word.Insert (disp, -1, 21, 11+32);
    END;
    RETURN disp;
  END GetBranchDisp;

PROCEDURE SetLit (VAR dst: T; lit: CARDINAL) =
  BEGIN
    SetInstField (dst, lit, LIT_MASK, LIT_SHIFT);
  END SetLit;

PROCEDURE GetLit (inst_val: T): INTEGER =
  VAR lit : INTEGER;
  BEGIN
    lit := GetInstField (inst_val, LIT_MASK, LIT_SHIFT);
    RETURN lit;
  END GetLit;

PROCEDURE FindByOpcode(opVal: INTEGER): Opcode =
  BEGIN
    FOR i := FIRST(DecodeTable) TO LAST(DecodeTable) DO
      IF DecodeTable[i].op_val = opVal THEN
        RETURN i;
      END;
    END;
    RETURN Opcode.unknown;
  END FindByOpcode;

PROCEDURE FindByFunction(idx: Opcode; op_val, fc_val: INTEGER): Opcode =
  BEGIN
    FOR i := idx TO LAST(DecodeTable) DO
      IF DecodeTable[i].op_val # op_val THEN
        EXIT;
      END;
      IF DecodeTable[i].fc_val = fc_val THEN
        RETURN i;
      END;
    END;
    RETURN Opcode.unknown;
  END FindByFunction;


PROCEDURE GetOp (inst_val: T) : Opcode =
  VAR
    op_val : CARDINAL;
    fc_val : CARDINAL;
    idx    : Opcode;
    fmt    : Format;
  BEGIN
    op_val := GetOpcode(inst_val);
    idx    := FindByOpcode(op_val);

    IF idx # Opcode.unknown THEN
      WITH entry = DecodeTable[idx] DO
        fmt := entry.format;
        
        CASE fmt OF
        | Format.PalCode =>
          RETURN entry.Opcode;
        | Format.Operate =>
          fc_val := GetFunction(inst_val);
          idx    := FindByFunction(idx, op_val, fc_val);
          IF idx # Opcode.unknown THEN
            RETURN DecodeTable[idx].Opcode;
          ELSE
            Utils.Error ("Unknown function code " & Fmt.Unsigned (fc_val) &
              " for Opcode " & entry.name & " in Instruction.GetOp");
            RETURN Opcode.unknown;
          END;
        | Format.Memory =>
          RETURN entry.Opcode;
        | Format.MemoryJMP =>
          fc_val := GetBranchPred(inst_val);
          idx    := FindByFunction(idx, op_val, fc_val);
          IF idx # Opcode.unknown THEN
            RETURN DecodeTable[idx].Opcode;
          ELSE
            Utils.Error ("Unknown function code " & Fmt.Unsigned (fc_val) &
              " for Opcode " & entry.name & " in Instruction.GetOp");
            RETURN Opcode.unknown;
          END;
        | Format.Branch =>
          RETURN entry.Opcode;
        | Format.MemoryFC =>
          fc_val := GetDisp(inst_val);
          idx    := FindByFunction(idx, op_val, fc_val);
          IF idx # Opcode.unknown THEN
            RETURN DecodeTable[idx].Opcode;
          ELSE
            Utils.Error ("Unknown function code " & Fmt.Unsigned (fc_val) &
              " for Opcode " & entry.name & " in Instruction.GetOp");
            RETURN Opcode.unknown;
          END;
        END;
      END;
    ELSE
      Utils.Error ("Unknown Opcode " & 
        Fmt.Unsigned(op_val) & " in Instruction.GetOp");
      RETURN Opcode.unknown;
    END;
  END GetOp;


(*
 * print a machine instruction
 *)

PROCEDURE Print (inst: T; s: Wr.T := NIL) =
  VAR
    op_val : CARDINAL;
    fc_val : CARDINAL;
    idx    : Opcode;
    fmt    : Format;
  BEGIN
    IF s = NIL THEN s := Stdio.stderr; END;

    op_val := GetOpcode(inst);
    idx    := FindByOpcode(op_val);
    IF idx # Opcode.unknown THEN
      WITH entry = DecodeTable[idx] DO
        fmt := entry.format;
        
        CASE fmt OF
        | Format.PalCode =>
          Wr.PutText (s, "Palcode instruction: 0x" &
                         Fmt.Unsigned (inst) & "\n");
        | Format.Operate =>
          fc_val := GetFunction(inst);
          idx    := FindByFunction(idx, op_val, fc_val);
          IF idx # Opcode.unknown THEN
            IF GetLitFlag(inst) THEN
              Wr.PutText(
                  s,
                  DecodeTable[idx].name & " " &
                  Register.Names[VAL(GetRegA(inst), Register.T)] & ", " &
                  Fmt.Int(GetLit(inst)) & ", " &
                  Register.Names[VAL(GetRegC(inst), Register.T)]);
            ELSE
              Wr.PutText(
                  s,
                  DecodeTable[idx].name & " " &
                  Register.Names[VAL(GetRegA(inst), Register.T)] & ", " &
                  Register.Names[VAL(GetRegB(inst), Register.T)] & ", " &
                  Register.Names[VAL(GetRegC(inst), Register.T)]);
            END;
          ELSE
            Wr.PutText (s, Fmt.Unsigned(inst));
            Utils.Error ("Unknown function code " & Fmt.Unsigned (fc_val) &
              " for Opcode " & entry.name & " in Instruction.Print");
          END;
        | Format.Memory =>
          Wr.PutText (
              s,
              entry.name & " " &
              Register.Names[VAL(GetRegA(inst), Register.T)] & ", " &
              Register.Names[VAL(GetRegB(inst), Register.T)] & "(" &
              Fmt.Int(GetDisp(inst)) & ")");
        | Format.MemoryJMP =>
          fc_val := GetBranchPred(inst);
          idx    := FindByFunction(idx, op_val, fc_val);
          IF idx # Opcode.unknown THEN
          Wr.PutText (
              s,
              DecodeTable[idx].name & " " &
              Register.Names[VAL(GetRegA(inst), Register.T)] & "(" &
              Register.Names[VAL(GetRegB(inst), Register.T)] & ")");
          ELSE
            Wr.PutText (s, Fmt.Unsigned(inst));
            Utils.Error ("Unknown function code " & Fmt.Unsigned (fc_val) &
              " for Opcode " & entry.name & " in Instruction.Print");
          END;
        | Format.Branch =>
          Wr.PutText(
              s,
              entry.name & " " & 
              Register.Names[VAL(GetRegA(inst), Register.T)] & ", " & 
              Fmt.Int(GetBranchDisp(inst)));
        | Format.MemoryFC =>
          fc_val := GetDisp(inst);
          idx    := FindByFunction(idx, op_val, fc_val);
          IF idx # Opcode.unknown THEN
            Wr.PutText (
                s,
                entry.name & " " &
                Register.Names[VAL(GetRegA(inst), Register.T)] & " " &
                Fmt.Int(GetDisp(inst)) & "(" &
                Register.Names[VAL(GetRegB(inst), Register.T)] & ")");
          ELSE
            Wr.PutText (s, Fmt.Unsigned(inst));
            Utils.Error ("Unknown function code " & Fmt.Unsigned (fc_val) &
              " for Opcode " & entry.name & " in Instruction.Print");
          END;
        END;
      END;
    ELSE
      Wr.PutText (s, Fmt.Unsigned(inst));
      Utils.Error ("Unknown Opcode " & 
        Fmt.Unsigned(op_val) & " in Instruction.Print");
    END;
  END Print;

PROCEDURE PrintUntilReturn(ptr: TPtr; name: TEXT) =
  CONST
    RET_MASK = 16_fc00c000;
    RET_CODE = 16_68008000;
  VAR
    i := 0;
  BEGIN
    Wr.PutText(Stdio.stderr, "procedure \"" & name & "\" disassembled at " &
      Fmt.Unsigned(LOOPHOLE(ptr, INTEGER)) & "\n");
    LOOP
      Wr.PutText(Stdio.stderr, "0x" & Fmt.Unsigned(LOOPHOLE(ptr,INTEGER)) & 
        " " & Fmt.Int(4 * i) & " ");
      Print(ptr^, Stdio.stderr);
      Wr.PutText(Stdio.stderr, "\n");
      INC(i);
      IF (Word.And(ptr^, RET_MASK) = RET_CODE) THEN
        EXIT;
      END;
      INC(ptr, BYTESIZE(T));
    END;
  END PrintUntilReturn; 

BEGIN
END Instruction.
 

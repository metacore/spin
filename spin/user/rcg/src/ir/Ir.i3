(*
 *
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *)

(*
 * HISTORY
 * 16-Oct-96  Wilson Hsieh (whsieh) at the University of Washington
 *	add blbs
 *
 *)

INTERFACE Ir;

(* This interface defines the instruction types for our
   intermediate representation.
*)

IMPORT Word, Wr;
IMPORT Residue;
IMPORT JumpTable;

CONST
  (* THESE SIZE ARE A BIT ARBITRARY *)
  Opbits = 8;
  Nonopbits = BITSIZE(Word.T) - Opbits;
  RegisterBits = 14; (* 15 bits = 32*1024 registers *)
  MemoryBits = 16;
  ImmediateBits = 16;

EXCEPTION

  UnknownInstruction;
  AlreadyImmediate;
  NotImmediate;

TYPE

  (****************  instruction types       ************)

  Opcode = {
  (* if this changes, potentially need to change:
       name table below
       table in assembler/Instruction.i3
       table in assembler/Assembler.m3,
       code in analyzer/avail/Expression.i3
  *)
            (* noop *)
            noop,
            (* alu ops with 3 registers *)
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
            lda,
            ldq, ldq_u, ldl, ldh, ldb,
            stq, stq_u, stl, sth, stb,
            (* alu ops with 2 registers, 1 immediate *)
            addqi, subqi, mulqi, adduqi, subuqi, muluqi,
            cmpeqi, cmplei, cmplti, cmpulei, cmpulti,
            andi, andnoti, ori, ornoti, xori, xornoti,
            slli, srai, srli,
            cmpbgei,
            extbli, extwli, extlli, extqli,
            extwhi, extlhi, extqhi,
            insbli, inswli, inslli, insqli,
            inswhi, inslhi, insqhi,
            mskbli, mskwli, msklli, mskqli,
            mskwhi, msklhi, mskqhi,
            cmoveqi, cmovlbci, cmovlbsi, cmovgei,
            cmovlti, cmovgti, cmovnei, cmovlei,
            s4addli, s4subli, s4addqi, s4subqi,
            s8addli, s8subli, s8addqi, s8subqi,
            zapi, zapnoti,

            (* load immediate *)
            li,
            (* branch ops *)
            beq, bge, bgt, ble, blt, bne, blbc, blbs, br,
            (* call ops *)
            jsr,            (* temporary placeholder for call instruction *)
            ret,
            (* copy instruction *)
            copy,
            (* IR call -- call to IR representation *)
            call, fault,
            (* pointer to additional instructions *)
            indirect,
            jt,
            deleted,
            (* reserved for future use *)
            unused0, unused1, unused2, unused3,
            (* unknown *)
            unknown
  };
    
CONST 
  InstructionNames = ARRAY Opcode OF TEXT {
            "noop",
            "addq", "subq", "mulq", "adduq", "subuq", "muluq",
            "cmpeq", "cmple", "cmplt", "cmpule", "cmpult",
            "and", "andnot", "or", "ornot", "xor", "xornot",
            "sll", "sra", "srl",
            "cmpbge",
            "extbl", "extwl", "extll", "extql",
            "extwh", "extlh", "extqh",
            "insbl", "inswl", "insll", "insql",
            "inswh", "inslh", "insqh",
            "mskbl", "mskwl", "mskll", "mskql",
            "mskwh", "msklh", "mskqh",
            "cmoveq", "cmovlbc", "cmovlbs", "cmovge",
            "cmovlt", "cmovgt", "cmovne", "cmovle",
            "s4addl", "s4subl", "s4addq", "s4subq", 
            "s8addl", "s8subl", "s8addq", "s8subq",
            "zap", "zapnot",
            "lda",
            "ldq", "ldq_u", "ldl", "ldh", "ldb",
            "stq", "stq_u", "stl", "sth", "stb",
            "addqi", "subqi", "mulqi", "adduqi", "subuqi", "muluqi",
            "cmpeqi", "cmplei", "cmplti", "cmpulei", "cmpulti",
            "andi", "andnoti", "ori", "ornoti", "xori", "xornoti",
            "slli", "srai", "srli",
            "cmpbgei",
            "extbli", "extwli", "extlli", "extqli",
            "extwhi", "extlhi", "extqhi",
            "insbli", "inswli", "inslli", "insqli",
            "inswhi", "inslhi", "insqhi",
            "mskbli", "mskwli", "msklli", "mskqli",
            "mskwhi", "msklhi", "mskqhi",
            "cmoveqi", "cmovlbci", "cmovlbsi", "cmovgei",
            "cmovlti", "cmovgti", "cmovnei", "cmovlei",
            "s4addli", "s4subli", "s4addqi", "s4subqi",
            "s8addli", "s8subli", "s8addqi", "s8subqi",
            "zapi", "zapnoti",
            "li",
            "beq", "bge", "bgt", "ble", "blt", "bne", "blbc", "blbs", "br",
            "jsr", "ret",
            "copy",
            "call", "fault",
            "indirect",
            "jumptable",
            "deleted",
            "unused0", "unused1", "unused2", "unused3",
            "unknown"};

TYPE
  (* These ranges correspond directly to the types of instructions
     we are currently using, which makes it possible to declare the
     opfield of an instruction to be one of these subtypes. *)

  AluReg =   [Opcode.addq .. Opcode.zapnot];      (*  42+8+8 *)
  AluImmed = [Opcode.addqi .. Opcode.zapnoti];    (*  42+8+8 *)
  Immed =    [Opcode.li .. Opcode.li];            (*   1 *)
  Memory =   [Opcode.lda .. Opcode.stb];          (*  10 *)
  Jumps =    [Opcode.beq .. Opcode.br];           (*   9 *)
  Ret  =     [Opcode.jsr .. Opcode.ret];          (*   2 *)
  Copy =     [Opcode.copy .. Opcode.copy];        (*   1 *)
  Call =     [Opcode.call .. Opcode.fault];       (*   2 *)
  Indirect = [Opcode.indirect .. Opcode.indirect];(*   1 *)
  Jumptable =[Opcode.jt .. Opcode.jt];            (*   1 *)
  Deleted =  [Opcode.deleted .. Opcode.deleted];  (*   1 *)
                                                  (* 110 *)
  Unused = [Opcode.unused0 .. Opcode.unknown];    (*   5 *)
                                                  (* 115 *)

  Opfield = BITS Opbits FOR Opcode;

  (* We assume register 0 is wired to 0
     assume registers 1-31 are hard registers 
  *)
  Register = BITS RegisterBits FOR [0 .. Word.Shift(1,RegisterBits)-1];

  MemoryDisplacement =
    BITS MemoryBits FOR [-Word.Shift(1, MemoryBits-1) .. 
                          Word.Shift(1, MemoryBits-1)-1];
  Immediate =
    BITS ImmediateBits FOR [-Word.Shift(1,ImmediateBits-1) .. 
                             Word.Shift(1,ImmediateBits-1)-1];
  
  (* load/store *)
  ImmedInstruction = RECORD
    op: BITS Opbits FOR Immed;
    rdest: Register;
    immed: BITS 40 FOR [-Word.Shift(1,39) .. Word.Shift(1,39)-1];
    index: INTEGER := -1;
    ignore: REFANY := NIL;
  END;
  
  (* load/store *)
  MemoryInstruction = RECORD
    op: BITS Opbits FOR Memory;
    r1, raddr: Register;
    disp: MemoryDisplacement;
    high: BITS 1 FOR BOOLEAN := FALSE;
    unused: BITS 10 FOR [0 .. 0] := 0;
    index: INTEGER := -1;
    ignore: REFANY := NIL;
  END;
    
  RetInstruction = RECORD
    op: BITS Opbits FOR Ret;
    r1, r2: Register;
    (* used to encode target *)
    unused: BITS 27 FOR [-Word.Shift (1,26) .. Word.Shift (1, 26)-1] := -1;
    index: INTEGER := -1;
    ignore: REFANY := NIL;
  END;
  
  (* branch *)
  BranchInstruction = RECORD
    op: BITS Opbits FOR Jumps;
    r1: Register; 
    target: BasicBlockIndex;
    unused: BITS 21 FOR [0 .. 0] := 0;
    index: INTEGER := -1;
    ignore: REFANY := NIL;
  END;

  (* alu *)
  (* Alpha uses minor Opfield info *)
  AluRRInstruction = RECORD
    op: BITS Opbits FOR AluReg;
    r1, r2, rdest: Register;
    unused: BITS 12 FOR [0 .. 0] := 0;
    index: INTEGER := -1;
    ignore: REFANY := NIL;
  END;
  AluRIInstruction = RECORD
    op: BITS Opbits FOR AluImmed;
    r1, rdest: Register; 
    immed: Immediate;
    unused: BITS 11 FOR [0 .. 0] := 0;
    index: INTEGER := -1;
    ignore: REFANY := NIL;
  END;

  CopyInstruction = RECORD
    op: BITS Opbits FOR Copy;
    rsrc, rdest: Register;
    unused: BITS 27 FOR [0 .. 0] := 0;
    index: INTEGER := -1;
    ignore: REFANY := NIL;
  END;

  CallInstruction = RECORD
    op: BITS Opbits FOR Call;
    target: BITS 20 FOR [0 .. Word.Shift(1,20)-1]; (* procedure index *)
    reg: Register;
    unused: BITS 12 FOR [0 .. 0] := 0;
    index: INTEGER := -1;
    ignore: REFANY := NIL;
  END;

  IndirectInstruction = RECORD
    op: BITS Opbits FOR Indirect;
    unused: BITS Nonopbits FOR [0..0];
    index: INTEGER := -1;
    indirect: InstructionArray;
  END;

  JtInstruction = RECORD
    op: BITS Opbits FOR Jumptable;
    unused: BITS Nonopbits FOR [0..0];
    index: INTEGER := -1;
    table: JumpTable.T;
  END;

  DeletedInstruction = RECORD
    op: BITS Opbits FOR Deleted;
    unused: BITS Nonopbits FOR [0..0];
    index: INTEGER := -1;
    ignore: REFANY := NIL;
  END;

  (* An Instruction *)
  Instruction = RECORD
    op: Opfield;
    other: BITS Nonopbits FOR [0 .. Word.Shift(1,Nonopbits)-1] := 0;
    index: INTEGER := -1;
    extra: REFANY := NIL;
  END;

  (* allow for lots of basic blocks - probably too many *)
  BasicBlockIndex =
    BITS 21 FOR [0 .. Word.Shift(1,21) - 1];

  InstructionArray = REF ARRAY OF Instruction;

CONST
  Noop = Instruction {op := Opcode.noop};

(****************  data flow types       ************)

TYPE
  DataflowInfo = RECORD
    live: Residue.T;
    constant: Residue.T;
    avail: Residue.T;
    reaching: Residue.T;
  END;
  Code = REFANY;

(* registers *)
CONST
  ReturnAddressRegister = 26;
  PVRegister = 27;
  ZeroRegister = 31;

TYPE
  RegisterSet = SET OF [0..31];

(*
  missing:
    raising exceptions
    handling exceptions
 *)

(* various procedures *)


(* operate on instruction arrays *)
PROCEDURE Delete (VAR i: Instruction);
PROCEDURE CopyArray (ia: InstructionArray) : InstructionArray;

PROCEDURE AddInstructionsAfter (ia: InstructionArray; i: INTEGER; VAR inst: ARRAY OF Instruction);
PROCEDURE AddInstructionsBefore (ia: InstructionArray; i: INTEGER; VAR inst: ARRAY OF Instruction);

PROCEDURE AddInstructionsAfter2 (VAR i: Instruction; VAR inst: ARRAY OF Instruction);
PROCEDURE AddInstructionsBefore2 (VAR i: Instruction; VAR inst: ARRAY OF Instruction);

TYPE PhysicalRegister = [0..31];

PROCEDURE RegisterIsPhysical (r:Register) : BOOLEAN;
PROCEDURE RegisterPhysicalNumber (r:Register) : PhysicalRegister;
PROCEDURE RegisterIsVirtual (r:Register) : BOOLEAN;

EXCEPTION Problem;

PROCEDURE PrintInstruction(inst: Instruction; s: Wr.T := NIL) RAISES {Problem};
PROCEDURE PrintArray(ia: InstructionArray; s: Wr.T := NIL) RAISES {Problem};

(* convert between immediate/unimmediate versions of opcodes *)
PROCEDURE MakeImmediate(op: Opcode) : Opcode
  RAISES {UnknownInstruction, AlreadyImmediate};

PROCEDURE UnmakeImmediate(op: Opcode) : Opcode
  RAISES {UnknownInstruction, NotImmediate};

PROCEDURE JoinArrays (ia1, ia2: InstructionArray) : InstructionArray;

END Ir.

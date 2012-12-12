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
 *	procedures now take T, not INTEGER
 *	add blbs
 *
 *)


INTERFACE Instruction;

IMPORT Ir, Wr;

(******************************************************************************
 *
 * instruction decoding
 *
 *****************************************************************************)

TYPE
  Opcode = 
    { 
      pal,
      lda,
      ldg,
      ldq,
      lds,
      stg,
      stq,
      sts,
      ldah,
      ldl,
      ldq_l,
      ldt,
      stl,
      stq_c,
      stt,
      ldf,
      ldl_l,
      ldq_u,
      stf,
      stl_c,
      stq_u,
      fetch,
      rc,
      trapb,
      fetch_m,
      rpcc,
      mb,
      rs,
      jmp,
      ret,
      jsr,
      jsr_co,
      br,
      fble,
      fbge,
      beq,
      blbs,
      bgt,
      fbeq,
      bsr,
      fbgt,
      blt,
      bne,
      fblt,
      fbne,
      blbc,
      ble,
      bge,
      addl,
      addqv,
      cmple,
      cmpult,
      subq,
      addlv,
      cmpbge,
      cmplt,
      subl,
      subqv,
      addq,
      cmpeq,
      cmpule,
      sublv,
      s4addl,
      s4subq,
      s8subl,
      s4addq,
      s8addl,
      s8subq,
      s4subl,
      s8addq,
      and,
      cmoveq,
      bic,
      cmovlbc,
      bis,
      cmovlbs,
      cmovge,
      cmovlt,
      ornot,
      cmovgt,
      cmovne,
      xor,
      cmovle,
      eqv,
      extbl,
      extqh,
      extwl,
      insll,
      inswh,
      msklh,
      mskql,
      sll,
      zap,
      extlh,
      extql,
      insbl,
      insqh,
      inswl,
      mskll,
      mskwh,
      sra,
      zapnot,
      extll,
      extwh,
      inslh,
      insql,
      mskbl,
      mskqh,
      mskwl,
      srl,
      mull,
      mulqv,
      mullv,
      umulh,
      mulq,
      unknown
   };

  Format = {
    Memory,
    MemoryFC,
    MemoryJMP,
    Branch,
    Operate,
    PalCode
  };

(******************************************************************************
 *
 * instructions descriptors and names
 *
 *****************************************************************************)

CONST
  DecodeTable = ARRAY [Opcode.pal .. Opcode.mulq] OF Desc {
    Desc { 16_00,    16_0, Opcode.pal,     "pal",     Format.PalCode,  Ir.Opcode.unknown },
    Desc { 16_08, 16_ffff, Opcode.lda,     "lda",     Format.Memory,   Ir.Opcode.lda     },
    Desc { 16_21, 16_ffff, Opcode.ldg,     "ldg",     Format.Memory,   Ir.Opcode.unknown },
    Desc { 16_29, 16_ffff, Opcode.ldq,     "ldq",     Format.Memory,   Ir.Opcode.ldq     },
    Desc { 16_22, 16_ffff, Opcode.lds,     "lds",     Format.Memory,   Ir.Opcode.unknown },
    Desc { 16_25, 16_ffff, Opcode.stg,     "stg",     Format.Memory,   Ir.Opcode.unknown },
    Desc { 16_2d, 16_ffff, Opcode.stq,     "stq",     Format.Memory,   Ir.Opcode.stq     },
    Desc { 16_26, 16_ffff, Opcode.sts,     "sts",     Format.Memory,   Ir.Opcode.unknown },
    Desc { 16_09, 16_ffff, Opcode.ldah,    "ldah",    Format.Memory,   Ir.Opcode.lda     },
    Desc { 16_28, 16_ffff, Opcode.ldl,     "ldl",     Format.Memory,   Ir.Opcode.ldl     },
    Desc { 16_2b, 16_ffff, Opcode.ldq_l,   "ldq_l",   Format.Memory,   Ir.Opcode.unknown },
    Desc { 16_23, 16_ffff, Opcode.ldt,     "ldt",     Format.Memory,   Ir.Opcode.unknown },
    Desc { 16_2c, 16_ffff, Opcode.stl,     "stl",     Format.Memory,   Ir.Opcode.stl     },
    Desc { 16_2f, 16_ffff, Opcode.stq_c,   "stq_c",   Format.Memory,   Ir.Opcode.unknown },
    Desc { 16_27, 16_ffff, Opcode.stt,     "stt",     Format.Memory,   Ir.Opcode.unknown },
    Desc { 16_20, 16_ffff, Opcode.ldf,     "ldf",     Format.Memory,   Ir.Opcode.unknown },
    Desc { 16_2a, 16_ffff, Opcode.ldl_l,   "ldl_l",   Format.Memory,   Ir.Opcode.unknown },
    Desc { 16_0b, 16_ffff, Opcode.ldq_u,   "ldq_u",   Format.Memory,   Ir.Opcode.ldq_u   },
    Desc { 16_24, 16_ffff, Opcode.stf,     "stf",     Format.Memory,   Ir.Opcode.unknown },
    Desc { 16_2e, 16_ffff, Opcode.stl_c,   "stl_c",   Format.Memory,   Ir.Opcode.unknown },
    Desc { 16_0f, 16_ffff, Opcode.stq_u,   "stq_u",   Format.Memory,   Ir.Opcode.stq     },
    Desc { 16_18, 16_8000, Opcode.fetch,   "fetch",   Format.MemoryFC, Ir.Opcode.unknown },
    Desc { 16_18, 16_e000, Opcode.rc,      "rc",      Format.MemoryFC, Ir.Opcode.unknown },
    Desc { 16_18, 16_0000, Opcode.trapb,   "trapb",   Format.MemoryFC, Ir.Opcode.unknown },
    Desc { 16_18, 16_a000, Opcode.fetch_m, "fetch_m", Format.MemoryFC, Ir.Opcode.unknown },
    Desc { 16_18, 16_c000, Opcode.rpcc,    "rpcc",    Format.MemoryFC, Ir.Opcode.unknown },
    Desc { 16_18, 16_4000, Opcode.mb,      "mb",      Format.MemoryFC, Ir.Opcode.unknown },
    Desc { 16_18, 16_f000, Opcode.rs,      "rs",      Format.MemoryFC, Ir.Opcode.unknown },
    Desc { 16_1a,    16_0, Opcode.jmp,     "jmp",     Format.MemoryJMP, Ir.Opcode.jsr    },
    Desc { 16_1a,    16_2, Opcode.ret,     "ret",     Format.MemoryJMP, Ir.Opcode.ret    },
    Desc { 16_1a,    16_1, Opcode.jsr,     "jsr",     Format.MemoryJMP, Ir.Opcode.jsr   },
    Desc { 16_1a,    16_3, Opcode.jsr_co,  "jsr_co",  Format.MemoryJMP, Ir.Opcode.unknown},
    Desc { 16_30, 16_ffff, Opcode.br,      "br",      Format.Branch,   Ir.Opcode.br      },
    Desc { 16_33, 16_ffff, Opcode.fble,    "fble",    Format.Branch,   Ir.Opcode.unknown },
    Desc { 16_36, 16_ffff, Opcode.fbge,    "fbge",    Format.Branch,   Ir.Opcode.unknown },
    Desc { 16_39, 16_ffff, Opcode.beq,     "beq",     Format.Branch,   Ir.Opcode.beq     },
    Desc { 16_3c, 16_ffff, Opcode.blbs,    "blbs",    Format.Branch,   Ir.Opcode.blbs },
    Desc { 16_3f, 16_ffff, Opcode.bgt,     "bgt",     Format.Branch,   Ir.Opcode.bgt     },
    Desc { 16_31, 16_ffff, Opcode.fbeq,    "fbeq",    Format.Branch,   Ir.Opcode.unknown },
    Desc { 16_34, 16_ffff, Opcode.bsr,     "bsr",     Format.Branch,   Ir.Opcode.call    },
    Desc { 16_37, 16_ffff, Opcode.fbgt,    "fbgt",    Format.Branch,   Ir.Opcode.unknown },
    Desc { 16_3a, 16_ffff, Opcode.blt,     "blt",     Format.Branch,   Ir.Opcode.blt     },
    Desc { 16_3d, 16_ffff, Opcode.bne,     "bne",     Format.Branch,   Ir.Opcode.bne     },
    Desc { 16_32, 16_ffff, Opcode.fblt,    "fblt",    Format.Branch,   Ir.Opcode.unknown },
    Desc { 16_35, 16_ffff, Opcode.fbne,    "fbne",    Format.Branch,   Ir.Opcode.unknown },
    Desc { 16_38, 16_ffff, Opcode.blbc,    "blbc",    Format.Branch,   Ir.Opcode.blbc },
    Desc { 16_3b, 16_ffff, Opcode.ble,     "ble",     Format.Branch,   Ir.Opcode.ble     },
    Desc { 16_3e, 16_ffff, Opcode.bge,     "bge",     Format.Branch,   Ir.Opcode.bge     },
    Desc { 16_10,   16_00, Opcode.addl,    "addl",    Format.Operate,  Ir.Opcode.unknown },
    Desc { 16_10,   16_60, Opcode.addqv,   "addqv",   Format.Operate,  Ir.Opcode.unknown },
    Desc { 16_10,   16_6d, Opcode.cmple,   "cmple",   Format.Operate,  Ir.Opcode.cmple   },
    Desc { 16_10,   16_1d, Opcode.cmpult,  "cmpult",  Format.Operate,  Ir.Opcode.cmpult },
    Desc { 16_10,   16_29, Opcode.subq,    "subq",    Format.Operate,  Ir.Opcode.subq    },
    Desc { 16_10,   16_40, Opcode.addlv,   "addlv",   Format.Operate,  Ir.Opcode.unknown },
    Desc { 16_10,   16_0f, Opcode.cmpbge,  "cmpbge",  Format.Operate,  Ir.Opcode.unknown },
    Desc { 16_10,   16_4d, Opcode.cmplt,   "cmplt",   Format.Operate,  Ir.Opcode.cmplt   },
    Desc { 16_10,   16_09, Opcode.subl,    "subl",    Format.Operate,  Ir.Opcode.unknown },
    Desc { 16_10,   16_69, Opcode.subqv,   "subqv",   Format.Operate,  Ir.Opcode.unknown },
    Desc { 16_10,   16_20, Opcode.addq,    "addq",    Format.Operate,  Ir.Opcode.addq    },
    Desc { 16_10,   16_2d, Opcode.cmpeq,   "cmpeq",   Format.Operate,  Ir.Opcode.cmpeq   },
    Desc { 16_10,   16_3d, Opcode.cmpule,  "cmpule",  Format.Operate,  Ir.Opcode.cmpule },
    Desc { 16_10,   16_49, Opcode.sublv,   "sublv",   Format.Operate,  Ir.Opcode.unknown },
    Desc { 16_10,   16_02, Opcode.s4addl,  "s4addl",  Format.Operate,  Ir.Opcode.s4addl  },
    Desc { 16_10,   16_2b, Opcode.s4subq,  "s4subq",  Format.Operate,  Ir.Opcode.s4subq  },
    Desc { 16_10,   16_1b, Opcode.s8subl,  "s8subl",  Format.Operate,  Ir.Opcode.s8subl  },
    Desc { 16_10,   16_22, Opcode.s4addq,  "s4addq",  Format.Operate,  Ir.Opcode.s4addq  },
    Desc { 16_10,   16_12, Opcode.s8addl,  "s8addl",  Format.Operate,  Ir.Opcode.s8addl  },
    Desc { 16_10,   16_3b, Opcode.s8subq,  "s8subq",  Format.Operate,  Ir.Opcode.s8subq  },
    Desc { 16_10,   16_0b, Opcode.s4subl,  "s4subl",  Format.Operate,  Ir.Opcode.s4subl  },
    Desc { 16_10,   16_32, Opcode.s8addq,  "s8addq",  Format.Operate,  Ir.Opcode.s8addq  },
    Desc { 16_11,   16_00, Opcode.and,     "and",     Format.Operate,  Ir.Opcode.and     },
    Desc { 16_11,   16_24, Opcode.cmoveq,  "cmoveq",  Format.Operate,  Ir.Opcode.cmoveq  },
    Desc { 16_11,   16_08, Opcode.bic,     "bic",     Format.Operate,  Ir.Opcode.andnot  },
    Desc { 16_11,   16_16, Opcode.cmovlbc, "cmovlbc", Format.Operate,  Ir.Opcode.cmovlbc },
    Desc { 16_11,   16_20, Opcode.bis,     "bis",     Format.Operate,  Ir.Opcode.or      },
    Desc { 16_11,   16_14, Opcode.cmovlbs, "cmovlbs", Format.Operate,  Ir.Opcode.cmovlbs },
    Desc { 16_11,   16_46, Opcode.cmovge,  "cmovge",  Format.Operate,  Ir.Opcode.cmovge  },
    Desc { 16_11,   16_44, Opcode.cmovlt,  "cmovlt",  Format.Operate,  Ir.Opcode.cmovlt  },
    Desc { 16_11,   16_28, Opcode.ornot,   "ornot",   Format.Operate,  Ir.Opcode.ornot   },
    Desc { 16_11,   16_66, Opcode.cmovgt,  "cmovgt",  Format.Operate,  Ir.Opcode.cmovgt  },
    Desc { 16_11,   16_26, Opcode.cmovne,  "cmovne",  Format.Operate,  Ir.Opcode.cmovne  },
    Desc { 16_11,   16_40, Opcode.xor,     "xor",     Format.Operate,  Ir.Opcode.xor     },
    Desc { 16_11,   16_64, Opcode.cmovle,  "cmovle",  Format.Operate,  Ir.Opcode.cmovle  },
    Desc { 16_11,   16_48, Opcode.eqv,     "eqv",     Format.Operate,  Ir.Opcode.xornot  },
    Desc { 16_12,   16_06, Opcode.extbl,   "extbl",   Format.Operate,  Ir.Opcode.extbl   },
    Desc { 16_12,   16_7a, Opcode.extqh,   "extqh",   Format.Operate,  Ir.Opcode.extqh   },
    Desc { 16_12,   16_16, Opcode.extwl,   "extwl",   Format.Operate,  Ir.Opcode.extwl   },
    Desc { 16_12,   16_2b, Opcode.insll,   "insll",   Format.Operate,  Ir.Opcode.insll   },
    Desc { 16_12,   16_57, Opcode.inswh,   "inswh",   Format.Operate,  Ir.Opcode.inswh   },
    Desc { 16_12,   16_62, Opcode.msklh,   "msklh",   Format.Operate,  Ir.Opcode.msklh   },
    Desc { 16_12,   16_32, Opcode.mskql,   "mskql",   Format.Operate,  Ir.Opcode.mskql   },
    Desc { 16_12,   16_39, Opcode.sll,     "sll",     Format.Operate,  Ir.Opcode.sll     },
    Desc { 16_12,   16_30, Opcode.zap,     "zap",     Format.Operate,  Ir.Opcode.zap },
    Desc { 16_12,   16_6a, Opcode.extlh,   "extlh",   Format.Operate,  Ir.Opcode.extlh },
    Desc { 16_12,   16_36, Opcode.extql,   "extql",   Format.Operate,  Ir.Opcode.extql },
    Desc { 16_12,   16_0b, Opcode.insbl,   "insbl",   Format.Operate,  Ir.Opcode.insbl },
    Desc { 16_12,   16_77, Opcode.insqh,   "insqh",   Format.Operate,  Ir.Opcode.insqh },
    Desc { 16_12,   16_1b, Opcode.inswl,   "inswl",   Format.Operate,  Ir.Opcode.inswl },
    Desc { 16_12,   16_22, Opcode.mskll,   "mskll",   Format.Operate,  Ir.Opcode.mskll },
    Desc { 16_12,   16_52, Opcode.mskwh,   "mskwh",   Format.Operate,  Ir.Opcode.mskwh },
    Desc { 16_12,   16_3c, Opcode.sra,     "sra",     Format.Operate,  Ir.Opcode.sra     },
    Desc { 16_12,   16_31, Opcode.zapnot,  "zapnot",  Format.Operate,  Ir.Opcode.zapnot },
    Desc { 16_12,   16_26, Opcode.extll,   "extll",   Format.Operate,  Ir.Opcode.extll },
    Desc { 16_12,   16_5a, Opcode.extwh,   "extwh",   Format.Operate,  Ir.Opcode.extwh },
    Desc { 16_12,   16_67, Opcode.inslh,   "inslh",   Format.Operate,  Ir.Opcode.inslh },
    Desc { 16_12,   16_3b, Opcode.insql,   "insql",   Format.Operate,  Ir.Opcode.insql },
    Desc { 16_12,   16_02, Opcode.mskbl,   "mskbl",   Format.Operate,  Ir.Opcode.mskbl },
    Desc { 16_12,   16_72, Opcode.mskqh,   "mskqh",   Format.Operate,  Ir.Opcode.mskqh },
    Desc { 16_12,   16_12, Opcode.mskwl,   "mskwl",   Format.Operate,  Ir.Opcode.mskwl },
    Desc { 16_12,   16_34, Opcode.srl,     "srl",     Format.Operate,  Ir.Opcode.srl     },
    Desc { 16_13,   16_00, Opcode.mull,    "mull",    Format.Operate,  Ir.Opcode.unknown },
    Desc { 16_13,   16_60, Opcode.mulqv,   "mulqv",   Format.Operate,  Ir.Opcode.unknown },
    Desc { 16_13,   16_40, Opcode.mullv,   "mullv",   Format.Operate,  Ir.Opcode.unknown },
    Desc { 16_13,   16_30, Opcode.umulh,   "umulh",   Format.Operate,  Ir.Opcode.unknown },
    Desc { 16_13,   16_20, Opcode.mulq,    "mulq",    Format.Operate,  Ir.Opcode.mulq    }
  };

(******************************************************************************
 *
 * code representation
 *
 *****************************************************************************)

CONST
  BitWidth = BITSIZE(T);
  ByteWidth = BYTESIZE(T);

TYPE
  Desc = RECORD
    op_val : INTEGER;
    fc_val : INTEGER;
    Opcode : Opcode;
    name   : TEXT;
    format : Format;
    ir     : Ir.Opcode;
  END;

  T = BITS 32 FOR [0..16_ffffffff];
  TPtr = UNTRACED REF T;

(* operations *)

PROCEDURE SetInstField (VAR dst: T; val, mask, shift: INTEGER);
PROCEDURE GetInstField (dst: T; mask, shift: INTEGER) : INTEGER;
PROCEDURE SetOpcode (VAR dst: T; code: INTEGER);
PROCEDURE SetOp (VAR dst: T; op: Opcode);
PROCEDURE GetOpcode (inst_val: T): INTEGER;
PROCEDURE SetBranchPred (VAR dst: T; pred: INTEGER);
PROCEDURE GetBranchPred (inst_val: T): INTEGER;
PROCEDURE SetTarget (VAR dst: T; target: INTEGER);
PROCEDURE GetTarget (inst_val: T): INTEGER;
PROCEDURE SetLitFlag (VAR dst: T; flag: BOOLEAN);
PROCEDURE GetLitFlag (inst_val: T): BOOLEAN;
PROCEDURE SetNumber (VAR dst: T; num: INTEGER);
PROCEDURE GetNumber (inst_val: T): INTEGER;
PROCEDURE SetFunction (VAR dst: T; fc: INTEGER);
PROCEDURE GetFunction (inst_val: T): INTEGER;
PROCEDURE SetRegA (VAR dst: T; reg: INTEGER);
PROCEDURE GetRegA (inst_val: T): INTEGER;
PROCEDURE SetRegB (VAR dst: T; reg: INTEGER);
PROCEDURE GetRegB (inst_val: T): INTEGER;
PROCEDURE SetRegC (VAR dst: T; reg: INTEGER);
PROCEDURE GetRegC (inst_val: T): INTEGER;
PROCEDURE SetDisp (VAR dst: T; disp: INTEGER);
PROCEDURE GetDisp (inst_val: T): INTEGER;
PROCEDURE SetBranchDisp (VAR dst: T; disp: INTEGER);  (* in instructions *)
PROCEDURE GetBranchDisp (inst_val: T): INTEGER;
PROCEDURE SetLit (VAR dst: T; lit: CARDINAL);
PROCEDURE GetLit (inst_val: T): INTEGER;
PROCEDURE FindByOpcode (opVal: INTEGER): Opcode;
PROCEDURE FindByFunction (idx: Opcode; op_val, fc_val: INTEGER): Opcode;

PROCEDURE Print (inst: T; s: Wr.T := NIL);
PROCEDURE GetOp (inst_val: T) : Opcode;
PROCEDURE PrintUntilReturn (ptr: TPtr; name: TEXT);

END Instruction.

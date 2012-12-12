(*
 * Copyright 1996, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 16-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Created from Alpha version.
 *
 *
 *)
INTERFACE CPUDep;
IMPORT Word;

TYPE GeneralRegister = Word.T;

TYPE SegmentRegister = Word.T;

TYPE FloatRegister = ARRAY[1..10] OF CHAR; (* sort of *)

TYPE InterruptLevel = Word.T;
TYPE InterruptClass = {
		       Low,
		       SoftClock,
		       IO,
		       Clock,
		       High
		       };

TYPE FloatRegs = RECORD
  Control: Word.T;
  Status: Word.T;
  Tag: Word.T;
  IpOffset: Word.T;
  CSSelector: Word.T;
  DataOffset: Word.T;
  OperandSelector: Word.T;
  Registers: ARRAY[1..20] OF Word.T;
END;

(* this structure has to be in sync with core.s and frame.h *)
TYPE SavedState = RECORD
  vec: Word.T;
  spl: InterruptLevel;
  es: SegmentRegister;
  ds: SegmentRegister;
  edi: GeneralRegister;
  esi: GeneralRegister;
  ebp: GeneralRegister;
  ksp: GeneralRegister;
  ebx: GeneralRegister;
  edx: GeneralRegister;
  ecx: GeneralRegister;
  eax: GeneralRegister;
  trapno: Word.T;
  err: Word.T;
  pc: Word.T;
  cs: SegmentRegister;
  eflags: Word.T;
  usp: GeneralRegister;
  uss: SegmentRegister;
END;

TYPE GeneralRegs = SavedState;

TYPE MachineState = RECORD
  eax : GeneralRegister;
  ecx : GeneralRegister;
  edx : GeneralRegister;
  ebx : GeneralRegister;
  esp : GeneralRegister;
  ebp : GeneralRegister;
  esi : GeneralRegister;
  edi : GeneralRegister;
  eip : GeneralRegister;
  efl : Word.T;
  cs : SegmentRegister;
  ss : SegmentRegister;
  ds : SegmentRegister;
  es : SegmentRegister;
  fs : SegmentRegister;
  gs : SegmentRegister;
END;


TYPE TrapType =
{
  DIVIDE_ERROR,
  DEBUG,
  NMI,
  BREAKPOINT,
  INTO_OVERFLOW,
  BOUND_EXCEEDED,
  INVALID_OPCODE,
  DEVICE_NA,
  DOUBLE_FAULT,
  RESERVED1,
  INVALID_TSS,
  SEGMENT_NA,
  STACK_FAULT,
  GPF,
  PAGE_FAULT,
  RESERVED2,
  FP_ERROR,
  ALIGNMENT_CHECK,
  RESERVED3
};

TYPE
  CalleeSavedRegs = RECORD
    sp, ebx, edi, esi, ebp, ra: Word.T;
  END;

CONST
  PGSHIFT: Word.T	 = 12;    (* Number of bits to shift for pages *)
  PAGESIZE: Word.T	 = 4096;  (* Size of a page *)
  PAGEMASK: Word.T	 = PAGESIZE-1;

TYPE
  VirtAddress		 = Word.T;
  PhysAddress		 = Word.T;

END CPUDep.


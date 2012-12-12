(*
 * Copyright 1996, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 11-Jan-97  Yasushi Saito (yasushi) at the University of Washington
 *	Unified RTOSMachine.InterruptLevel and this one.
 * 10-Dec-94  Emin Gun Sirer (egs) at the University of Washington
 *	Machine dependent CPU specific types and constants.
 *
 *)
INTERFACE CPUDep;
IMPORT Word;

TYPE
  GeneralRegister = Word.T;
  FloatRegister = Word.T;
  FRegGFormat = 
  RECORD   (* An Float Register in g format is actually this. *)
    sign: BITS  1 FOR [0..1];
    exp : BITS 11 FOR [0..2047];
    frac: BITS 52 FOR [0..4503599627370495];
  END;

TYPE FloatRegs = RECORD
  (* Floating point state *)
  fpr: ARRAY [0..31] OF FloatRegister;
  csr: GeneralRegister;
END;

(* this structure has to be in sync with core.s *)
TYPE SavedState = RECORD
        (* core saved portion of register state *)
        v0: GeneralRegister;
        t0, t1, t2, t3, t4, t5, t6, t7: GeneralRegister;
        s0, s1, s2, s3, s4, s5, s6: GeneralRegister;
        a3, a4, a5: GeneralRegister;
        t8, t9, t10, t11: GeneralRegister;
        ra: GeneralRegister;
        pv: GeneralRegister;
        at: GeneralRegister;
        ksp, usp: GeneralRegister;

        (* PAL saved portion of state *)
        ps: GeneralRegister;
        pc: GeneralRegister;
        gp: GeneralRegister;
        a0, a1, a2: GeneralRegister;
END;

TYPE MachineState = RECORD
  v0: GeneralRegister;
  t0: GeneralRegister;
  t1: GeneralRegister;
  t2: GeneralRegister;
  t3: GeneralRegister;
  t4: GeneralRegister;
  t5: GeneralRegister;
  t6: GeneralRegister;
  t7: GeneralRegister;
  s0: GeneralRegister;   
  s1: GeneralRegister;
  s2: GeneralRegister;
  s3: GeneralRegister;
  s4: GeneralRegister;
  s5: GeneralRegister;
  fp: GeneralRegister;
  a0: GeneralRegister;   
  a1: GeneralRegister;
  a2: GeneralRegister;
  a3: GeneralRegister;
  a4: GeneralRegister;
  a5: GeneralRegister;
  t8: GeneralRegister;
  t9: GeneralRegister;
  t10: GeneralRegister;
  t11: GeneralRegister;
  ra: GeneralRegister;
  t12: GeneralRegister;
  at: GeneralRegister;
  gp: GeneralRegister;
  sp: GeneralRegister;
  zero: GeneralRegister;
  floats: ARRAY [0..31] OF FloatRegister;
  pc: GeneralRegister;
  vfp: GeneralRegister;
  bad_address: VirtAddress;
  cause: Word.T;
END;

TYPE GeneralRegs = SavedState;

TYPE
  InterruptLevel = {
		    Low,
		    SoftClock,
		    SPL_2, SPL_3, 
		    IO,
		    Clock,
		    SPL_6,
		    High
		    };
  InterruptClass = InterruptLevel;
  
  (* Alert! same definition is duplicated in RTOSMachine.i3. We have to
     merge them. *)

(* Alpha OSF PAL code system entry points *)
TYPE TrapType =
{  KNIO,
   ARITHMETIC,
   MEMORYMANAGEMENT,
   INSNFAULT,
   UNALIGNED,
   SYSCALL,
   CLOCK,       (* this is a pseudo-interrupt - it is demuxed from entInt *)
   MACHINECHECK,(* this is a pseudo-interrupt - it is demuxed from entInt *)
   PFM, (* performance monitor, this is a pseudo-interrupt *)
   STRAY        (* this is a pseudo-interrupt - it is demuxed from entInt *)
};

TYPE
  CalleeSavedRegs = RECORD
    ra, sp, extra1, extra2, s0, s1, s2, s3, s4, s5, s6: Word.T;
  END;

CONST
  PGSHIFT: Word.T	 = 13;    (* Number of bits to shift for pages *)
  PAGESIZE: Word.T	 = 8192;  (* Size of a page *)
  PAGEMASK: Word.T	 = PAGESIZE-1;

TYPE
  VirtAddress		 = Word.T;
  PhysAddress		 = Word.T;

END CPUDep.

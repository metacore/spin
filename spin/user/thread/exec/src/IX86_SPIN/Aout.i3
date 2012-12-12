(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created. Coff definitions. The names correspond to C equivalents.
 *)
INTERFACE Aout;
IMPORT Ctypes;

TYPE AoutHeader = RECORD
  a_midmag:   Ctypes.long;
  a_text:     Ctypes.long;
  a_data:     Ctypes.long;
  a_bss:      Ctypes.long;
  a_syms:     Ctypes.long;
  a_entry:    Ctypes.long;
  a_trsize:   Ctypes.long;
  a_drsize:   Ctypes.long;
END;

CONST OMAGIC = 8_407;	(* old impure format  *)
CONST NMAGIC = 8_410;	(* read-only text     *)
CONST ZMAGIC = 8_413;	(* demand load format *)
CONST QMAGIC = 8_314;	(* demand load format *)

PROCEDURE N_BADMAG(READONLY a: AoutHeader) : BOOLEAN;
PROCEDURE N_TXTOFF(READONLY a: AoutHeader) : INTEGER;
PROCEDURE N_TXTADDR(READONLY a: AoutHeader) : INTEGER;

END Aout.

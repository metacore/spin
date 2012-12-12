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
INTERFACE Coff;
IMPORT Ctypes;

TYPE Exechdr = RECORD
  f: Filehdr;
  a: Aouthdr;
END;

TYPE Filehdr = RECORD
  f_magic:  Ctypes.unsigned_short;  (* magic number *)
  f_nscns:  Ctypes.unsigned_short;  (* number of sections *)
  f_timdat: Ctypes.int;             (* time & date stamp *)
  f_symptr: Ctypes.long;            (* file pointer to symtab *)
  f_nsyms:  Ctypes.int;             (* number of symtab entries *)
  f_opthdr: Ctypes.unsigned_short;  (* sizeof(optional hdr) *)
  f_flags:  Ctypes.unsigned_short;
END;

TYPE Scnhdr = RECORD
  s_name:    Ctypes.long;           (* section name *)
  s_paddr:   Ctypes.long;           (* physical address *)
  s_vaddr:   Ctypes.long;           (* virtual address *)
  s_size:    Ctypes.long;           (* section size *)
  s_scnptr:  Ctypes.long;           (* file ptr to raw data for section *)
  s_relptr:  Ctypes.long;           (* file ptr to relocation *)
  s_lnnoptr: Ctypes.long;           (* file ptr to line numbers *)
  s_nreloc:  Ctypes.unsigned_short; (* number of relocation entries *)
  s_nlnno:   Ctypes.unsigned_short; (* number of line number entries *)
  s_flags:   Ctypes.int;            (* flags *)
END;

TYPE Aouthdr = RECORD
  magic:      Ctypes.short;  (* see magic.h		         *)
  vstamp:     Ctypes.short;  (* version stamp		         *)
  pad:        Ctypes.int;    (* help for cross compilers	 *)
  tsize:      Ctypes.long;   (* text size in bytes, padded to FW *)
  dsize:      Ctypes.long;   (* initialized data "  "	         *)
  bsize:      Ctypes.long;   (* uninitialized data "   "	 *)
  entry:      Ctypes.long;   (* entry point, value of "start"    *)
  text_start: Ctypes.long;   (* base of text used for this file  *)
  data_start: Ctypes.long;   (* base of data used for this file  *)
  bss_start:  Ctypes.long;   (* base of bss used for this file   *)
  gprmask:    Ctypes.int;    (* general purpose register mask    *)
  fprmask:    Ctypes.int;    (* FPA register mask		 *)
  gp_value:   Ctypes.long;   (* the gp value used for this object*)
END;

CONST OMAGIC = 8_407;	(* old impure format  *)
CONST NMAGIC = 8_410;	(* read-only text     *)
CONST ZMAGIC = 8_413;	(* demand load format *)

CONST ALPHAMAGIC = 8_603;

CONST SCNROUND = 16;

CONST F_EXEC = 0000002;

PROCEDURE N_BADMAG(READONLY a: Aouthdr) : BOOLEAN;
PROCEDURE N_TXTOFF(READONLY f: Filehdr; READONLY a: Aouthdr) : INTEGER;
  
PROCEDURE Dynamic(READONLY f: Filehdr): BOOLEAN;
  (* Return TRUE if the prog is dynlinked. *)
  
END Coff.

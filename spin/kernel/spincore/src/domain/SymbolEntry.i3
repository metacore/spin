(*
 * HISTORY
 * 03-Sep-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed SymbolEntry.E to mirror the SymbolEntry.T type with the only
 *	exception that the SE_name field is a TEXT rather than a char_star.
 *
 * 07-Oct-95  Marc Fiuczynski (mef) at the University of Washington
 *	Changed SE_ptr and SE_nvalue to be Word.T's instead of ADDRESS.
 *	
 * 05-Oct-95  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *
 *)

INTERFACE SymbolEntry;
IMPORT Ctypes;
IMPORT Word;

CONST scText =         1;                   (* text symbol             *)
CONST scData =         2;                   (* initialized data symbol *)

TYPE T = RECORD
  SE_ptr     : ADDRESS;          (* void         *SE_ptr;       /* ptr to this entity            */ *)
  SE_name    : Ctypes.char_star; (* char         *SE_name;      /* ptr to symbol in StringList   */ *)
  SE_nvalue  : ADDRESS;          (* long          SE_nvalue;    /* n_value of symbol             */ *)
  SE_nsclass : INTEGER;          (* Ctypes.char; *)      (* char          SE_nsclass;   /* n_sclass of symbol            */ *)
  SE_module  : Word.T;           (* Module       *SE_module;    /* module in which it is defined */ *)
END;  

(* XXX same as a SymbolEntry.T except that SE_name : TEXT *)
TYPE E = RECORD 
  SE_ptr     : ADDRESS;
  SE_name    : TEXT;
  SE_nvalue  : ADDRESS;
  SE_nsclass : INTEGER;
  SE_module  : Word.T;
END;

END SymbolEntry.

(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 18-Jun-96  Yasushi Saito (yasushi) at the University of Washington
 *	Changed to raise Error.E
 *	
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created. Coff aout parser for bootstrapping user level tasks.
 *)

INTERFACE AoutParser;
IMPORT Space, File, Error;
IMPORT Word;

TYPE Info = RECORD
  pc : Word.T;
  break: Word.T;  (* last BSS addr + 1 *)
  stackTop: Word.T;
END;
   
PROCEDURE GetExecutable(s: Space.T; fp: File.T; VAR info : Info)
  RAISES {Error.E};
(* Read an executable file "fp" into the address space "s".
   "info" will hold some information about the file on successful
   return.

  Note: Stack segment is not set up. 
 *)

PROCEDURE IsExecutable(fp : File.T) : BOOLEAN RAISES {Error.E};
  
END AoutParser.
 






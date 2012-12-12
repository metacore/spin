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
 * html
 *)

(* This modules defines "Info", a record to hold varioun info about the
   a.out file. *)

INTERFACE AoutParser;
IMPORT Space, File, Error;
IMPORT Word;

TYPE Info = RECORD
  pc, gp : Word.T; (* initial program counter and global ptr(see Alpha ref) *)
  break : Word.T;  (* last addr of BSS seg + 1 *)
  stackTop: Word.T; (* Note: this field is not filled by GetInfo module. *)
  dynamic: BOOLEAN; (* dynamically linked binary? *)
END;
   
PROCEDURE GetExecutable(s: Space.T; fp: File.T) RAISES {Error.E};
(* Read an executable file "fp" into the address space "s".
   Note: Stack segment is not set up. 
 *)

PROCEDURE GetInfo(fp : File.T; VAR info:Info) RAISES {Error.E};
(* "info" will hold some information about the file on successful
   return.
   Note: "stackTop" is not set.
 *)
  
END AoutParser.
 

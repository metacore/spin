(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 05-Nov-95  Yasushi Saito (yasushi) at the University of Washington
 *	created
 *)

(*
 *  Knows everything about a module.
 *)

INTERFACE Module;
IMPORT NameSeq, TextSeq;
IMPORT Declaration;
IMPORT M3CPragma, TextRefTbl;

TYPE T = RECORD
  intf : TEXT; (* Interface name. *)
  domain: TEXT; (* Name of the domain *)
  
  names : NameSeq.T; (* local identifiers *)
  imports : TextSeq.T; (* list of imported module names *)
  
  pragmas : M3CPragma.Store; (* list of pragmas *)

  errorFunction : TEXT; (* this function should be called when a3 is not
			 zero upon syscall return *)
  afterHook : TEXT; (* this function should be called after each syscall *)
		     
  exceptionList : TextRefTbl.T; (* list of all the exceptions raised by the
				 procedures in this interface *)
  descendingProcIDs: BOOLEAN;
  noVarOptimization: BOOLEAN;
  
  minProcID, maxProcID : INTEGER; (* min and max proc ids in the module.
				   XXX this is set in RegScheduler now.
				   We have to move it to Pass1. *)
END;

PROCEDURE Lookup(READONLY m : T; READONLY name : TEXT;
		 VAR body : Declaration.T) : BOOLEAN;
(* Look up a "name" and return whatever found.
 *)

 
PROCEDURE LookupProc(READONLY m : T; id : INTEGER; 
		     VAR body : Declaration.Proc) : BOOLEAN;
(* Look up the procedure with "id" and return Type.Proc. If not found, return
 NIL. *)
  
END Module.

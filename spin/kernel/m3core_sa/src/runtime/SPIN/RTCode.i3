(* 
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 10-Jan-96  Brian Bershad (bershad) at the University of Washington
 *      Added iteration support. Whisted.
 *
 *
 * 10-26-95  Charlie Garrett
 *      Created.
 *
 *      RTCode is the interface through which users access the
 *      runtime representation of interfaces and modules.
 *
 *      RTCodeBuiltin is the subset of the interface which the
 *      SPIN M3 compiler has built into it. 
 *)




INTERFACE RTCode;
IMPORT RTCodeBuiltin;

TYPE
    Module = RTCodeBuiltin.Module;
    Interface = RTCodeBuiltin.Interface;

(* returns true iff m exports a function declared in i *)
PROCEDURE ModuleExportsInterface(m: Module; i: Interface): BOOLEAN;

(* return true iff m defines procedure p *)
PROCEDURE ModuleImplementsProcedure(m: Module; p_ref: PROCANY): BOOLEAN; 



(*
 * Iterators 
 *)
TYPE Iterator = OBJECT
  METHODS
  apply(p: PROCANY; name: TEXT): BOOLEAN;       (* return TRUE to continue. *)
END;

PROCEDURE IterateOnProceduresInInterface(i: Interface; iter: Iterator);
PROCEDURE IterateOnProceduresInModule(m: Module; iter: Iterator);


END RTCode.

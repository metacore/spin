(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Fri Nov 11 13:12:43 PST 1994 by kalsow     *)

(* RTLinker defines the values initialized by the linker and
   startup code.
*)

(*
 * HISTORY
 * 13-Jun-97  Wilson Hsieh (whsieh) at the University of Washington
 *	fixed a comment
 *
 * 11-Feb-97  Przemek Pardyak (pardy) at the University of Washington
 *	Cleaned-up a bit. Added subtruction of modules.
 *
 * 14-Jun-96  Przemek Pardyak (pardy) at the University of Washington
 *	Got rid of typeidx.
 *
 * 02-Apr-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added the "link_info" field to the LinkInfo structure to pass
 *	interface link information list to the arguments passed from 
 *	_m3main.c to the runtime.
 *
 * 13-Nov-95  Przemek Pardyak (pardy) at the University of Washington
 *	Changed the type of range of statistics vectors from 
 *	RT0.Typecode into RT0.Typeidx (see RT0.i3)
 *
 * 14-Jul-95  Przemek Pardyak (pardy) at the University of Washington
 *	Added support for initialization of dynamicly linked modules
 *
 *)

INTERFACE RTLinker;
IMPORT RT0;

TYPE
  LinkInfo = UNTRACED REF RECORD
    (* global module table *)
    n_modules  : INTEGER;
    modules    : ADDRESS; (* ptr to ARRAY [0..n_modules-1] OF RT0.ModulePtr *)
    link_info  : UNTRACED REF UNTRACED REF CHAR; 

    (* external environment *)
    argc       : INTEGER;
    argv       : ADDRESS;
    envp       : ADDRESS;
    instance   : ADDRESS;  (* Windows "instance" handle *)

    (* initial thread bounds *)
    bottom_of_stack : ADDRESS;
    top_of_stack    : ADDRESS;
  END;

VAR info: LinkInfo;

TYPE
  TypeInfo = RECORD
    nTypes: CARDINAL;
    totalTypes: CARDINAL;
    modules_array : ADDRESS; 
    types: UNTRACED REF (*ARRAY OF*) RT0.TypeDefn := NIL;
    n_type_ids : INTEGER;
    type_ids   : ADDRESS; (* REF ARRAY [0..n_type_ids-1] OF IDMap *)
    alloc_cnts : UNTRACED REF ARRAY RT0.Typecode OF INTEGER;
    alloc_bytes : UNTRACED REF ARRAY RT0.Typecode OF INTEGER;
    subTypeCode : UNTRACED REF INTEGER;
    lastSubTypeCode : UNTRACED REF INTEGER;
  END;

VAR
  type_info: TypeInfo;

VAR
  n_initialized: INTEGER;

TYPE
  ModulePtrs = UNTRACED REF ARRAY OF RT0.ModulePtr;

PROCEDURE Reinitialize (modules: ModulePtrs): BOOLEAN;
PROCEDURE Destroy (newModules: ModulePtrs):BOOLEAN;
PROCEDURE SubtractModules (modules: ModulePtrs) : ModulePtrs;
PROCEDURE Update ();
PROCEDURE CleanUp ();
PROCEDURE ExecuteMainBodies ();

END RTLinker.


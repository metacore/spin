(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Fri Nov 11 13:12:43 PST 1994 by kalsow     *)

(* RTLinker defines the values initialized by the linker and
   startup code.
*)

UNSAFE INTERFACE RTLinker;
IMPORT RT0;

TYPE
  LinkInfo = UNTRACED REF RECORD
    (* global module table *)
    n_modules  : INTEGER;
    modules    : ADDRESS; (* REF ARRAY [0..n_modules-1] OF RT0.ModulePtr *)
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

TYPE (* SPIN *)
  TypeInfo = RECORD
    nTypes: CARDINAL;
    modules_array : ADDRESS; 
    types: UNTRACED REF (*ARRAY OF*) RT0.TypeDefn := NIL;
    n_type_ids : INTEGER;
    type_ids   : ADDRESS; (* REF ARRAY [0..n_type_ids-1] OF IDMap *)
    alloc_cnts : UNTRACED REF ARRAY RT0.Typecode OF INTEGER;
    alloc_bytes : UNTRACED REF ARRAY RT0.Typecode OF INTEGER;
    subTypeCode : UNTRACED REF INTEGER;
    lastSubTypeCode : UNTRACED REF INTEGER;
  END;

VAR type_info: TypeInfo; (* SPIN *)

VAR n_initialized: INTEGER; (* SPIN *)

END RTLinker.


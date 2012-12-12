(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Thu Jul 14 11:29:36 PDT 1994 by kalsow     *)

(* RT0u is almost "the bottom of the world".  It contains
   variables that are shared by multiple modules of the runtime
   and/or the compiler and linker.

   If you're using this interface, you're a wizard!

   This interface and its implemenation MUST NOT import any
   interface other than RT0.
*)

(*
 * HISTORY
 * 14-Nov-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added counts for correct cleanup of the runtime on initialization.
 *
 * 14-Jun-96  Przemek Pardyak (pardy) at the University of Washington
 *	Made RT0u__types external to save one load (through interface) 
 *	on each NARROW.
 *
 * 17-Feb-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added external symbols describing memory layout of the system.
 *
 * 15-Nov-95  Przemek Pardyak (pardy) at the University of Washington
 *	Added a counter of reconfigurations.  Used to distinguish
 *	incarnations of the runtime after each dynamic linking.
 *
 * 13-Nov-95  Przemek Pardyak (pardy) at the University of Washington
 *	Changed the type of range of statistics vectors from 
 *	RT0.Typecode into RT0.Typeidx (see RT0.i3)
 *)

UNSAFE INTERFACE RT0u;

IMPORT RT0;
(* SPIN mod *)
(* Unsigned integers are not a builtin type. Words do
   not depend on any other interface and do not do any initialization *)
IMPORT Word;

(*--------------------------------------------------- global module table ---*)

VAR (*CONST*) nModules: CARDINAL := 0;  (* == # compilation units in pgm *)
(* initialized by RTHooks' main body *)

VAR (*CONST*) modules: UNTRACED REF (*ARRAY OF*) RT0.ModulePtr := NIL;
(* initially allocated by the linker, after reinitalization it points to *)
(* element 0 in modules_array its actual bounds are [0..nModules-1] *)

VAR (*CONST*) modules_array: UNTRACED REF ARRAY OF RT0.ModulePtr := NIL;
(* created during reinitialization *)

(*----------------------------------------------------- global type table ---*)

VAR (*CONST*) nTypes: CARDINAL := 0;   (* == max allocated typecode *)
(* initialized by RTType.Init. *)

<* EXTERNAL RT0u__types *>
VAR (*CONST*) types: UNTRACED REF (*ARRAY OF*) RT0.TypeDefn;
(* allocated by the startup code, its actual bounds are [0..nTypes-1] *)

(*------------------------------------------------------ mutual exclusion ---*)

<*EXTERNAL RT0u__inCritical*>
VAR inCritical: INTEGER;
(* inCritical provides low-level mutual exclusion between the thread
   runtime, garbage collector and the Unix signal that triggers thread
   preemption.  If inCritical is greater than zero, thread preemption
   is disabled.  We *ASSUME* that "INC(inCritical)" and "DEC(inCritical)"
   generate code that is atomic with respect to Unix signal delivery. *)

(*------------------------------------------------------- allocator stats ---*)


VAR (*CONST*) alloc_cnts : UNTRACED REF ARRAY RT0.Typecode OF INTEGER;
(* allocated by the startup code, its actual bounds are [0..nTypes-1] *)

VAR (*CONST*) alloc_bytes: UNTRACED REF ARRAY RT0.Typecode OF INTEGER;
(* allocated by the startup code, its actual bounds are [0..nTypes-1] *)

VAR total_traced_bytes: Word.T := 0;
(* total number of bytes allocated by M3 runtime on traced heap *)

VAR total_untraced_bytes: Word.T := 0;
(* total number of bytes allocated by M3 runtime on untraced heap *)

(*------------------------------------------------- reinitialization ------- *)

<*EXTERNAL RT0u__RuntimeEpoch*>
VAR TypeEpoch: INTEGER;

VAR 
  static_reinitialized := FALSE;
  free_nModules, free_nTypes, free_n_type_ids: INTEGER;
  free_modules: UNTRACED REF ARRAY OF RT0.ModulePtr;
  free_types, free_alloc_cnts, free_alloc_bytes: ADDRESS;
  free_subTypeCode, free_lastSubTypeCode, free_type_ids: ADDRESS;

(*---------------------------------------------------------------------------*)

<* EXTERNAL "_edata"  *> VAR E_edata : INTEGER;
<* EXTERNAL "_etext"  *> VAR E_etext : INTEGER;
<* EXTERNAL "_end"    *> VAR E_end   : INTEGER;
<* EXTERNAL *> VAR untraced_start : ADDRESS;
<* EXTERNAL *> VAR untraced_end   : ADDRESS;
<* EXTERNAL *> VAR kmembase       : ADDRESS;
<* EXTERNAL *> VAR kmemlimit      : ADDRESS;

END RT0u.


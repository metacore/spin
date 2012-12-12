(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Host.i3                                               *)
(* Last modified on Wed Oct 12 16:52:13 PDT 1994 by kalsow     *)
(*      modified on Sat May 12 07:05:52 1990 by muller         *)

(*
 * HISTORY
 * 24-Aug-97  Przemek Pardyak (pardy) at the University of Washington
 *	Added code to pass compile flags around and change them
 *	during recompilation.
 *
 * 18-Feb-97  Wilson Hsieh (whsieh) at the University of Washington
 *	read and write barrier
 *
 * 28-Jan-97  Wilson Hsieh (whsieh) at the University of Washington
 *	add ref_count option, no_float
 *
 * 28-Jan-97  Marc Fiuczynski (mef) at the University of Washington
 *	Modification to turn off the generation of dynamic link
 *	information and type information for procedures.
 *
 * 25-Nov-96  Wilson Hsieh (whsieh) at the University of Washington
 *	removed unused new_opaque_ok global
 *
 * 25-Jul-96  Wilson Hsieh (whsieh) at the University of Washington
 *	add method_ok to allow methods to be used as procedures
 *
 * 14-Jun-96  Przemek Pardyak (pardy) at the University of Washington
 *	Got rid of "-UserSpace" option. 
 *
 * 03-Jun-96  Wilson Hsieh (whsieh) at the University of Washington
 *	remove unused stuff
 *
 * 21-May-96  Wilson Hsieh (whsieh) at the University of Washington
 *	remove ViewAlignmentOff
 *
 * 12-Apr-96  Wilson Hsieh (whsieh) at the University of Washington
 *	support for NoLoopholeWarning
 *
 * 22-Feb-96  Charles Garrett (garrett) at the University of Washington
 *	Added the Phase type and currentPhase variable which are used to
 *	 fix compiler PR/42. Since the compiler can check which Phase it
 *	 is in, it will not print duplicate warnings.
 *
 * 16-Feb-96  Wilson Hsieh (whsieh) at the University of Washington
 *	support for ViewAlignmentOff flag
 *
 * 29-Dec-95  Przemek Pardyak (pardy) at the University of Washington
 *	Added "-UserSpace" option. 
 *
 * 29-Dec-95  Przemek Pardyak (pardy) at the University of Washington
 *	Whisted.  Previous SPIN changes: added extern_ok and new_opaque_ok
 *	variables to support safety features: no extern declarations in 
 *	safe interfaces and no allocation of opaque object types.  These
 *	features are overridable for backward compatibility using 
 *	"-SpinRelax" option.
 *
 *)

INTERFACE Host;

IMPORT M3ID, File, M3Compiler;

VAR (* parameters to the top-level compile *)
  filename      : TEXT := NIL;
  source        : File.T;
  env           : M3Compiler.Environment;

VAR (* misc. options *)
  verbose       : BOOLEAN := FALSE; (* => diagnostic listing of phases *)
  errorDie      : INTEGER := -1;    (* N>=0 => abort on the Nth error *)
  warnings      : INTEGER := 2;     (* => ignore levels less than this value *)
  coverage      : BOOLEAN := FALSE; (* => generate coverage counts *)
  versionStamps : BOOLEAN := TRUE;  (* do emit version stamps *)
  emitBuiltins  : BOOLEAN := FALSE; (* do not compile, emit the builtins *)
  init_floats   : BOOLEAN := FALSE; (* initialize all floats to zero *)
  vs_debug      : BOOLEAN := FALSE; (* print version stamp debugging *)
  load_map      : BOOLEAN := TRUE;  (* print unit's load map as a comment *)
  ext_direct    : BOOLEAN := TRUE;  (* call external procedure directly? *)
  all_direct    : BOOLEAN := FALSE; (* call all procedures directly? *)
  stack_walker  : BOOLEAN := TRUE;  (* use the Target specified stack walker *)
  verbose_checks: BOOLEAN := FALSE; (* warn the user about runtime checks? *)

  nested_calls : BOOLEAN := FALSE;
  (* can calls to procedures be nested? *)

  nested_procs_first : BOOLEAN := TRUE;
  (* should nested procedures be generated before or after their parent *)

  inline_nested_procs : BOOLEAN := TRUE;
  (* should the code for nested procedures be generated inline *)
    
  clean_stores : BOOLEAN := FALSE;
  (* must the stack be empty after every store *)

  clean_jumps : BOOLEAN := TRUE;
  (* must the stack be empty before every jump or call *)

  (* SPIN *)
  extern_ok : BOOLEAN := FALSE;
  (* We do not allow externals in safe interfaces, but allow it
     for backward compatibility. *)

  (* SPIN *)
  method_ok : BOOLEAN := FALSE;
  (* We do not allow methods to be used as procedures, but allow it
     for backward compatibility. *)

  (* SPIN *)
  loophole_warning_off : BOOLEAN := FALSE;
  (* We issue warnings for all LOOPHOLE calls. *)

  (* SPIN *)
  no_float : BOOLEAN := FALSE;
  (* issue errors for use of floats *)

  (* SPIN *)
  ref_count : BOOLEAN := FALSE;
  (* generate reference counting code *)

  (* SPIN *)
  trace_ref : BOOLEAN := FALSE;
  (* dump trace of traced ref manipulations *)

  (* SPIN *)
  linkinfo_ok : BOOLEAN := TRUE;

  (* SPIN *)
  procinfo_ok : BOOLEAN := TRUE;


VAR (* runtime checks *)
  doNarrowChk : BOOLEAN := TRUE;
  doRangeChk  : BOOLEAN := TRUE;
  doReturnChk : BOOLEAN := TRUE;
  doCaseChk   : BOOLEAN := TRUE;
  doTCaseChk  : BOOLEAN := TRUE;
  doAsserts   : BOOLEAN := TRUE;
  doNilChk    : BOOLEAN := TRUE;
  doRaisesChk : BOOLEAN := TRUE;

VAR
  new_adr      : BOOLEAN := FALSE;  (* TRUE =>  "ADR (t: T): UNTRACED REF T" *)
  report_stats : BOOLEAN := FALSE;

PROCEDURE Initialize (READONLY options: ARRAY OF TEXT): BOOLEAN;

PROCEDURE OpenUnit (name: M3ID.T; interface, generic: BOOLEAN; push: BOOLEAN;
                    VAR(*OUT*) filename: TEXT): File.T;

PROCEDURE CloseFile (rd: File.T);

PROCEDURE PushEnvironment (file: M3Compiler.SourceFile);
PROCEDURE PopEnvironment ();

PROCEDURE FileTail (path: TEXT): TEXT;
(* returns the tail of 'path' *)

TYPE
  Phase = {None, InitBuiltins, Parsing, TypeChecking, EmittingCode};

VAR
  currentPhase := Phase.None;

PROCEDURE StartPhase (p: Phase);

END Host.

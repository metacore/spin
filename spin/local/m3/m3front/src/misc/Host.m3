(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: Host.m3                                               *)
(* Last modified on Mon Sep 26 08:16:37 PDT 1994 by kalsow     *)
(*      modified on Tue May 25 14:27:57 PDT 1993 by muller     *)

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
 *	add -RefCount
 *
 * 28-Jan-97  Marc Fiuczynski (mef) at the University of Washington
 *	Modification to turn off the generation of dynamic link
 *	information and type information for procedures.
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
 *	Whisted.  Previous SPIN changes: "-SpinRelax" option.
 *
 *)

MODULE Host;

IMPORT File, Text, (*ETimer, M3Timers,*) M3ID, M3Compiler;
IMPORT Error;

PROCEDURE Initialize (READONLY options: ARRAY OF TEXT): BOOLEAN =
  BEGIN
    verbose       := FALSE;
    errorDie      := -1;
    warnings      := 2;
    coverage      := FALSE;
    versionStamps := TRUE;
    emitBuiltins  := FALSE;
    init_floats   := FALSE;
    vs_debug      := FALSE;
    load_map      := TRUE;
    ext_direct    := TRUE;
    all_direct    := FALSE;
    stack_walker  := TRUE;
    nested_calls        := FALSE;
    nested_procs_first  := FALSE;
    inline_nested_procs := TRUE;
    clean_stores  := FALSE;
    clean_jumps   := TRUE;
    doNarrowChk   := TRUE;
    doRangeChk    := TRUE;
    doReturnChk   := TRUE;
    doCaseChk     := TRUE;
    doTCaseChk    := TRUE;
    doAsserts     := TRUE;
    doNilChk      := TRUE;
    doRaisesChk   := TRUE;
    new_adr       := FALSE;
    report_stats  := FALSE;

    FOR i := 0 TO LAST (options) DO
      IF NOT ProcessArg (options[i]) THEN RETURN FALSE; END;
    END;
    RETURN TRUE;
  END Initialize;

(*------------------------------------------------ command line arguments ---*)

PROCEDURE ProcessArg (t: TEXT): BOOLEAN =
  VAR key: TEXT;
  BEGIN
    IF (t = NIL) THEN RETURN TRUE; END;
    key := Text.Sub (t, 0, 2);
    IF (Text.Equal (t, "-v")) THEN
      verbose  := TRUE;
      vs_debug := TRUE;
      warnings := -1;
    ELSIF (Text.Equal (key, "-t")) THEN
      (* ignore the old -tTARGET option which is still in the config files  *)
    ELSIF (Text.Equal (t, "-g")) THEN
      (* generate debugging, which we always do anyway! *)
    ELSIF (Text.Equal (t, "-S")) THEN
      versionStamps := FALSE;   (* do not generate version stamps *)
    ELSIF (Text.Equal (t, "-w")) THEN
      warnings := 99;
    ELSIF (Text.Equal (key, "-w")) THEN
      warnings := GetInt (t, 2);
    ELSIF (Text.Equal (t, "-builtins")) THEN
      emitBuiltins := TRUE; (* emit the predefined scopes *)
    ELSIF (Text.Equal (t, "-Z")) THEN
      coverage := TRUE; (* generate line profiling *)
    ELSIF (Text.Equal (key, "-E")) THEN
      errorDie := GetInt (t, 2);
    ELSIF (Text.Equal (t, "-NoAsserts")) THEN
      doAsserts := FALSE;
    ELSIF (Text.Equal (t, "-NoNarrowChk")) THEN
      doNarrowChk := FALSE;
    ELSIF (Text.Equal (t, "-NoRangeChk")) THEN
      doRangeChk := FALSE;
    ELSIF (Text.Equal (t, "-NoReturnChk")) THEN
      doReturnChk := FALSE;
    ELSIF (Text.Equal (t, "-NoCaseChk")) THEN
      doCaseChk := FALSE;
    ELSIF (Text.Equal (t, "-NoTypecaseChk")) THEN
      doTCaseChk := FALSE;
    ELSIF (Text.Equal (t, "-NoNilChk")) THEN
      doNilChk := FALSE;
    ELSIF (Text.Equal (t, "-NoRaisesChk")) THEN
      doRaisesChk := FALSE;
    ELSIF (Text.Equal (t, "-NoChecks")) THEN
      doAsserts   := FALSE;
      doNarrowChk := FALSE;
      doRangeChk  := FALSE;
      doReturnChk := FALSE;
      doCaseChk   := FALSE;
      doTCaseChk  := FALSE;
      doNilChk    := FALSE;
      doRaisesChk := FALSE;
    ELSIF (Text.Equal (t, "-InitFloats")) THEN
      init_floats := TRUE;
    ELSIF (Text.Equal (t, "-load_map")) THEN
      load_map := TRUE;
    ELSIF (Text.Equal (t, "-No_load_map")) THEN
      load_map := FALSE;
    ELSIF (Text.Equal (t, "-No_stack_walker")) THEN
      stack_walker := FALSE;
    ELSIF (Text.Equal (t, "-externals_direct")) THEN
      ext_direct := TRUE;
    ELSIF (Text.Equal (t, "-externals_indirect")) THEN
      ext_direct := FALSE;
    ELSIF (Text.Equal (t, "-all_direct")) THEN
      all_direct := TRUE;
    ELSIF (Text.Equal (t, "-all_indirect")) THEN
      all_direct := FALSE;
    ELSIF (Text.Equal (t, "-nested_calls")) THEN
      nested_calls  := TRUE;
    ELSIF (Text.Equal (t, "-no_nested_calls")) THEN
      nested_calls  := FALSE;
    ELSIF (Text.Equal (t, "-nested_procs_first")) THEN
      (* nested_procs_first := TRUE; *)
      (* THIS IS BOGUS:  nested_procs_first => the nested procedure
         cannot reference local variables of its parent that
         were not declared at the outermost level.  *)
    ELSIF (Text.Equal (t, "-nested_procs_last")) THEN
      nested_procs_first := FALSE;
    ELSIF (Text.Equal (t, "-inline_nested_procs")) THEN
      inline_nested_procs := TRUE;
    ELSIF (Text.Equal (t, "-unfold_nested_procs")) THEN
      inline_nested_procs := FALSE;
    ELSIF (Text.Equal (t, "-clean_stores")) THEN
      clean_stores  := TRUE;
    ELSIF (Text.Equal (t, "-dirty_stores")) THEN
      clean_stores  := FALSE;
    ELSIF (Text.Equal (t, "-clean_jumps")) THEN
      clean_jumps   := TRUE;
    ELSIF (Text.Equal (t, "-dirty_jumps")) THEN
      clean_jumps   := FALSE;
    ELSIF (Text.Equal (t, "-vsdebug")) THEN
      vs_debug := TRUE;
    ELSIF (Text.Equal (t, "-new_adr")) THEN
      new_adr := TRUE;
    ELSIF (Text.Equal (t, "-stats")) THEN
      report_stats := TRUE;

    (* SPIN *)
    ELSIF (Text.Equal (t, "-NoLoopholeWarning")) THEN
      loophole_warning_off := TRUE;

    (* SPIN *)
    ELSIF (Text.Equal (t, "-RefCount")) THEN
      ref_count := TRUE;
      IF trace_ref THEN
        env.report_error (NIL, 0, "-TraceRef and -RefCount conflict");
      END;

    (* SPIN *)
    ELSIF (Text.Equal (t, "-ReadBarrier")) THEN
      IF trace_ref THEN
        env.report_error (NIL, 0, "-TraceRef and -ReadBarrier conflict");
      END;

    ELSIF (Text.Equal (t, "-WriteBarrier")) THEN
      IF trace_ref THEN
        env.report_error (NIL, 0, "-TraceRef and -WriteBarrier conflict");
      END;
      IF ref_count THEN
        env.report_error (NIL, 0, "-RefCount and -WriteBarrier conflict");
      END;
      
    (* SPIN *)
    ELSIF (Text.Equal (t, "-NoLinkInfo")) THEN
      linkinfo_ok := FALSE;
      
    (* SPIN *)
    ELSIF (Text.Equal (t, "-NoProcedureTypeInfo")) THEN
      procinfo_ok := FALSE;

    (* SPIN *)
    ELSIF (Text.Equal (t, "-SpinRelax")) THEN
      extern_ok := TRUE;
      method_ok := TRUE;

    (* SPIN *)
    ELSIF (Text.Equal (t, "-TraceRef")) THEN
      trace_ref := TRUE;
      IF ref_count THEN
        env.report_error (NIL, 0, "-TraceRef and -RefCount conflict");
      END;

    (* SPIN *)
    ELSIF (Text.Equal (t, "-NoFloat")) THEN
      no_float := TRUE;

    (* SPIN *)
    ELSIF (Text.Equal (t, "-VerboseChecks")) THEN
      verbose_checks := TRUE;

    ELSE
      env.report_error (NIL, 0, "m3c: unknown option, \"" & t & "\"");
      RETURN FALSE;
    END;
    RETURN TRUE;
  END ProcessArg;

PROCEDURE GetInt (t: TEXT;  start: INTEGER): INTEGER =
  VAR c: CHAR;  n: INTEGER := 0;
  BEGIN
    FOR j := start TO Text.Length (t)-1 DO
      c := Text.GetChar (t, j);
      IF (c < '0') OR ('9' < c) THEN RETURN n END;
      n := n * 10 + ORD (c) - ORD ('0');
    END;
    RETURN n;
  END GetInt;

(*-------------------------------------------------- misc file operations ---*)

TYPE
  OptionDesc = REF RECORD
    extern_ok : BOOLEAN;
    method_ok : BOOLEAN;
    ref_count : BOOLEAN;
    trace_ref : BOOLEAN;
    no_float  : BOOLEAN;
    prev      : OptionDesc;
  END;

VAR
  optionStack: OptionDesc := NIL;

PROCEDURE OpenUnit (name: M3ID.T; interface, generic: BOOLEAN; push: BOOLEAN;
                    VAR(*OUT*) filename: TEXT): File.T =
  VAR file: M3Compiler.SourceFile;
  BEGIN
    (* ETimer.Push (M3Timers.search); *)
    file := env.find_source (name, interface, generic);
    filename := file.name;
    (* ETimer.Pop (); *)

    IF push THEN
      PushEnvironment(file);
    END;

    RETURN file.contents;
  END OpenUnit;

PROCEDURE CloseFile (rd: File.T) =
  BEGIN
    IF (rd # NIL) THEN
      TRY rd.close () EXCEPT ELSE END;
    END;
  END CloseFile;

PROCEDURE PushEnvironment (file: M3Compiler.SourceFile) =
  BEGIN
    (* save the part of the state that may be overridden by compilation
       of imported interfaces, this must nest so we build a stack *)
    optionStack := NEW (OptionDesc, 
                        extern_ok := extern_ok,
                        method_ok := method_ok,
                        ref_count := ref_count,
                        trace_ref := trace_ref,
                        no_float  := no_float,
                        prev := optionStack);
    
    (* some flags were passed from the compilation for the library *)
    WITH flags = file.flags DO
      IF flags # NIL THEN
        FOR i := FIRST(flags^) TO LAST(flags^) DO
          IF NOT ProcessArg(flags[i]) THEN
            Error.Msg (
                "illegal options in link information for imported interface");
          END;
        END;
      END;
    END;
  END PushEnvironment;

PROCEDURE PopEnvironment () =
  BEGIN
    (* restore the part of the state saved while opening this file *)
    extern_ok := optionStack.extern_ok;
    method_ok := optionStack.method_ok;
    ref_count := optionStack.ref_count;
    trace_ref := optionStack.trace_ref;
    no_float  := optionStack.no_float;
    optionStack := optionStack.prev;
  END PopEnvironment;

PROCEDURE FileTail (path: TEXT): TEXT =
  VAR c: CHAR;
  BEGIN
    IF (path = NIL) THEN RETURN NIL END;

    (* search for the last slash or blank in the string *)
    FOR x := Text.Length (path) - 1 TO 0 BY -1 DO
      c := Text.GetChar (path, x);
      IF (c = '/') OR (c = ' ') OR (c = '\\') THEN
        RETURN Text.Sub (path, x+1);
      END;
    END;

    (* no slashes *)
    RETURN path;
  END FileTail;

CONST
  phaseMsg = ARRAY Phase OF TEXT {
    "Not started",
    "initializing builtins",
    "parsing",
    "type checking",
    "emitting code"
  };

PROCEDURE StartPhase (p: Phase) =
  BEGIN
    currentPhase := p;
    IF (verbose) THEN
      env.report_error (NIL, 0, phaseMsg[currentPhase] & "...");
    END;
  END StartPhase;

BEGIN
END Host.

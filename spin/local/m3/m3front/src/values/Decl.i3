(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Decl.i3                                               *)
(* Last modified on Tue Dec 20 14:43:48 PST 1994 by kalsow     *)
(*      modified on Fri Feb 23 03:42:15 1990 by muller         *)

(*
 * HISTORY
 * 23-May-96  Wilson Hsieh (whsieh) at the University of Washington
 *	support for functional
 *
 *)

INTERFACE Decl;

IMPORT M3, M3ID, CG;

TYPE 
  Attributes = RECORD 
      alias:       M3ID.T   := M3ID.NoID;
      isExternal:  BOOLEAN  := FALSE;
      isInline:    BOOLEAN  := FALSE;
      isUnused:    BOOLEAN  := FALSE;
      isObsolete:  BOOLEAN  := FALSE;
      isBounded:   BOOLEAN  := FALSE;
      isFunctional:BOOLEAN  := FALSE;
      isImplicit:  BOOLEAN  := FALSE;
      callingConv: CG.CallingConvention := NIL;
    END;


PROCEDURE Parse (interface, top_level: BOOLEAN;  VAR fails: M3.ExSet);

PROCEDURE ParseExternalPragma (VAR(*OUT*) alias : M3ID.T;
                               VAR(*OUT*) cc    : CG.CallingConvention);

END Decl.

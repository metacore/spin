(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Decl.m3                                               *)
(* Last modified on Tue Dec 20 14:54:22 PST 1994 by kalsow     *)
(*      modified on Sat Mar 16 01:56:20 1991 by muller         *)

(*
 * HISTORY
 * 09-Feb-97  Wilson Hsieh (whsieh) at the University of Washington
 *	made errors about external declarations more precise
 *
 * 23-May-96  Wilson Hsieh (whsieh) at the University of Washington
 *	support for functional
 *      reorganized implicit and bounded so that they are inside
 *        Attributes
 *
 * 24-Mar-96  Wilson Hsieh (whsieh) at the University of Washington
 *	fixed bug on Modula-3 web page
 *	CONST was incorrectly disallowed in EXTERNAL interfaces
 *
 * 08-Jan-96  Wilson Hsieh (whsieh) at the University of Washington
 *	support for parsing IMPLICIT
 *
 * 25-Oct-95  Wilson Hsieh (whsieh) at the University of Washington
 *	support for bounded
 *
 * 23-Oct-95  Wilson Hsieh (whsieh) at the University of Washington
 *	EXTERN only in UNSAFE modules
 *
 *)

MODULE Decl;

IMPORT M3, M3ID, M3String, Token, Error, ESet, Module, Exceptionz;
IMPORT Constant, Tipe, Variable, Procedure, Revelation, CG, Target;
IMPORT Host;
FROM Scanner IMPORT GetToken, Match, cur;

TYPE
  TK = Token.T;

PROCEDURE Parse (interface, top_level: BOOLEAN;  VAR fails: M3.ExSet) =
  VAR att: Attributes;
  BEGIN
    att.isInline    := FALSE;
    att.isExternal  := FALSE;
    att.isUnused    := FALSE;
    att.isObsolete  := FALSE;
    att.alias       := M3ID.NoID;
    att.callingConv := NIL;
    LOOP
      CASE cur.token OF
      | TK.tEXTERNAL =>
          (* External modules are not allowed *)
          IF NOT Module.IsInterface () THEN
            Error.Msg ("External declarations only allowed in interfaces");
          (* external interfaces must be unsafe in SPIN *)
          ELSIF (Module.IsSafe () AND NOT Host.extern_ok) THEN
            Error.Msg ("External declarations are unsafe");
          END;
          ParseExternalPragma (att.alias, att.callingConv);
          att.isExternal := TRUE;
      | TK.tINLINE   =>
          att.isInline := TRUE;
          GetToken (); (* INLINE *)
          Match (TK.tENDPRAGMA);
      | TK.tUNUSED   =>
          att.isUnused := TRUE;
          GetToken (); (* UNUSED *)
          Match (TK.tENDPRAGMA);
      | TK.tOBSOLETE =>
          att.isObsolete := TRUE;
          GetToken (); (* OBSOLETE *)
          Match (TK.tENDPRAGMA);
      | TK.tCALLCONV   =>
          att.callingConv := Target.FindConvention (M3ID.ToText (cur.id));
          GetToken (); (* convention name *)
          Match (TK.tENDPRAGMA);
      | TK.tBOUNDED =>
          att.isBounded := TRUE;
          GetToken (); (* EPHEMERAL *)
      | TK.tFUNCTIONAL =>
          att.isFunctional := TRUE;
          GetToken (); (* FUNCTIONAL *)
      | TK.tIMPLICIT =>
          att.isImplicit := TRUE;
          GetToken (); (* IMPLICIT *)
          Match (TK.tENDPRAGMA);
      ELSE EXIT;
      END;
    END;

    CASE cur.token OF
    | TK.tCONST =>
        att.isExternal := att.isExternal;
        Constant.ParseDecl (att);
    | TK.tTYPE =>
        Tipe.Parse (att);
    | TK.tVAR =>
        att.isExternal := att.isExternal OR Module.IsExternal ();
        Variable.ParseDecl (att);
    | TK.tPROCEDURE =>
        att.isExternal := att.isExternal OR Module.IsExternal ();
        Procedure.ParseDecl (att, interface);
    | TK.tREVEAL =>
        IF (NOT top_level) THEN Error.Msg ("nested revelation") END;
        Revelation.Parse (att);
    | TK.tEXCEPTION =>
        IF (NOT top_level) THEN Error.Msg ("nested exception declaration") END;
        att.isExternal := att.isExternal OR Module.IsExternal ();
        Exceptionz.ParseDecl (att);
    | TK.tFATAL =>
        fails := ESet.ParseFails (fails);
    ELSE 
        IF att.isInline OR att.isExternal OR att.isUnused
           OR att.isObsolete
           (* SPIN *)
           OR att.isBounded OR att.isFunctional OR att.isImplicit THEN
          Error.Msg ("declaration pragma not followed by a declaration");
        END;
    END;
  END Parse;

PROCEDURE ParseExternalPragma (VAR alias : M3ID.T;
                               VAR cc    : CG.CallingConvention) =
  BEGIN
    <* ASSERT cur.token = TK.tEXTERNAL *>
    GetToken (); (* EXTERNAL *)

    alias := M3ID.NoID;  (* default => use the Modula-3 name *)
    cc    := Target.DefaultCall;

    IF (cur.token = TK.tIDENT) OR (cur.token = TK.tTEXTCONST) THEN
      IF (cur.token = TK.tIDENT)
        THEN alias := cur.id;
        ELSE alias := M3ID.Add (M3String.ToText (cur.str));
      END;
      GetToken (); (* IDENT, TEXTCONST *)

      IF (cur.token = TK.tCOLON) THEN
        GetToken (); (* : *)
        IF (cur.token = TK.tIDENT) OR (cur.token = TK.tTEXTCONST) THEN
          cc := Target.FindConvention (M3ID.ToText (cur.id));
          IF (cc = NIL) THEN
            Error.ID (cur.id, "unsupported language or calling convention");
            cc := Target.DefaultCall;
          END;
          GetToken (); (* IDENT/TEXTCONST *)
        ELSE
          Error.Msg ("Missing language for <*EXTERNAL*> pragma");
        END;
      END;

    END;

    Match (TK.tENDPRAGMA);
  END ParseExternalPragma;

BEGIN
END Decl.

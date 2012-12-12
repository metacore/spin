(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Fri Aug  6 08:19:50 PDT 1993 by kalsow     *)
(*      modified on Thu Mar  7 03:13:05 1991 by muller         *)

(*
 * HISTORY						       
 * 17-Jun-97  Przemek Pardyak (pardy) at the University of Washington
 *	Unified SPIN M3 runtime: added InfoFromAddress.
 *)

UNSAFE MODULE RTModule;

IMPORT RT0, RT0u, RTMisc;

PROCEDURE Count (): CARDINAL =
  BEGIN
    RETURN RT0u.nModules;
  END Count;

PROCEDURE Get (m: CARDINAL): RT0.ModulePtr =
  VAR p := RT0u.modules;
  BEGIN
    IF (m >= RT0u.nModules) THEN
      RTMisc.FatalErrorI ("improper module index: ", m);
    END;
    p := p + m * ADRSIZE (RT0.ModulePtr);
    RETURN p^;
  END Get;

PROCEDURE InfoFromAddress(ptr: ADDRESS): RT0.ModulePtr =
  VAR
    p := RT0u.modules;
  BEGIN
    FOR i := 0 TO RT0u.nModules - 1 DO
      WITH pp = p^ DO
        IF pp < ptr AND ptr <= pp.file THEN
          RETURN pp;
        END;
      END;
      INC(p, ADRSIZE (RT0.ModulePtr));
    END;
    RETURN NIL;
  END InfoFromAddress; 

BEGIN
END RTModule.


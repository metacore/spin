(*| Copyright (C) 1990, Digital Equipment Corporation           *)
(*| All rights reserved.                                        *)
(*| See the file COPYRIGHT for a full description.              *)
(*| Last modified on Thu Jun  3 16:55:08 PDT 1993 by kalsow     *)
(*|      modified on Mon Feb 22 09:59:59 PST 1993 by jdd        *)
(*|      modified on Tue Oct  9 21:54:08 1990 by muller         *)

(*
 * HISTORY
 * 17-Jun-97  Przemek Pardyak (pardy) at the University of Washington
 *	Unified SPIN M3 runtime: Added GetByName(), GetByPC(), GetExported(),
 *	Reset(), CleanUp(), more information returned about procedures.
 *)

(* "RTProcedureSRC" is an implementation-dependent extension to
   "RTProcedure". *)

(* FIXME: simplify? *)

INTERFACE RTProcedureSRC;

FROM RTProcedure IMPORT Proc;
IMPORT RT0;

TYPE
  Name = ADDRESS;
  (* a C-style null terminated string *)

TYPE
  ProcInfo = RT0.ProcPtr;
  UnitInfo = RT0.ModulePtr;

PROCEDURE NumProcedures (): CARDINAL;
(* Returns the number of global procedures registered in the runtime. *)

PROCEDURE FromPC (pc: ADDRESS;  VAR p: Proc;  VAR end: ADDRESS;
                  VAR proc: ProcInfo; VAR unit: UnitInfo; VAR expo: UnitInfo;
                  VAR file, name: Name);
(* Returns in (p, name) the address and name of the procedure that seems to
   contain pc.  (i.e.  the first registered procedure before pc) Note that
   this procedure may require a linear search of the registered
   procedures. *)

TYPE
  ProcDesc = RECORD
    proc: PROCANY;
    end : ADDRESS;
    name: TEXT;
    file: TEXT;
    info: ProcInfo;
    unit: UnitInfo;
    expo: UnitInfo;
  END;

PROCEDURE GetByName (file: TEXT := NIL; 
                     name: TEXT := NIL): REF ARRAY OF ProcDesc;
(* Returns descriptors of all the procedures with the given name. 
   The name should include file name.  If both file and name are
   NIL then return all procedures *)

PROCEDURE GetByPC (pc: ADDRESS): REF ProcDesc;
(* Returns a descriptor of the procedure which contains given PC *)

PROCEDURE GetExported (interface: RT0.ModulePtr): REF ARRAY OF ProcDesc;
(* Returns a list of all procedures exported by this interface *)

PROCEDURE Reset ();
(* Reset the maps to enable adding new procedures. *)
  
PROCEDURE CleanUp ();
(* Clean up the maps. *)

END RTProcedureSRC.

(*
 * Copyright 1996, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
 * HISTORY
 * 12-Jun-97  Wilson Hsieh (whsieh) at the University of Washington
 *	fix comment
 *
 * 07-Aug-96  Marc Fiuczynski (mef) at the University of Washington
 *	Cleanup.
 *
 * 28-Oct-95  Brian Bershad (bershad) at the University of Washington
 *)

(* "SpinPublicExtern" defines the domain symbol and size
   of the publically available C symbols. *)

UNSAFE (* because it defines externals. *)
INTERFACE SpinPublicExtern;

(* These symbols are defined by 
   machine/$PLATFORM/sal/CoreSymbols.c *)

<*EXTERNAL "SpinPublicInterface__domainSymbols"*> 
VAR domainSymbols: ADDRESS;
<*EXTERNAL "SpinPublicInterface__domainSize"*> 
VAR domainSize: INTEGER;
END SpinPublicExtern.

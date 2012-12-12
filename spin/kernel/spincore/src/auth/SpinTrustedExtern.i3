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

(* "SpinTrustedExtern" defines the domain symbol and size
   of the C symbols only to be used by trusted entities. *)

UNSAFE (* because it defines externals. *)
INTERFACE SpinTrustedExtern;

(* These symbols are defined by 
   machine/$PLATFORM/sal/CoreSymbols.c *)

<*EXTERNAL "SpinTrustedInterface__domainSymbols"*>
VAR domainSymbols:ADDRESS;
<*EXTERNAL "SpinTrustedInterface__domainSize"*>
VAR domainSize:INTEGER;
END SpinTrustedExtern.

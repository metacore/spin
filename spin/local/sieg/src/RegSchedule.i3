(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 03-May-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

(* Schedules what register is used for what param. *)

INTERFACE RegSchedule;
IMPORT Type, Module;

PROCEDURE Scan(VAR m: Module.T);
(* Scan through the module "m", and assign registers to params in each proc. *)
 
PROCEDURE CalledByValueResult(READONLY m: Module.T; t: Type.T): BOOLEAN;
(* Returns "TRUE" if the VAR x : "T" is value-result parameter *)


END RegSchedule.


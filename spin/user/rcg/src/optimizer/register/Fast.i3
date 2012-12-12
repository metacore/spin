(*
 *
 * Copyright 1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 *)

(*
 * HISTORY
 *
 *)


INTERFACE Fast;

IMPORT Procedure;

FROM Analysis IMPORT Problem; (* exception *)

(* reallocate registers
 *)
PROCEDURE Reallocate (p: Procedure.T) RAISES {Problem};

END Fast.

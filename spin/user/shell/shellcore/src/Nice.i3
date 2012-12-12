(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created. Set up the priority for kernel shell execed threads.
 *)
INTERFACE Nice;

IMPORT Strand, ParseParams;

CONST CommandName = "nice";
CONST CommandHelp = " priority";

PROCEDURE Run(pp: ParseParams.T): BOOLEAN;

(*
 * Get current shell priority.
 *)
PROCEDURE Priority(): Strand.PriorityT;

END Nice.

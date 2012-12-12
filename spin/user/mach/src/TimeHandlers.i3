(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 *)
INTERFACE TimeHandlers;

IMPORT Strand, CPU;

VAR
  TimersEnabled : BOOLEAN := FALSE;

PROCEDURE TimeSemaWrapper(args: REFANY);

PROCEDURE MaintainTime(strand: Strand.T; VAR ms: CPU.SavedState);

PROCEDURE SetSoftclock(<* UNUSED *>strand: Strand.T; 
                       VAR ms: CPU.SavedState);

PROCEDURE PokeSoftclock(<* UNUSED *>strand: Strand.T; 
                        <* UNUSED *>VAR ms: CPU.SavedState);

PROCEDURE TimerInit(timer: INTEGER);

PROCEDURE TimerSample(timer: INTEGER; id: Strand.T);

PROCEDURE TimerPrint(timer: INTEGER);

END TimeHandlers.


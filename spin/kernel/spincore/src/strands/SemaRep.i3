(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created. Counting semaphores.
 *)
INTERFACE SemaRep;
IMPORT Sema, Strand;

REVEAL Sema.T = BRANDED REF RECORD
  val: INTEGER;
  list: Strand.T;
END;

END SemaRep.

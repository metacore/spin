(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 *
 * 27-Aug-97  Robert Grimm (rgrimm) at the University of Washington
 *      Removed Next() since it is unsafe.
 *
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created. Round-robin global scheduler.
 *)
INTERFACE Sched;
IMPORT Strand;

PROCEDURE GetPriority(s: Strand.T) : Strand.PriorityT;

PROCEDURE SetPriority(s: Strand.T; pri: Strand.PriorityT);

END Sched.

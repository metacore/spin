(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 13-Jul-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Routines to make fli realtime.
 *
 *)
INTERFACE RealTime; 

(*
 * Set the start time for the next activity.
 *)
PROCEDURE SetStartTime(starttime: INTEGER);

(*
 * Stall when done before the next deadline.
 *)
PROCEDURE WaitUntilStartTime();

END RealTime. 

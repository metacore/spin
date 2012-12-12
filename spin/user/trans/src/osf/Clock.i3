(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 16-Jan-97  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
INTERFACE Clock;
PROCEDURE SetAlarm(x: INTEGER; p: PROCEDURE(r: REFANY); r: REFANY);
  (* just a dummy. *)
END Clock.

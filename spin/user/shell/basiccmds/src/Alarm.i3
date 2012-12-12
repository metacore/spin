(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created. Test the alarm interface.
 *)
INTERFACE Alarm;

IMPORT ParseParams;

CONST CommandName = "alarm";
CONST CommandHelp = " #ticks";

PROCEDURE Run(pp: ParseParams.T) : BOOLEAN;

END Alarm.












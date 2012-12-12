(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created. Test for the alarm interface.
 *)
MODULE Alarm;
IMPORT Clock, Log, ParseParams;

PROCEDURE Alarm1(<*UNUSED*>arg: REFANY) = 
BEGIN
  Log.Log("Alarm went off at ");
  Log.Logi(Clock.ReadTicks());
  Log.Log(" ticks.\n");
END Alarm1;

PROCEDURE Run (pp: ParseParams.T): BOOLEAN =
  VAR ticks: INTEGER;
  BEGIN
    pp.reset();
    TRY
      pp.skipNext();
      ticks := pp.getNextInt();
      Log.Log("Alarm set at ");
      Log.Logi(Clock.ReadTicks());
      Log.Log(" ticks to expire in ");
      Log.Logi(ticks);
      Log.Log(" ticks.\n");
      Clock.SetAlarm(ticks, Alarm1, NIL);
      RETURN TRUE;
    EXCEPT
      ParseParams.Error => RETURN FALSE;
    END;
  END Run;

BEGIN
END Alarm.

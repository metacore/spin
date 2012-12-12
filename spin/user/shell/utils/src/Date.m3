(*
 * Copyright 1994, 1995, 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY 
 * 26-Sep-97  Yasushi Saito (yasushi) at the University of Washington
 *	Changed to use the new Commands module.
 * 19-Mar-97  Przemek Pardyak (pardy) at the University of Washington
 *	Reenabled.
 *
 * 28-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed TmToText to print out minutes and seconds more nicely.
 *
 * 02-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	Whisted.
 *)
MODULE Date;
IMPORT IO, Fmt, ParseParams, Clock;
IMPORT Commands;

PROCEDURE Run (<*UNUSED*>r: REFANY; <*UNUSED*>pp: ParseParams.T) : BOOLEAN =
  VAR 
    t: Clock.TimeVal;
  BEGIN
    Clock.TimeOfDay(t);
    IO.Put("The time is " & Fmt.Int(t.tv_sec) & "." & 
	   Fmt.Int(t.tv_usec) & "\n");
    RETURN TRUE;
  END Run;

BEGIN
  EVAL Commands.Install(Run, "date", "", "Display the current date and time.");
END Date.

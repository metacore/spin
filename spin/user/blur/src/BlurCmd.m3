(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 18-Jan-98  Tian Fung Lim (tian) at the University of Washington
 *	Front end for simple blur benchmark
 *
 *)
MODULE BlurCmd;
IMPORT Text, IO, ParseParams, Fmt;
IMPORT FPaint;

PROCEDURE Run (pp: ParseParams.T): BOOLEAN =
  VAR
    option     : TEXT;
    swapdevice : TEXT;
    livemem    := 8*1024*1024; 
    distx      := 16;
    disty      := 40960;
    iter       := 150;
  BEGIN
    pp.reset();
    TRY
      pp.skipNext(); (* skip command name *)
      IF pp.testNext("n") THEN
        iter := pp.getNextInt();
      END      ;

      IF pp.testNext("live") THEN
        livemem := pp.getNextInt();
      END;

      IF pp.testNext("dist") THEN
        distx := pp.getNextInt();
        disty := pp.getNextInt();
      END;

      IO.Put("Running "&Fmt.Int(iter)&" iterations with livemem "&
        Fmt.Int(livemem)&" distx "&Fmt.Int(distx)&" disty "&
        Fmt.Int(disty)&"\n");
      FPaint.Run(iter,livemem,distx,disty);
      
      RETURN TRUE;
    EXCEPT
      ParseParams.Error => IO.Put(CommandName & CommandHelp & "\n");
      RETURN FALSE;
    END;
  END Run;

BEGIN
END BlurCmd.

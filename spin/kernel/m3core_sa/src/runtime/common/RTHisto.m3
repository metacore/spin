(*
 * Copyright 1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 17-Nov-97  Tian Fung Lim (tian) at the University of Washington
 *	Created.  Pause histogram generator.
 *
 *
 *)

UNSAFE MODULE RTHisto;
IMPORT RTOSMachine;
FROM RTIO IMPORT PutText, PutInt;

PROCEDURE PutCR()=
  BEGIN
    PutText("\n");
  END PutCR;

PROCEDURE HistogramEnd() =
  BEGIN
      pause := RTOSMachine.CycleCounter();
      pause := RTOSMachine.CycleMinus(pause,pauseStart);
      pause := RTOSMachine.CycleToMicrosec(pause);
      pause := pause DIV 100;

      IF NormalPause # NIL THEN
        IF pause > LAST(NormalPause^) THEN
          pause := LAST(NormalPause^);
        END;
        IF pause > MaxIndex THEN
          MaxIndex := pause;
        END;
        INC(NormalPause[pause]);
      END;
      (*
        UNCOMMENT this and allocation of other arrays
        to distinguish different allocation types 
      CASE RTHisto.ALLOCTYPE OF
        RTHisto.AllocType.normal =>
        INC(NormalPause[pause]);
      | RTHisto.AllocType.mig =>
        INC(MigPause[pause  ]);
      | RTHisto.AllocType.gc =>
        INC(GCPause[pause  ]);
      END;
      *)

  END HistogramEnd;

PROCEDURE DumpHistogram()=
  BEGIN
    IF Histogram THEN
      PutText("\nus or more\t number of times \n");
      PutText("### BEGIN LATENCY\n");
      FOR i := 0 TO MaxIndex DO
        IF NormalPause[i] >0 THEN
          PutInt(i*100,12);
          PutInt(NormalPause[i],12);
          PutCR();
        END;
        (* UNCOMMENT for 3 histograms
        IF NormalPause[i] + MigPause[i] + GCPause[i] # 0 THEN
          PutInt(i*100);
          PutText("\t\t");
          PutInt(NormalPause[i]);
          PutText("\t");
          PutInt(MigPause[i]);
          PutText("\t");
          PutInt(GCPause[i]);
          PutText("\n");
        END;
        *)
      END;
      PutText("\n### END LATENCY\n");
    END;
  END DumpHistogram;


(* done in ResetStats *)
PROCEDURE ResetHistogram () = 
  VAR
  BEGIN
    IF Histogram THEN
      FOR i := 0 TO LAST(NormalPause^) DO
        NormalPause[i] := 0;
      END;
      IF MigPause # NIL THEN
        FOR i := 0 TO LAST(MigPause^) DO
          MigPause[i] := 0;
        END;

        FOR i := 0 TO LAST(GCPause^) DO
          GCPause[i] := 0;
        END;
      END;
    END;
    MaxIndex := 0;
  END ResetHistogram;


PROCEDURE Init() =
  BEGIN
  IF Histogram THEN
    NormalPause := NEW (UNTRACED REF ARRAY OF INTEGER, MaxGC DIV 100);
    (*
      MigPause := NEW (UNTRACED REF ARRAY OF INTEGER, MaxGC DIV 100 );
      GCPause := NEW (UNTRACED REF ARRAY OF INTEGER, MaxGC DIV 100);
    *)
    MaxIndex := 0;
    PutText("Histogram stats initialized\n");
  END;
  END Init;

BEGIN

END RTHisto.

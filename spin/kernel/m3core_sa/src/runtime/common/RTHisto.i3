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
INTERFACE RTHisto;

(* indicate to RTAllocator what kind of allocation this was *)
TYPE
  AllocType = {normal, mig, gc };
VAR
  ALLOCTYPE : AllocType;
  NormalPause : UNTRACED REF ARRAY OF INTEGER;
  MigPause    : UNTRACED REF ARRAY OF INTEGER;
  GCPause     : UNTRACED REF ARRAY OF INTEGER;
  LongPause   : UNTRACED REF ARRAY OF INTEGER;
  pauseStart : INTEGER;
  pause : INTEGER;
  timeStamp : INTEGER; 
  MaxIndex : INTEGER;

CONST 
  Histogram = FALSE;
  MaxPause = 1000000000;
  MaxNormal = 500;
  MaxMig = 4000;
  MaxGC  = 800000;

PROCEDURE HistogramEnd();
PROCEDURE DumpHistogram();
PROCEDURE ResetHistogram();
PROCEDURE Init();
END RTHisto.







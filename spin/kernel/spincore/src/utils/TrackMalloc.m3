(*
 * Copyright 1994, 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
 * HISTORY
 * 24-Feb-97  Marc Fiuczynski (mef) at the University of Washington
 *	Made TrackStrand conditional.
 *
 * ??-Nov-96  David Becker (becker) at the University of Washington
 *      Keep track of malloc usage per extension.
 *)

MODULE TrackMalloc;
IMPORT Wr, Fmt;
IMPORT Track;
IMPORT DebugOption, TrackStrand; <*NOWARN*>

REVEAL T = Track.T BRANDED OBJECT
		malloc, free: INTEGER;
		OVERRIDES
		print:=Print;
		END;

VAR
  bootMalloc, bootFree: INTEGER;
  badMalloc, badFree: INTEGER;
  init := FALSE;

PROCEDURE Print(self:T; wr:Wr.T) =
  BEGIN
    Wr.PutText(wr,"Track strand malloc.\n");
    Wr.PutText(wr,"malloc "&Fmt.Int(self.malloc)&"\n");
    Wr.PutText(wr,"free   "&Fmt.Int(self.free)&"\n");
  END Print;

PROCEDURE Malloc(bytes:INTEGER) =
  BEGIN
    IF init THEN
      INC(TrackStrand.Malloc().malloc, bytes);
    ELSE
      INC(bootMalloc,bytes);
    END;
  END Malloc;

PROCEDURE Free(bytes:INTEGER) =
  BEGIN
    IF init THEN
      INC(TrackStrand.Malloc().free, bytes);
    ELSE
      INC(bootFree,bytes);
    END;
  END Free;

PROCEDURE Init() =
  BEGIN
    INC(TrackStrand.Malloc().malloc, bootMalloc);
    INC(TrackStrand.Malloc().free, bootFree);
    init := TRUE;
  END Init;

BEGIN
END TrackMalloc.

(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 27-Jun-96  Marc Fiuczynski (mef) at the University of Washington
 *	Name clean up.
 *
 * 11-Mar-96  Emin Gun Sirer (egs) at the University of Washington
 *	Changed to a lock-free implementation.
 *
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created. Regions of code that need to be treated specially.
 *      Restartable atomic sequences and non-preemptible pieces of
 *      code require special treatment.
 *)
MODULE CodeRegions;
IMPORT Region, RegionArraySort, Word;
IMPORT Fmt;

REVEAL T = Public BRANDED "CodeRegions" OBJECT
    regionlist: REF ARRAY OF Region.T;
  OVERRIDES
    contains  := Contains;
    rasEnsure := RASEnsure;
    add       := Add;
    dump      := Dump;
    iterate   := Iterate;
  END;

PROCEDURE BinarySearch(self: T; 
                       VAR pc: Word.T;
                       arg: REFANY;
                       func: PROCEDURE (self: T;
                                        VAR pc: Word.T;
                                        regindex: INTEGER;
                                        arg: REFANY)) =
  VAR lo, hi, mid: INTEGER;
  BEGIN
    IF self.regionlist = NIL THEN
      RETURN;
    END;
    lo := FIRST(self.regionlist^); 
    hi := LAST(self.regionlist^);
    LOOP
      IF lo > hi THEN
        RETURN;
      END;
      mid := (lo + hi) DIV 2;
      IF Word.LT(pc, self.regionlist[mid].begin) THEN
        lo := mid + 1;
      ELSIF Word.LT(Word.Minus(pc, self.regionlist[mid].begin), 
                 self.regionlist[mid].length) THEN
        func(self, pc, mid, arg);
        (* We can only be in one region at any given time, *)
        (* since regions cannot be overlapping. *)
        RETURN;
      ELSE (* Word.GT(pc, self.regionlist[mid].begin) THEN *)
        hi := mid - 1;
      END;
    END;
  END BinarySearch; 

(* 
 * This call back function signals that it found the pc in a 
 * non-preemptive code region by setting it to zero. The caller
 * can then restore the pc to a sane value and take remedial
 * action.
 *)
PROCEDURE RangeUpdate(<*UNUSED*>self: T;
                      VAR pc: Word.T; 
                      <*UNUSED*>regindex: INTEGER;
                      <*UNUSED*>arg: REFANY) =
  BEGIN
    pc := 0;
  END RangeUpdate;

PROCEDURE RASUpdate(self: T; VAR pc: Word.T; regindex: INTEGER;<*UNUSED*>arg: REFANY) =
  BEGIN
    pc := self.regionlist[regindex].begin;
  END RASUpdate;

PROCEDURE Contains(self: T; VAR pc: Word.T) : BOOLEAN =
  VAR
    oldpc: Word.T := pc;
  BEGIN
    BinarySearch(self, pc, NIL, RangeUpdate);
    IF pc = 0 THEN
      pc := oldpc;
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END Contains; 

PROCEDURE RASEnsure(self: T; VAR pc: Word.T) = 
  BEGIN
    BinarySearch(self, pc, NIL, RASUpdate);
  END RASEnsure;

PROCEDURE Add(self: T; READONLY newregion: Region.T) =
  VAR
    newarray : REF ARRAY OF Region.T;
    size: CARDINAL;
  BEGIN
    IF self.regionlist # NIL THEN
      size := NUMBER(self.regionlist^);
    ELSE
      size := 0;
    END;
    newarray := NEW(REF ARRAY OF Region.T, size + 1);
    IF self.regionlist # NIL THEN
      SUBARRAY(newarray^, 0, size) := SUBARRAY(self.regionlist^, 0, size);
    END;
    newarray[size] := newregion;
    RegionArraySort.Sort(newarray^);
    self.regionlist := newarray;
  END Add;

PROCEDURE Dump(self: T) : TEXT =
  VAR str : TEXT := "";
  BEGIN
    FOR i := FIRST(self.regionlist^) TO LAST(self.regionlist^) DO
      str := str & "Region #" & Fmt.Int(i) &
                 " pc=0x" & Fmt.Unsigned(self.regionlist[i].begin) &
                 " len=0x" & Fmt.Unsigned(self.regionlist[i].length) & "\n";
    END;
    RETURN str;
  END Dump;

PROCEDURE Iterate(self: T;
                  processRegion:PROCEDURE(regno: INTEGER; READONLY region: Region.T)) =
  BEGIN
    FOR i := FIRST(self.regionlist^) TO LAST(self.regionlist^) DO
      processRegion(i, self.regionlist[i]);
    END;
  END Iterate;

PROCEDURE New() : T = 
  VAR ras: T;
  BEGIN
    ras := NEW(T);
    RETURN ras;
  END New;

BEGIN
END CodeRegions. 

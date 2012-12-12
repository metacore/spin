(*
 * Copyright 1994, 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
  	Spy.m3
  
   Spin system monitor

   Note for all the people who want to hack this module: All the procedures
   in this module must be VERY FAST. So don't insert any
   codes that slow down the execution unnecessarily. -yas.

   And also don't complicate the code. I don't understand what Move means.:(
   
 *)
(*
 * HISTORY
 * 10-May-97  Tian Fung Lim (tian) at the University of Washington
 *	Fixed percentage calculation.
 *
 * 14-Nov-96  Przemek Pardyak (pardy) at the University of Washington
 *	Made Spy-s untraced to be able to measure the collector.
 *
 * 14-Nov-96  Przemek Pardyak (pardy) at the University of Washington
 *	The current interface is too rigid.  Added alwaysActive and
 *	nested flags, and Move() and Reset() operations which where needed
 *	for measuring the collector. More flexibility and clean-up to come.
 *
 * 10-Oct-96  Przemek Pardyak (pardy) at the University of Washington
 *      Do not print error messages until initialized to avoid crashing
 *	if IO is not initialized yet.
 *
 * 03-May-96  David Becker (becker) at the University of Washington
 * 	Removed Limits() from init to quiet it down
 *
 * 02-Apr-96  Marc Fiuczynski (mef) at the University of Washington
 *	The Chain function produces a machine check when a NIL timer is
 *	passed in.  It now checks for NIL, but the semantics of the
 *	procedure might not be right.
 * 
 *      Increased the "MaxTimers" constant from 256 to 1024.
 *
 * 09-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	This file is initialized before the console writer, so it cannot use
 *	IO.Put.
 *
 * 27-Oct-95  Brian Bershad (bershad) at the University of Washington
 *	Fixed Up.
 *
 * 02-Mar-95  David Becker (becker) at the University of Washington
 *	Created.
 *
 *)
MODULE Spy;	
IMPORT IO, Text, Fmt;
IMPORT Word, Extern;
CONST
  MaxTimers = 1024;

REVEAL T = BRANDED REF RECORD
    label: TEXT;
    hits := 0; 
    time := 0; (* cycles *)
    start := 0; (* cycles *)
    min := LAST(INTEGER);
    max := FIRST(INTEGER);
    samples: REF ARRAY OF INTEGER;
  END;

VAR
  data : ARRAY [1..MaxTimers] OF T;
  alloc : INTEGER := FIRST(data);

PROCEDURE Create (label:TEXT; 
		  <*UNUSED*>nested: BOOLEAN;
		  nSamples: INTEGER := 0): T =
  VAR
    newt : T;	
  BEGIN
    IF alloc = MaxTimers THEN RETURN NIL END;

    newt := NEW(T, label := label);
    IF nSamples > 0 THEN
      newt.samples := NEW(REF ARRAY OF INTEGER, nSamples);
    END;
    data[alloc] := newt;
    INC(alloc);
    RETURN newt;
  END Create;
	
PROCEDURE Enter (timer:T) = 
  BEGIN
    IF timer = NIL THEN RETURN; END;

    timer.start := Extern.get_rpcc();
    IF timer.start = 0 THEN
      timer.start := 1;
    END;
  END Enter;
  
PROCEDURE Hit (timer:T; start, stop: INTEGER) =
  VAR
    delta: INTEGER;
  BEGIN
    start := Word.And(16_ffffffff,start);
    stop := Word.And(16_ffffffff,stop);
    IF start > stop THEN
      delta := 16_100000000-start+stop;
    ELSE
      delta := stop-start;
    END;
    IF timer.samples # NIL AND timer.hits < NUMBER(timer.samples^) THEN
      timer.samples[timer.hits] := delta;
    END;
    INC(timer.hits);
    INC(timer.time, delta);

    IF timer.min > delta THEN timer.min := delta; END;
    IF timer.max < delta THEN timer.max := delta; END;
  END Hit;

PROCEDURE Exit (timer:T) =
  VAR
    stop := Extern.get_rpcc();
  BEGIN
    IF timer = NIL THEN 
      RETURN;
    END;
  
    Hit(timer, timer.start, stop);
    timer.start := 0;
  END Exit;

PROCEDURE Clear (timer: T) =
  BEGIN
    timer.hits := 0;
    timer.time := 0;
    timer.start:= 0;
    timer.min  := LAST(INTEGER);
    timer.max  := 0;
  END Clear;
PROCEDURE PutInt (i: INTEGER; len:=0) =
  VAR t: TEXT;
  BEGIN
    IF len > 0 THEN
      t := Fmt.Pad(Fmt.Int(i), len);
    ELSE
      t := Fmt.Int(i);
    END;
    IO.Put(t);
  END PutInt;

PROCEDURE Median (t: T): INTEGER =
  VAR x, tmp: INTEGER;
  BEGIN
    IF t.samples = NIL THEN RETURN 0; END;
    FOR i := 1 TO t.hits-1 DO
      x := t.samples[i];
      FOR j := 0 TO i-1 DO
	IF x < t.samples[j] THEN
	  (* swap i and j. *)
	  tmp := t.samples[j];
	  t.samples[j] := x;
	  t.samples[i] := tmp;
	  EXIT;
	END;
      END;
    END;
    RETURN t.samples[t.hits DIV 2 + 1];
  END Median;
  
PROCEDURE DumpTime (entry:T) =
  VAR
    ave:INTEGER;
    percent:INTEGER;
    min:INTEGER;
    max:INTEGER;
  BEGIN
    percent := 0;
    
    IF entry.hits # 0 THEN
      ave := entry.time DIV entry.hits;
    ELSE
      ave := 0;
    END;
    IF entry.max # FIRST(INTEGER) THEN
      max := entry.max;
    ELSE
      max := 0;
    END;
    IF entry.min # LAST(INTEGER) THEN
      min := entry.min;
    ELSE
      min := 0;
    END;
    IF entry.label = NIL THEN
      IO.Put("<no-label>           "); 
    ELSE
      IO.Put(entry.label); 
      FOR i := 20 - Text.Length(entry.label) TO 0 BY -1 DO
        IO.Put(" ");
      END;
    END;
    IF percent >= 0 AND percent <= 100 THEN
      PutInt(percent, 5); IO.Put(" ");
    ELSE
      IO.Put("?");
      PutInt(percent, 3); IO.Put("?");
    END;
    PutInt(TicksToUsecs(min), 9); IO.Put(" ");
    PutInt(TicksToUsecs(max), 9); IO.Put(" ");
    PutInt(TicksToUsecs(ave), 9); IO.Put(" ");
    PutInt(TicksToUsecs(Median(entry)), 9); IO.Put(" ");
    PutInt(entry.hits, 9); IO.Put(" ");
    PutInt(TicksToUsecs(entry.time), 9); IO.Put("\n");

    IF FALSE AND entry.samples # NIL THEN
      IO.Put("samples: ");
      FOR i := 0 TO MIN(entry.hits-1, LAST(entry.samples^)) DO
	PutInt(entry.samples[i]);
	IO.Put("(");
	PutInt(TicksToUsecs(entry.samples[i]));
	IO.Put(") ");
      END;
      IO.Put(".\n");
    END;
  END DumpTime;

PROCEDURE Dump() = (* Pretty stats table *)
  BEGIN
    IO.Put("Timer                    %        Min       Max      Median    Avg      Hits  Total Usecs\n");  
    FOR i := FIRST(data) TO alloc-1 DO
      WITH entry = data[i] DO
        IF entry.hits # 0 THEN
          DumpTime(entry);
        END;
      END;
    END;
  END Dump;

PROCEDURE TicksToUsecs (cycles: INTEGER) : INTEGER = (* microseconds *)
  VAR 
    result   : INTEGER;
    tickRate : INTEGER;
  BEGIN
    tickRate := Extern.get_mhz();
    result  := cycles DIV tickRate;
    RETURN result;
  END TicksToUsecs;

PROCEDURE Reset () = (* resets all stats to zero *)
  BEGIN
    FOR i := FIRST(data) TO alloc-1 DO
      Clear(data[i]);
    END;
  END Reset; 

BEGIN
END Spy. 

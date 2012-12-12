(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 18-Jan-98  Tian Fung Lim (tian) at the University of Washington
 *	Whisted
 *
 *)
MODULE FPaint;

IMPORT IO, Fmt;

(* benchmark to test the collectors on a broad range of allocation
   sizes with different levels of live memory *)

CONST
  QUADSIZE = 234; (* 234 ints *)
  STROKEINC = 1000;

TYPE
  Quad = REF ARRAY OF INTEGER;

VAR
  Canvas : REF ARRAY OF Quad;

PROCEDURE Run(n : INTEGER; livemem : INTEGER; distx : INTEGER; disty : INTEGER) =
  VAR
    quads : INTEGER;
    currsize : INTEGER;
  BEGIN
    (* canvas is live memory in quadrants of 15000 bytes *)
    quads := livemem DIV (QUADSIZE*BYTESIZE(INTEGER));
    IO.Put("Initializing canvas.  Number of quads = "&Fmt.Int(quads)&"\n");

    Canvas := NEW(REF ARRAY OF Quad, quads);
    FOR i := 1 TO quads DO
      Canvas[i-1] := NEW (REF ARRAY OF INTEGER, QUADSIZE );
    END;
    
    (* now perform strokes of sizes ranging from distx to disty *)
    
    IO.Put("Striping...\n");
    FOR i := 1 TO n DO
      currsize := distx;
      WHILE currsize < disty DO
        Stroke(currsize);
        (*currsize := currsize * 2;*)
        INC(currsize, STROKEINC);
      END;
      (*IO.Put(Fmt.Int(i)&" ");*)
    END;
    FOR i := 1 TO quads DO
      Canvas[i-1] := NIL;
    END;
    Canvas := NIL;
  END Run;


(* create a "stroke" requiring size bytes to create a dup of the old
   information.  strokes are performed at powers of 2 increments of
   STROKESIZE .  at each increment, a new sction of the canvas must be
   saved, and added to the linked list.  at the end, the canvas is
   restored *)

CONST 
  STROKESIZE = 8;

TYPE
  buf = REF RECORD 
    data : REF ARRAY OF INTEGER;
    next : buf;
  END;

VAR
  SaveInfo : buf;

PROCEDURE Stroke (size:  INTEGER) =
  VAR
    s := 0;
    currstroke := STROKESIZE;
    temp : buf;
  BEGIN
    LOOP
      temp := NEW(buf);
      temp.data := NEW(REF ARRAY OF INTEGER, currstroke);
      temp.next := SaveInfo;
      SaveInfo := temp;

      (* copy old data, write new data *)
      FOR i:= 0 TO currstroke-1 DO
        temp.data[i]:= Canvas[(i DIV QUADSIZE) MOD LAST(Canvas^)][i MOD QUADSIZE];
        Canvas[(i DIV QUADSIZE) MOD LAST(Canvas^)][i MOD QUADSIZE] :=
            (Canvas[((i-1) DIV QUADSIZE) MOD LAST(Canvas^)][(i-1) MOD QUADSIZE] +
            (*Canvas[((i) DIV QUADSIZE) MOD LAST(Canvas^)][(i) MOD QUADSIZE] +*)
            Canvas[((i+1) DIV QUADSIZE) MOD LAST(Canvas^)][(i+1) MOD QUADSIZE]) DIV 2;
      END;

      INC(s,currstroke);
      currstroke := currstroke*2;


      IF s > size THEN
        EXIT;
      END;
    END;

    (* restore*)
    temp := SaveInfo;
    WHILE temp # NIL DO
      IF temp.data#NIL THEN
        FOR i:= 0 TO LAST(temp.data^) DO
          temp.data[i] := 0;
        END;
      END;
      temp.data := NIL;
      temp := temp.next;
    END;
    SaveInfo := NIL;
  END Stroke;

BEGIN
END FPaint.

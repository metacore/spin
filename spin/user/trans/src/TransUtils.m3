(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 10-Nov-96  Yasushi Saito (yasushi) at the University of Washington
 *	Added Warn
 * 26-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
MODULE TransUtils;
IMPORT IO;
IMPORT CPU;
IMPORT Debugger;

PROCEDURE Msg1 (a,b,c,d,e,f,g,h,i,j: TEXT) =
  BEGIN
    IO.Put("trans:");
    IF a # NIL THEN IO.Put(a & " "); END;
    IF b # NIL THEN IO.Put(b & " "); END;
    IF c # NIL THEN IO.Put(c & " "); END;
    IF d # NIL THEN IO.Put(d & " "); END;
    IF e # NIL THEN IO.Put(e & " "); END;
    IF f # NIL THEN IO.Put(f & " "); END;
    IF g # NIL THEN IO.Put(g & " "); END;
    IF h # NIL THEN IO.Put(h & " "); END;
    IF i # NIL THEN IO.Put(i & " "); END;
    IF j # NIL THEN IO.Put(j & " "); END;
  END Msg1;
  
PROCEDURE Msg (a,b,c,d,e,f,g,h,i,j: TEXT) =
  BEGIN
    Msg1(a,b,c,d,e,f,g,h,i,j);
    IO.Put("\n");
  END Msg;
  
  
PROCEDURE Warn (a,b,c,d,e, f, g, h, i, j: TEXT) =
  BEGIN
    Msg(a,b,c,d,e,f,g,h,i,j);
  END Warn;

PROCEDURE ValueIsNotPageAligned (p: INTEGER): BOOLEAN =
  BEGIN
    (* 0 is special. it is forgone. *)
    IF p = 0 THEN RETURN TRUE; END;
    IF p MOD CPU.PAGESIZE # 0 THEN
      RETURN TRUE;
    ELSE
      Debugger.Enter();
      RETURN FALSE;
    END;
  END ValueIsNotPageAligned;
  
BEGIN
  GatherStats := FALSE;
END TransUtils.

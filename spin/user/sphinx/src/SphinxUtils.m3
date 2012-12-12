(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 10-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
MODULE SphinxUtils;
(* IMPORT Fmt; *)
IMPORT IO;
IMPORT Proc, ProcRep;

PROCEDURE Msg (a,b,c,d,e: TEXT) =
  BEGIN
    IO.Put(ProcName(NIL) & ":");
    IF a # NIL THEN IO.Put(a & " "); END;
    IF b # NIL THEN IO.Put(b & " "); END;
    IF c # NIL THEN IO.Put(c & " "); END;
    IF d # NIL THEN IO.Put(d & " "); END;
    IF e # NIL THEN IO.Put(e & " "); END;
    IO.Put("\n");
  END Msg;
PROCEDURE ProcName (proc: Proc.T): TEXT =
  VAR
    name: TEXT;
  BEGIN
    IF proc = NIL THEN
      proc := Proc.Self();
    END;
    IF proc = NIL THEN
      name := "unknown";
    ELSE
      name := proc.print();
    END;
    RETURN name;
  END ProcName;

BEGIN
END SphinxUtils.

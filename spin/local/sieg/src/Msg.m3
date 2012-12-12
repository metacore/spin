(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
 * HISTORY
 * 28-Mar-96  Yasushi Saito (yasushi) at the University of Washington
 *	Changed to display src code location.
 * 03-Nov-95  Yasushi Saito (yasushi) at the University of Washington
 *	created.
 *)
MODULE Msg;
IMPORT IO, Stdio, M3Error, RTProcess;
IMPORT M3CSrcPos;

PROCEDURE PutText(READONLY pos : Pos; a,b,c,d,e : TEXT := NIL) =
VAR t := "";
BEGIN
  IF a # NIL THEN
    t := a;
  END;
  IF b # NIL THEN
    t := t & b;
  END;
  IF c # NIL THEN
    t := t & c;
  END;
  IF d # NIL THEN
    t := t & d;
  END;
  IF e # NIL THEN
    t := t & e;
  END;
  IF pos.pos = -1 THEN
    IO.Put(t & "\n", Stdio.stderr);
  ELSE
    IF pos.cu # NIL THEN 
      M3Error.SetCu(pos.cu);
    END;
    M3Error.ReportAtPos(pos.pos, t);
  END;
END PutText;

PROCEDURE Fatal (READONLY pos : Pos; a,b,c,d,e : TEXT := NIL) =
BEGIN
  PutText(pos, a, b, c, d, e);
  RTProcess.Exit(2);
END Fatal;


PROCEDURE Error (READONLY pos : Pos; a,b,c,d,e : TEXT := NIL) =
BEGIN
    PutText(pos, a, b, c, d, e);
    errorCode := 1;
END Error;

PROCEDURE Warn (READONLY pos : Pos; a,b,c,d,e : TEXT := NIL) =
BEGIN
    PutText(pos, a, b, c, d, e);
END Warn;

PROCEDURE Debug (READONLY pos : Pos; a,b,c,d,e : TEXT := NIL) =
BEGIN
  IF debuggingP THEN
    PutText(pos, a, b, c, d, e);
  END;
END Debug;
PROCEDURE Verbose (READONLY pos : Pos; a,b,c,d,e : TEXT := NIL) =
BEGIN
  Debug(pos, a, b, c, d, e);
END Verbose;

BEGIN
END Msg.

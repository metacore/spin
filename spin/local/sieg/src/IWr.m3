(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
 * HISTORY
 * 24-Nov-95  Yasushi Saito (yasushi) at the University of Washington
 *	changed block indentation to 2.
 *)

MODULE IWr;
IMPORT IO, Wr, Text, Fmt, Thread, Process;
IMPORT Msg;

REVEAL
  T = Public BRANDED OBJECT
    wr : Wr.T; (* real Output stream *)
    curIndent : CARDINAL; (* current amount of indentation *)
    lastCharWasEOL : BOOLEAN;
  OVERRIDES
    put := Put;
    f := F;
    indent := Indent;
    unindent := Unindent;
    close := Close;
  END;

(* Output a single text. *)
PROCEDURE PutSub (t : T; s : TEXT) RAISES {Thread.Alerted, Wr.Failure} =
  VAR sLen : INTEGER := Text.Length(s);
    ch : CHAR;
  BEGIN
      <* ASSERT s # NIL *>
      FOR i := 0 TO sLen-1 DO
	IF t.lastCharWasEOL THEN
	  (* indent the next line *)
	  FOR j := 1 TO t.curIndent DO
	    Wr.PutChar(t.wr, ' ');
	  END;
	  t.lastCharWasEOL := FALSE;
	END;
	
	ch := Text.GetChar(s, i);
	Wr.PutChar(t.wr, ch);
	IF ch = '\n' THEN t.lastCharWasEOL := TRUE; END;
      END;
  END PutSub;

PROCEDURE Put(t : T; a, b, c, d, e, f, g, h : TEXT := NIL) =
  BEGIN
    TRY 
      IF a # NIL THEN PutSub(t, a); END;
      IF b # NIL THEN PutSub(t, b); END;
      IF c # NIL THEN PutSub(t, c); END;
      IF d # NIL THEN PutSub(t, d); END;
      IF e # NIL THEN PutSub(t, e); END;
      IF f # NIL THEN PutSub(t, f); END;
      IF g # NIL THEN PutSub(t, g); END;
      IF h # NIL THEN PutSub(t, h); END;
      Wr.Flush(t.wr);
    EXCEPT
    | Thread.Alerted, Wr.Failure =>
      IO.Put("*** write failed.\n");
      Process.Exit(1);
    END;
  END Put;

PROCEDURE F (t: T; fmt: TEXT; a, b, c, d, e: TEXT := NIL) =
  BEGIN
    PutSub(t, Fmt.F(fmt, a, b, c, d, e));
    Wr.Flush(t.wr);
  END F;
  
PROCEDURE Indent(t : T) =
  BEGIN
    INC(t.curIndent, 2);
  END Indent;

PROCEDURE Unindent(t : T) =
BEGIN
    DEC(t.curIndent, 2);
END Unindent;

PROCEDURE Close(t : T) =
BEGIN
  TRY
    Wr.Close(t.wr);
  EXCEPT
  | Thread.Alerted, Wr.Failure =>
    IO.Put("*** close failed.\n");
    Process.Exit(1);
  END;
END Close;

PROCEDURE OpenWrite(f : TEXT) : T =
VAR
    t : T;
BEGIN
    t := NEW(T);
    t.wr := IO.OpenWrite(f);
    t.curIndent := 0;
    IF t.wr = NIL THEN
	Msg.Fatal(Msg.POS_VOID, f, " : can't open this one.\n");
    END;
    Msg.Verbose(Msg.POS_VOID, "opening ", f, ".\n");
    RETURN t;
END OpenWrite;

BEGIN
END IWr.

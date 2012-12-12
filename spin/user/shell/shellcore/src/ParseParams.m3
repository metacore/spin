(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)


(* HISTORY
 * 07-Mar-97  Wilson Hsieh (whsieh) at the University of Washington
 *	removed warnings
 *
 * 19-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	added functional versions of methods
 *
 * 20-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	Borrowed from m3 distribution. Sank FloatMode.
 *      Add reset. Make keywordPresent non-mutating.
 *
 *)

(* Last modified on Mon Jan 30 15:01:03 PST 1995 by kalsow     *)
(*      modified on Fri Sep  2 03:32:09 PDT 1994 by stolfi     *)
(*      modified on Wed Mar 13 01:30:31 1991 by muller         *)
(*      modified on Fri Jun  2 18:25:43 1989 by ellis          *)

MODULE ParseParams;

IMPORT Text, Wr, Fmt, Scan, Lex, Thread (*,Params , FloatMode;*);

REVEAL
  T = Public BRANDED OBJECT
        wr: Wr.T;                (* Writer for error messages *)
      OVERRIDES
        init            := Init;
        keywordPresent  := KeywordPresent;
        fkeywordPresent := FKeywordPresent;
        getKeyword      := GetKeyword;
        getNext         := GetNext;
        peekNext        := PeekNext;
        skipNext	:= SkipNext;
        testNext        := TestNext;
        ftestNext       := FTestNext;
        getNextInt      := GetNextInt;
        error           := PrintError;
        skipParsed      := SkipParsed;
        finish          := Finish;
        reset           := Reset;
        dup	        := Dup;
      END;

PROCEDURE Init(t: T; wr: Wr.T; argc: CARDINAL; argv: REF ARRAY OF TEXT): T =
  BEGIN
    t.wr := wr;
    WITH num = argc DO
      t.arg := NEW(REF ARRAY OF TEXT, num);
      t.parsed := NEW(REF ARRAY OF BOOLEAN, num);
      WITH a = t.arg^, p = t.parsed^ DO
        FOR i := 0 TO num-1 DO 
          a[i] := argv[i]; p[i] := FALSE
        END;
	p[0] := FALSE;
	t.next := 0;
      END;
    END;
    RETURN t
  END Init;

PROCEDURE KeywordPresent(t: T; key: TEXT): BOOLEAN =
  BEGIN
    WITH a = t.arg^, p = t.parsed^ DO
      FOR i := 0 TO LAST(a) DO
        IF NOT p[i] AND Text.Equal(key, a[i]) THEN
          t.next := i + 1;
          p[i] := TRUE;
          RETURN TRUE;
        END
      END
    END;
    RETURN FALSE
  END KeywordPresent;

FUNCTIONAL PROCEDURE FKeywordPresent(t: T; key: TEXT): BOOLEAN =
  (* same as KeywordPresent, but FUNCTIONAL *)
  BEGIN
    WITH a = t.arg^, p = t.parsed^ DO
      FOR i := 0 TO LAST(a) DO
        IF NOT p[i] AND Text.Equal(key, a[i]) THEN
          RETURN TRUE;
        END
      END
    END;
    RETURN FALSE;
  END FKeywordPresent;

PROCEDURE GetKeyword(t: T; key: TEXT) RAISES {Error} =
  BEGIN
    IF NOT t.keywordPresent(key) THEN
      t.error("keyword \"" & key & "\" not found.");
    END;
  END GetKeyword;

PROCEDURE GetNext(t: T): TEXT RAISES {Error} =
  BEGIN
    WITH a = t.arg^, p = t.parsed^ DO
      IF (t.next > LAST(a)) OR p[t.next] THEN
        t.error("missing argument after argument " &
          Fmt.Int(t.next-1) & " = \"" &
          a[t.next-1] & "\"."
	)
      END;
      p[t.next] := TRUE;
      INC(t.next);
      RETURN a[t.next-1]
    END;
  END GetNext;

(*
PROCEDURE OldPeekNext (t: T): TEXT RAISES {Error} =
  VAR n: TEXT;
  BEGIN
    WITH a = t.arg^, p = t.parsed^ DO
      IF (t.next > LAST(a)) OR p[t.next] THEN
        t.error("missing argument after argument " &
          Fmt.Int(t.next-1) & " = \"" &
          a[t.next-1] & "\"."
	)
      END;
      p[t.next] := TRUE;
      INC(t.next);
      RETURN a[t.next-1]
    END;
    n := t.getNext();
    DEC(t.next);
    t.parsed^[t.next] := FALSE;
    RETURN n;
  END OldPeekNext;
*)

PROCEDURE PeekNext (t: T): TEXT =
  BEGIN
    WITH a = t.arg^, p = t.parsed^ DO
      IF (t.next > LAST(a)) OR p[t.next] THEN
        RETURN NIL;
      END;
      RETURN a[t.next]
    END;
  END PeekNext;


PROCEDURE SkipNext(t: T) RAISES {Error} =
  BEGIN
    EVAL t.getNext();
  END SkipNext;

    
PROCEDURE Dup(t: T): T =
  VAR argv: REF ARRAY OF TEXT;
      cnt: CARDINAL := 0;
  BEGIN
    WITH a = t.arg^, p = t.parsed^ DO
      argv := NEW(REF ARRAY OF TEXT, NUMBER(a));
      FOR i := 0 TO LAST(a) DO
        IF NOT p[i] THEN
          argv[cnt] := a[i];
          INC(cnt);
        END;
      END;
    END;
    RETURN NEW(T).init(t.wr, cnt, argv);
  END Dup;
    

PROCEDURE TestNext (t: T; key: TEXT): BOOLEAN RAISES {} =
  BEGIN
    WITH a = t.arg^, p = t.parsed^ DO
      IF (t.next > LAST(a)) OR p[t.next] OR NOT Text.Equal(key, a[t.next]) THEN
        RETURN FALSE;
      ELSE
        p[t.next] := TRUE;
        INC(t.next);
        RETURN TRUE;
      END
    END
  END TestNext;


FUNCTIONAL PROCEDURE FTestNext (t: T; key: TEXT): BOOLEAN =
  (* same as TestNext, but FUNCTIONAL *)
  BEGIN
    WITH a = t.arg^, p = t.parsed^ DO
      IF (t.next > LAST(a)) OR p[t.next] OR NOT Text.Equal(key, a[t.next]) THEN
        RETURN FALSE;
      ELSE
        RETURN TRUE;
      END
    END
  END FTestNext;


PROCEDURE GetNextInt(
    t: T; 
    min := FIRST(INTEGER);  
    max := LAST(INTEGER)
  ): INTEGER RAISES {Error} =
  VAR nn: INTEGER;
  BEGIN
    WITH txt = t.getNext() DO
      TRY
        nn := Scan.Int(txt);
      EXCEPT 
        Lex.Error (*, FloatMode.Trap *)=>
          t.error(
	    "parameter " & Fmt.Int(t.next-1) & " = \"" & txt &
	    "\" should be an integer."
	  )
      END;
      IF (nn < min) OR (nn > max) THEN
        t.error (
	  "parameter " & Fmt.Int(t.next-1) & " = " & Fmt.Int(nn) &
          " should be in [" & Fmt.Int(min) & ".." & Fmt.Int(max) & "]."
	)
      END;
    END;
    RETURN nn
  END GetNextInt;


PROCEDURE SkipParsed(t: T) RAISES {Error} =
  CONST MaxBogus = 5;
  VAR bogus: CARDINAL := 0;
  BEGIN
    WITH a = t.arg^, p = t.parsed^ DO
      t.next := NUMBER(a);
      WHILE (t.next > 0) AND NOT p[t.next-1] DO DEC(t.next) END;
      (* Check for unparsed arguments: *)
      FOR i := 0 TO t.next-1 DO
        IF NOT p[i] THEN
          INC (bogus);
          IF bogus <= 5 THEN
            Message(
	      t.wr,
	      "parameter " & Fmt.Int(i) & " = \"" & a[i] & 
	      "\" extraneous or misplaced."
	    );
          END;
        END;
      END;
      IF bogus > MaxBogus THEN
        Message(t.wr, "(and " & Fmt.Int (bogus - MaxBogus) & " more).");
      END;
      IF bogus > 0 THEN RAISE Error END;
    END
  END SkipParsed;

PROCEDURE Finish(t: T) RAISES {Error} =
  CONST MaxBogus = 99;
  VAR bogus: CARDINAL := 0;
  BEGIN
    WITH a = t.arg^, p = t.parsed^ DO
      FOR i := 0 TO LAST(a) DO
        IF NOT p[i] THEN
          INC (bogus);
          IF bogus <= 5 THEN
            Message(
	      t.wr,
	      "parameter " & Fmt.Int(i) & " = \"" & a[i] & 
	      "\" extraneous or misplaced."
	    );
          END;
        END;
      END;
      IF bogus > MaxBogus THEN
        Message(t.wr, "(and " & Fmt.Int (bogus - MaxBogus) & " more).");
      END;
      (*
      t.parsed := NIL;
      t.arg := NIL;
      t.wr := NIL;
      *)
      IF bogus > 0 THEN RAISE Error END;
    END
        
  END Finish;

PROCEDURE Reset (t: T) =
  BEGIN
    (* Mark all arguments as unparsed! *)
    FOR i := FIRST(t.parsed^) TO LAST(t.parsed^) DO t.parsed^[i] := FALSE; END;
    t.next := 0;
  END Reset;
  


PROCEDURE Message(wr: Wr.T; msg: TEXT) =
  <*FATAL Wr.Failure, Thread.Alerted*>
  BEGIN
    IF (wr # NIL) THEN
      Wr.PutText(wr, "ParseParams: ");
      Wr.PutText(wr, msg);
      Wr.PutChar(wr, '\n');
      Wr.Flush(wr);
    END
  END Message;

PROCEDURE PrintError (t: T; msg: TEXT) RAISES {Error} =
  BEGIN
    Message(t.wr, msg);
    RAISE Error
  END PrintError;

BEGIN
END ParseParams.


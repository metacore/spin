(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 13-May-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
MODULE Pragma;
IMPORT M3CPragma;
IMPORT Module, AST, Text;

(* Extract a body from the "pragma" *)
PROCEDURE Parse (pragma : M3CPragma.T; directive : TEXT) : TEXT =
  VAR
    pragmaBody := M3CPragma.Body(pragma);
    pat := "<*" & directive;
    patLen := Text.Length(pat);
    val : TEXT;
  BEGIN
    IF Text.Length(pragmaBody) >= patLen + 2
      AND Text.Equal(Text.Sub(pragmaBody, 0, patLen), pat) THEN
      (* Skip spaces *)
      WHILE Text.GetChar(pragmaBody, patLen) = ' ' DO
	INC(patLen);
      END;
      
      val := Text.Sub(pragmaBody, patLen);
      (* cut off "*>" *)
      val := Text.Sub(val, 0, Text.Length(val)-2);
      RETURN val;
    END;
    RETURN NIL;
  END Parse;

PROCEDURE Find (READONLY m : Module.T; p : AST.NODE; directive : TEXT) : TEXT =
  VAR 
    iter : M3CPragma.Iter;
    pragma : M3CPragma.T;
  BEGIN
    IF p # NIL THEN
      (* Look for pragma just before "p" *)
      iter := M3CPragma.BeforeNode(m.pragmas, p);
      WHILE M3CPragma.Next(iter, pragma) DO
	IF M3CPragma.FollowingNode(pragma) = p THEN
	  WITH val = Parse(pragma, directive) DO
	    IF val # NIL THEN RETURN val; END;
	  END;
	END;
      END;
    ELSE
      (* Look for pragma appearing anywhere in the interface *)
      iter := M3CPragma.NewIter(m.pragmas);
      WHILE M3CPragma.Next(iter, pragma) DO
	WITH val = Parse(pragma, directive) DO
	  IF val # NIL THEN RETURN val; END;
	END;
      END;
    END;
    
    RETURN NIL;
  END Find;
  
BEGIN
END Pragma.

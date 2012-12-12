(*
 * HISTORY
 * 09-Nov-95  Yasushi Saito (yasushi) at the University of Washington
 *	created.
 *)
(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
MODULE Module;
IMPORT Text;
IMPORT Declaration;

PROCEDURE Lookup(READONLY m : T; READONLY name : TEXT;
		 VAR ref : Declaration.T) : BOOLEAN =
BEGIN
  FOR i := 0 TO m.names.size()-1 DO
    WITH decl = m.names.get(i) DO
      IF Text.Equal(decl.name, name) THEN
	ref := decl;
	RETURN TRUE;
      END;
    END;
  END;
  RETURN FALSE;
END Lookup;

PROCEDURE LookupProc(READONLY m : T; id : INTEGER;
		     VAR body : Declaration.Proc) : BOOLEAN =
  BEGIN
    FOR i := 0 TO m.names.size()-1 DO
      WITH genericDecl = m.names.get(i) DO
	TYPECASE genericDecl OF
	| Declaration.Proc(decl) =>
	  IF decl.id = id THEN
	    body := decl;
	    RETURN TRUE;
	  END;
	ELSE
	END;
      END;
    END;
    RETURN FALSE;
  END LookupProc;
BEGIN
END Module.

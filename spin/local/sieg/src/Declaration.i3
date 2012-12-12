(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)

(*

 Declaration holds an association between a name and an entity.
 Entity is either a constant(CONST decl), a type(TYPE decl), or
 procedure(PROCEDURE decl). 
 *)

INTERFACE Declaration;
IMPORT Type AS BaseType;

TYPE
  T = OBJECT
    name : TEXT; (* TYPE, PROCEDURE name *)
  END;

  Const = T OBJECT
    value : TEXT;
  END;

  Type = T OBJECT
    type : BaseType.T;
  END;

  Proc = T OBJECT
    proc : BaseType.Proc;
    id : INTEGER; (* ID for this proc, used in table jump *)
    userSideName : ARRAY [0 .. 3] OF TEXT; (* proc can have at most 4
					    synonyms *)
  END;
    

    
CONST Brand = "Declaration";
END Declaration.

(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *)
INTERFACE Error;


TYPE T <: Public;
  Public = OBJECT METHODS
    init(code : INTEGER): T := Init;
    resultCode(): INTEGER;
    message(): TEXT;
  END;

EXCEPTION E(T);

PROCEDURE Init(e : Public; code : INTEGER): T ;
  
END Error.

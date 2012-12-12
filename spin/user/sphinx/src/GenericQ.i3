(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *)
INTERFACE GenericQ;

TYPE T = OBJECT
  prev, next : T;
END;
CONST Brand = "GenericQ";
  
END GenericQ.

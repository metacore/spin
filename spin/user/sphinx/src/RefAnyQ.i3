(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *)
INTERFACE RefAnyQ;

TYPE T = OBJECT
  prev, next : T;
  data : REFANY;
END;
CONST Brand = "RefAnyQ";

END RefAnyQ.

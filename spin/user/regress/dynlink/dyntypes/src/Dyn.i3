(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *
 * 27-Dec-95  Przemek Pardyak (pardy) at the University of Washington
 *	Created. Regression test.
 *
 *)

INTERFACE Dyn;
IMPORT DynTypes;

PROCEDURE NarrowTest(): BOOLEAN;
PROCEDURE TypecaseTest(): BOOLEAN;

TYPE
  T = DynTypes.OT1 OBJECT 
    f2: INTEGER;
  METHODS
    m3(i: INTEGER): INTEGER;
  END;
  
  OT1 <: T;
  
END Dyn.





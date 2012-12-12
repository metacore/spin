(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 16-Jan-97  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
INTERFACE Spy;
TYPE T <: REFANY;
PROCEDURE Create(name: TEXT; b1: BOOLEAN; nsamples := 0): T;
PROCEDURE Enter(t: T);
PROCEDURE Exit(t: T);
PROCEDURE Reset();
PROCEDURE Dump();
  
END Spy.

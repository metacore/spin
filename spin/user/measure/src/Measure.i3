(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 15-Nov-95  Emin Gun Sirer (egs) at the University of Washington
 *	Added the Hit function which allows samples to be manually added.
 *
 * 01-Aug-95  Stefan Savage (savage) at the University of Washington
 *	Created to match timing functions used for OSF/1 and Mach in
 *	SOSP paper.
 *)
INTERFACE Measure;

TYPE
  T <: Public;
  Public = OBJECT
    METHODS
      init(name: TEXT; count: INTEGER): T;
      hit(t: INTEGER);
      start();
      stop();
    END;
  Histogram = REF ARRAY OF INTEGER;

PROCEDURE PrintStats(timer: T);

CONST
  Brand = "Measure";

END Measure. 

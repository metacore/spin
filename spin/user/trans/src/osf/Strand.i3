(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *)
INTERFACE Strand;
IMPORT Thread;
TYPE T = Thread.T;
PROCEDURE GetCurrent(): T;
PROCEDURE Yield();  
END Strand.

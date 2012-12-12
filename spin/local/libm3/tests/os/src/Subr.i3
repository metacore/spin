(* Copyright 1993 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Tue Jun  1 14:26:38 PDT 1993 by mcjones *)
 
INTERFACE Subr;

IMPORT OSError, Pathname, RefList, Sx;

EXCEPTION Usage(TEXT);

TYPE T = PROCEDURE(args: RefList.T)
  : Sx.T RAISES {Usage, OSError.E, Pathname.Invalid};

END Subr.

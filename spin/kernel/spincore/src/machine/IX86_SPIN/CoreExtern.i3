(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 16-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Created from Alpha version.
 *
 *) 
UNSAFE (* to define externals *)
INTERFACE CoreExtern;

(* "CoreExtern" defines the interface to the interrupt management
   code.  Misuse of this interface can cause unchecked runtime errors,
   hence it needs to be unsafe. *)

(* non-preemptible SPIN core *)
<* EXTERNAL *> PROCEDURE corebegin();
<* EXTERNAL *> PROCEDURE coreend();
(* "corebegin" and "coreend" are not actual procedures.  
   They are used to demarcate the spin core code region. *)

END CoreExtern.

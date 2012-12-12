(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 29-Nov-95  Emin Gun Sirer (egs) at the University of Washington
 *	Added VThread to support Mach synch primitives.
 *      Added equal, hash and brand to allow tables of semaphores.
 *
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created. Counting semaphores.
 *)
INTERFACE Sema;
IMPORT Strand;

(* Counting semaphores *)
TYPE T <: REFANY;
EXCEPTION InUse;
CONST Brand = "Semaphores";

(* Semaphores can be V'ed from interrupt level *)

PROCEDURE Alloc(initval: INTEGER := 0) : T;
PROCEDURE Dealloc(sema: T) RAISES {InUse};

PROCEDURE Reset(sema: T; val: INTEGER := 0) RAISES {InUse};

PROCEDURE P(sema: T);
PROCEDURE V(sema: T);
PROCEDURE Broadcast(sema: T);

(* support for signalling a particular thread waiting on a semaphore *)
PROCEDURE VThread(sema: T; s: Strand.T);

(* for use with tables *)
PROCEDURE Equal(a, b: T) : BOOLEAN;
PROCEDURE Hash(a: T) : INTEGER;

END Sema.

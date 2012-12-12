(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 13-Nov-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

(* Manages the binding between transaction group and sphinx process. *)

INTERFACE TransProc;
IMPORT TransGroup;
IMPORT Proc;
IMPORT Error;
IMPORT Transaction;

(* For various silly reasons, we do our own externalized ref management.
   "MaxObjects" is the max # of ext. ref per group. *)

CONST MaxObjects = 128;
TYPE ExtRef = [0 .. MaxObjects-1];
  
TYPE
  T <: TPublic;
  TPublic = TransGroup.T OBJECT
  (*
     "curTrans" is used to identify the currently active transaction
     and is used in page fault trapping.
     
     We now support only one transaction at a time.
     If you begin transaction while another transaction is still active,
     then "curTrans" is just overwritten.

     XXX we should hide the implementation.
  *)
    curTrans: Transaction.T;
    proc: Proc.T;
    obj: ARRAY ExtRef OF REFANY; (* extern ref table. *)
  END;

PROCEDURE Intern(proc: Proc.T): T RAISES {Error.E};
(* Find or create "T" corresponding to the process "proc". *)

PROCEDURE Self(): T;

PROCEDURE AddObject(t: T; obj: REFANY): ExtRef;
PROCEDURE UnaddObject(t: T; e: ExtRef);
  
PROCEDURE GetSphinxProc(t: T): Proc.T;
(* Opposite of "Intern" *)


END TransProc.

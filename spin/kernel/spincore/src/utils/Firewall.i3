(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 27-Apr-96  Stefan Savage (savage) at the University of Washington
 *	Added extra args to Firewall.Fault so we can better debug 
 *
 * 28-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Generalized the fault handling support.  Clients pass in the type
 *	of SpinException that occured.
 *
 * 06-Oct-95  Emin Gun Sirer (egs) at the University of Washington
 *	Copyright.
 *
 * 27-Jul-95  Brian Bershad (bershad) at the University of Washington
 *	Created
 *
 * This interface is responsible for isolating client failures from system
 * code.
 *)
INTERFACE Firewall;
IMPORT Strand, CPU, Word, SpinException;

(* A runtime protection fault within the kernel transfers here.  Clean
 * occurs according to the id of the faulting thread.  *)

PROCEDURE Fault(s: Strand.T; VAR ss: CPU.SavedState; badAddr: Word.T;
  arg1: Word.T; arg2: Word.T; ec: SpinException.ExceptionCode) RAISES ANY;

PROCEDURE Init(verbose: BOOLEAN);
VAR debug: BOOLEAN;	(* set TRUE for more noise during error situations *)

END Firewall.


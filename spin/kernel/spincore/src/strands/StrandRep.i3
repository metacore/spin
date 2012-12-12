(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *
 * 01-Aug-97  Robert Grimm (rgrimm) at the University of Washington
 *      Changed security information to fit new security manager.
 *
 * 04-Jun-97  Yasushi Saito (yasushi) at the University of Washington
 *	Added translation and interruptable fields.
 *
 * 07-Mar-97  Robert Grimm (rgrimm) at the University of Washington
 *      Removed Identity and added SecurityContext.
 *
 * 22-Nov-96  becker at the University of Washington
 *	Added TrackStrand.
 *
 * 05-Oct-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Strand implementation specifics.
 *)

INTERFACE StrandRep;

IMPORT Strand, SecurityContext, Ctypes;
IMPORT TrackStrand; <* NOWARN *>
IMPORT CPU;
IMPORT Translation;

REVEAL Strand.T = Strand.PublicT BRANDED OBJECT
  prev, next : Strand.T := NIL;     (* for the scheduler *)
  syncnext: Strand.T := NIL;        (* for fast sync ops *)
  count: INTEGER := 0;              (* suspend counts *)
  bound_to_kernel: Strand.T := NIL; (* holds the identity of the kthread *)
                                    (* that is doing work on our behalf. *)
  bound_to_user: Strand.T := NIL;   (* holds the identity of the user thread *)
                                    (* on whose behalf we are in the kernel. *)
  dbgnext, dbgprev: Strand.T := NIL;(* for the debugger *)
  tid: Ctypes.unsigned_int;         (* for the debugger *)
  context : SecurityContext.ContextT  := NIL; (* Per-user context information *)
  sid     : SecurityContext.SIDStackT := NIL; (* Per-thread subject+object SID*)
  trapFrame: UNTRACED REF CPU.SavedState; (* cpu state of this strand *)
  translation: Translation.T; (* Address space on which this thread
				 is supposed to attached to. NIL by default. *)
  track	: TrackStrand.T;     (* resource tracking *)
END;

  
END StrandRep.

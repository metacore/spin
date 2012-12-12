(* 
 * Copyright 1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * Security Context
 *
 *   Encapsulates functionality for strands to correctly handle
 *   security information (i.e., the per-user context and the per-thread
 *   stack of subject and object SIDs).
 *
 *   This interface must not be domain exported,
 *   since it can really wreck havoc!
 *
 *   On a historical note, this interface subsumes the Identity interface,
 *   though the public functionality is now (as of 8/1/97) part of the
 *   security manager interface.
 *
 * HISTORY
 *
 * 01-Aug-97  Robert Grimm (rgrimm) at the University of Washington
 *      Rewritten to fit into the new security manager.
 *      
 * 07-Mar-97  Robert Grimm (rgrimm) at the University of Washington
 *      Cleaned up, added DomainListT, moved implementation into DTE.m3.
 *
 * 10-Feb-97  Robert Grimm (rgrimm) at the University of Washington
 *      Created.
 *
 *)

INTERFACE SecurityContext;

IMPORT Strand;

TYPE
  ContextT  <: REFANY;   (* Per-user context information *)
  SIDStackT <: ADDRESS;  (* Per-thread SID information   *)

VAR
  currentSid : SIDStackT := NIL;
  (* Pointer to SID information of currently running thread. *)

PROCEDURE InitSIDStack( VAR new : SIDStackT; old : SIDStackT );
  (*
     Initialize the SID stack of a new kernel thread
     to a copy of the top of the old thread.
   *)

PROCEDURE CollectSIDStack( VAR stack : SIDStackT );
  (* Collect all elements from the SID stack of a kernel thread. *)

PROCEDURE Tazify( s : Strand.T );
  (*
     Tazify the original strand.
     Only to be called once in Sched.m3 !
   *)

PROCEDURE HandOffContext( user, kernel : Strand.T );
  (*
     When entering the kernel,
     fix the user thread security info if necessary
     and set the kernel thread security info to a copy of it,
     which effectively hands off the security context
     from the user to the kernel thread.
   *)

END SecurityContext.

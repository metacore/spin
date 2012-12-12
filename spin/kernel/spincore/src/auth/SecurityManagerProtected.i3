(* 
 * Copyright 1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * SecurityManager
 *    The secondary public interface to dynamic security management;
 *    supports general security discovery and enforcement.
 *
 * HISTORY
 * 30-Jul-97  Robert Grimm (rgrimm) at the University of Washington
 *      Created
 *
 * html
 *)

INTERFACE SecurityManagerProtected;

IMPORT Strand, SecurityError, AccessMode, SecurityManager;

(*
         Context and user/group id management
    (context management is on a per-session basis,
     as contexts are shared by threads spawned by
                  the initial thread)
   ------------------------------------------------
 *)

PROCEDURE GetProperty( s : Strand.T ; name: TEXT; remove: BOOLEAN := FALSE)
  : REFANY;
  (*
     Get the property value associated with name,
     return NIL if no such property exists,
     destroy property if remove is TRUE.
   *)

PROCEDURE SetProperty( s : Strand.T ; name: TEXT; property: REFANY ) : REFANY;
  (*
     Associate the specified property value with name.
     Return old value if already exists.
   *)

PROCEDURE GetUid( s : Strand.T ) : SecurityManager.UID;
  (* Get user id *)

(*
    Security ID Management
   ------------------------
 *)

PROCEDURE GetSubjectSid( s : Strand.T ) : SecurityManager.SID;
  (* Get subject security id. *)

PROCEDURE GetObjectSid( s : Strand.T ) : SecurityManager.SID;
  (* Get default object security id. *)

PROCEDURE SetObjectSid( s : Strand.T; sid : SecurityManager.SID )
  RAISES { SecurityError.T };
  (*
     Set default object security security id to "sid".
     Raise "SecurityError.EC.Unauthorized"
     if subject is unauthorized for this "sid".
     Note that the subject security id is implicitly managed
     by the security manager and can not be changed.
   *)

(*
    Mediation
   -----------
 *)

PROCEDURE Mediate(      k1, k2 : SecurityManager.SID;
                   VAR   s, o  : SecurityManager.SID  ) : AccessMode.T;
  (*
     Given the two security identifiers "k1" and "k2",
     return the currently legal access mode,
     as well as the subject and object sids.
   *)

(*
    Utilities
   -----------
 *)

PROCEDURE InheritContext( s : Strand.T );
  (*
     To be used by extension thread packages:
     If the strand s has no security context or SID stack,
     inherit from current one.
   *)

END SecurityManagerProtected.

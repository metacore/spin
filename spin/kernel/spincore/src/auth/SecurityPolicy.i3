(* 
 * Copyright 1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * SecurityPolicy
 *    Interface to the security policy server.
 *
 * HISTORY
 *
 * 07-Jan-98  Robert Grimm (rgrimm) at the University of Washington
 *      Added auditing.
 *
 * 30-Jul-97  Robert Grimm (rgrimm) at the University of Washington
 *      Created
 *
 *)

INTERFACE SecurityPolicy;

IMPORT Dispatcher, SecurityError, AccessMode, SecurityManager;

CONST
  Brand = "SecurityPolicy";

(*
    Mediation
   -----------
 *)

PROCEDURE Mediate(     k1, k2    : SecurityManager.SID;
                   VAR   mode    : AccessMode.T;
                   VAR   s, o    : SecurityManager.SID;
                   VAR   timeout : INTEGER;             ) : BOOLEAN;
  (*
     Given the two security identifiers "k1" and "k2",
     return the valid access mode "mode",
     as well as new SIDs for security domain changes,
     where "s" is the new subject SID
     and "o" is the new default object SID.
     The result can be cached inside the security manager
     iff "Mediate" returns TRUE.
     If the result can be cached and "timeout" is greater 0,
     the cache entry has to be removed after "timeout" ticks.
     Note, that "s" and "o" are meaningful
     iff both "k1" and "k2" are subject SIDs.
   *)

PROCEDURE MediateObjectDefault( k1, k2 : SecurityManager.SID;
                                VAR legal   : BOOLEAN;
                                VAR timeout : INTEGER;        ) : BOOLEAN;
  (*
     Given the the security id "k1" of a subject,
     determine whether "k2" is a "legal" default security id for objects.
     The result can be cached inside the security manager
     iff "MediateObjectDefault" returns TRUE.
     If the result can be cached and "timeout" is greater 0,
     the cache entry has to be removed after "timeout" ticks.
   *)

(*
    Users & Groups
   ----------------
 *)

PROCEDURE GetUserName ( uid : SecurityManager.UID ) : TEXT
  RAISES { SecurityError.T };
  (* Turn "uid" into human-readable form; raise if invalid id. *)

PROCEDURE GetGroupName( gid : SecurityManager.GID ) : TEXT
  RAISES { SecurityError.T };
  (* Turn "gid" into human-readable form; raise if invalid id. *)

PROCEDURE GetGroups(     uid     : SecurityManager.UID;
                     VAR groups  : REF ARRAY OF SecurityManager.GID;
                     VAR timeout : INTEGER; )
  : BOOLEAN RAISES { SecurityError.T };
  (*
     Get groups user "uid" is a member in.
     The result can be cached inside the security manager
     iff "GetGroups" returns TRUE.
     If the result can be cached and "timeout" is greater 0,
     the cache entry has to be removed after "timeout" ticks.
     Raise if invalid id.
   *)

(*
    Security IDs
   --------------
 *)

PROCEDURE GetSidName( sid : SecurityManager.SID ) : TEXT
  RAISES { SecurityError.T };
  (* Turn "sid" into human-readble form; raise if invalid id. *)

(*
     Auditing
    ----------
 *)

TYPE
  AuditStatusT = {
    FailedEnterCode,   (* Did not call proc due to check on code.         *)
    FailedEnterNIL,    (* Did not call proc due to check on NIL argument. *)
    FailedEnterObject, (* Did not call proc due to check on argument.     *)
    NormalEnter,       (* Did call proc.                                  *)
    FailedExitNIL,     (* Failed exit check on NIL argument.              *)
    FailedExit,        (* Failed exit check on argument.                  *)
    NormalExit };      (* Normal exit from proc.                          *)

  AuditRecordT = RECORD
    subject : SecurityManager.SID; (* A thread with security id     *)
    user    : SecurityManager.UID; (* User id for thread            *)
    binding : Dispatcher.Binding;  (* calls on the binding          *)
    time    : INTEGER;             (* at this time (in Clock ticks) *)
    status  : AuditStatusT;        (* with that status.             *)
  END;

  AuditLogT = REF ARRAY OF AuditRecordT; (* An audit log *)

PROCEDURE FlushAuditLog( log : AuditLogT; length : INTEGER );
  (*
     The security manager notifies the security policy that it
     needs to clear the audit log buffer "log" whose first "length"
     entries are valid.
   *)

(*
    Initialization
   ----------------
 *)

PROCEDURE Init( verbose : BOOLEAN := FALSE );
  (* Initialize security policy server. *)

END SecurityPolicy.

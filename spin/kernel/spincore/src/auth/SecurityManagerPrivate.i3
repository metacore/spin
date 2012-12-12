(* 
 * Copyright 1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * SecurityManagerPrivate
 *    The private, trusted interface to dynamic security management.
 *    Mostly for providing the security policy server control over
 *    the security manager.
 *
 * HISTORY
 *
 * 07-Jan-98  Robert Grimm (rgrimm) at the University of Washington
 *      Added auditing.
 *
 * 03-Dec-97  Robert Grimm (rgrimm) at the University of Washington
 *      Bug fixes and more features.
 *
 * 27-Oct-97  Robert Grimm (rgrimm) at the University of Washington
 *      Changed SetAccessControl and GetAccessControl.
 *
 * 30-Jul-97  Robert Grimm (rgrimm) at the University of Washington
 *      Created.
 *
 *)

INTERFACE SecurityManagerPrivate;

IMPORT RTType, Wr, Dispatcher, SecurityError, AccessMode, SecurityManager;

(*
    Access Control and Binding Information
   ----------------------------------------
 *)

TYPE
  AccessCheckT = {
  DomainTransfer, (* Change security domain.                                *)
  CodeCheck,      (* Check that subject can execute code.                   *)
  ObjectCheck,    (* Check that subject can execute operation on arguments. *)
  ResultCheck,    (* Check that subject can accept result.                  *)
  ExceptCheck,    (* Check that subject can access exception argument.      *)
  Audit           (* Generate audit record.                                 *)
  };

  AccessControlT  = SET OF AccessCheckT;

PROCEDURE SetAccessControl( binding     : Dispatcher.Binding;
                            kind        : AccessControlT;
                            executeMode : AccessMode.T     := NIL;
                            inModes     : AccessMode.ListT := NIL;
                            resultMode  : AccessMode.T     := NIL;
                            exceptMode  : AccessMode.T     := NIL; )
  RAISES { SecurityError.T };
  (*
     Impose access control of sort "kind" onto "binding".

     Raises "SecurityError.EC.InvalidBinding" if "binding" is illegal.
     Raises "SecurityError.EC.InvalidCheck" if "kind" is illegal for
     "binding". E.g., an access mode is provided for a non-reference
     argument (see below).

     If "kind" includes "AccessCheckT.CodeCheck", "executeMode"
     determines the required access mode for the code itself.

     If "kind" includes "AccessCheckT.ObjectCheck",
     . "inModes" denotes the required access modes for each argument
       in order of the procedure arguments, to be checked before the
       procedure is executed.
     If "kind" includes "AccessCheckT.ResultCheck",
     . "resultMode" denotes the required access mode for the result,
       to be checked after the procedure is executed.
     If "kind" includes "AccessCheckT.ExceptCheck",
     . "exceptMode" denotes the required access mode for exception
       arguments, to be checked when an exception is raised.
     Note that
     . there is no list of access modes for outgoing arguments (i.e.,
       for VARs), since VARs rely on shared memory with no implicit
       concurrency control (a return value is only visible to the
       caller, after the call completes).
     If the access mode for a given object is NIL, no access check is
     performed for this argument. Access modes for non-referenced
     arguments must be NIL (since they can not be associated with
     security identifiers).
     If "executeMode", "inModes", "resultMode", or "exceptMode is NIL,
     but has been non-NIL on a prior call to "SetAccessControl", the
     old access modes are used. As a result, the policy server can
     disable and enable access control checks without providing access
     modes, once the access modes have been established with the
     security manager.
   *)

PROCEDURE GetAccessControl(     binding     : Dispatcher.Binding;
                            VAR executeMode : AccessMode.T;
                            VAR inModes     : AccessMode.ListT;
                            VAR resultMode  : AccessMode.T;
                            VAR exceptMode  : AccessMode.T;       )
  : AccessControlT;
  (* Returns the current sort of access control checks on "binding". *)

PROCEDURE SetTypeSecurity( tc : RTType.Typecode; secure : BOOLEAN );
  (*
     Set security attribute of type with typecode "tc" to "secure".
     If a type is secure, all instances are associated
     with a security identifier on instance creation.
     Raises "SecurityError.EC.InvalidType" if "tc" does not denote
     a reference type.
   *)

PROCEDURE GetTypeSecurity( tc : RTType.Typecode ) : BOOLEAN;
  (* Get security attribute of type with typecode "tc". *)


(*
    Object Security Control
   -------------------------
 *)

PROCEDURE SetSid( o : REFANY; sid : SecurityManager.SID )
  RAISES { SecurityError.T };
  (*
     Set the security identifier associated with object "o".
     Raise "SecurityError.EC.InvalidType" if the objects type
     is insecure. This procedure is part of the
     "SecurityManagerPrivate" interface and should only be used
     by a trusted service that maps objects to security identifiers,
     possibly at a fine granularity than the default object security
     identifier provides.
   *)


(*
    Cache Control
   ---------------
 *)

PROCEDURE ClearMediationCache();
  (* Clear the entire mediation cache. *)

PROCEDURE ClearMediationCacheEntry( k1, k2 : SecurityManager.SID ) : BOOLEAN;
  (*
     Delete entry for the pair of SIDs "k1", "k2" from mediation cache.
     Returns true iff such entry was in the cache.
   *)

PROCEDURE SetMediationCacheSize( size1, size2 : INTEGER )
  RAISES { SecurityError.T };
  (*
     Set size of mediation cache to be the product of "size1" and "size2".
     Both numbers must be powers of two
     and represent the hash-size per security id
     for the two security ids used on each lookup.
     Raises "SecurityError.EC.BadNumber" if numbers are not powers of two.
   *)

PROCEDURE ClearObjectCache();
  (* Clear the object default security id cache. *)

PROCEDURE ClearObjectCacheEntry( k1, k2 : SecurityManager.SID ) : BOOLEAN;
  (*
     Delete entry for the pair of SIDs "k1", "k2"
     from object default security id cache.
     Returns true iff such entry was in the cache.
   *)

PROCEDURE SetObjectCacheSize( size : INTEGER ) RAISES { SecurityError.T };
  (*
     Set "size" of object default security id cache,
     where "size" is a power of two.
     Raises "SecurityError.EC.BadNumber" if "size" is not a power of two.
   *)

PROCEDURE ClearGroupCache();
  (* Clear the user to groups cache. *)

PROCEDURE ClearGroupCacheEntry( uid : SecurityManager.UID ) : BOOLEAN;
  (*
     Delete entry for the user id from groups cache.
     Returns true iff such entry was in the cache.
   *)

PROCEDURE SetGroupCacheSize( size : INTEGER ) RAISES { SecurityError.T };
  (*
     Set "size" of groups cache, where "size" is a power of two.
     Raises "SecurityError.EC.BadNumber" if "size" is not a power of two.
   *)


(*
    Auditing
   ----------
 *)

PROCEDURE SetAuditLogSize( size : INTEGER ) RAISES { SecurityError.T };
  (* Set "size" (which must be greater 0) of audit log. *)

PROCEDURE GetAuditLogSize() : INTEGER;
  (* Get size of audit log. *)


(*
    Initialization
   ----------------
 *)

PROCEDURE Init( verbose : BOOLEAN := FALSE );
  (*
     Initialize the security manager at boot time.
     Must be called before the SPIN threads and scheduler are activated.
     This call is currently located in "kernel/start/src/Boot.m3".
   *)

PROCEDURE Reset();
  (*
     Reset all state within the security manager.
     To be called when a new security policy is installed,
     to clear the state of the previous policy.
   *)


(*
    State Information 
   -------------------
 *)

PROCEDURE CheckTypeSecurity() : BOOLEAN;
  (* Returns "TRUE" iff type security is activated. *)


(*
    Debugging
   -----------
 *)

PROCEDURE DumpCurrentSIDStack( wr : Wr.T := NIL );
  (* Print debugging information about the current security state. *)

END SecurityManagerPrivate.

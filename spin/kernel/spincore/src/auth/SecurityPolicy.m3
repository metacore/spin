(* 
 * Copyright 1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * SecurityPolicy
 *   Dumb, simpleton, default security policy.
 *
 * HISTORY
 * 02-Aug-97  Robert Grimm (rgrimm) at the University of Washington
 *      Created
 *
 *)

MODULE SecurityPolicy;

IMPORT IO, SecurityError, AccessMode, SecurityManager;

VAR
  noAccess   : AccessMode.T;
  fullAccess : AccessMode.T;

(*
    Mediation
   -----------
 *)

PROCEDURE Mediate(   k1, k2    : SecurityManager.SID;
                   VAR mode    : AccessMode.T;
                   VAR s, o    : SecurityManager.SID;
                   VAR timeout : INTEGER;             ) : BOOLEAN =
  BEGIN
    IF k1 = SecurityManager.TazSid THEN
      IF    k2 = SecurityManager.TazSid      THEN
        mode := fullAccess;
        s    := SecurityManager.TazSid;
        o    := SecurityManager.TazDataSid;
      ELSIF k2 = SecurityManager.AnonSid     THEN
        mode := fullAccess;
        s    := SecurityManager.AnonSid;
        o    := SecurityManager.AnonDataSid;
      ELSIF k2 = SecurityManager.TazDataSid  THEN
        mode := fullAccess;
      ELSIF k2 = SecurityManager.AnonDataSid THEN
        mode := fullAccess;
      ELSE
        mode := noAccess;
      END;
    ELSIF k1 = SecurityManager.AnonSid THEN
      IF    k2 = SecurityManager.AnonSid     THEN
        mode := fullAccess;
        s    := SecurityManager.AnonSid;
        o    := SecurityManager.AnonDataSid;
      ELSIF k2 = SecurityManager.AnonDataSid THEN
        mode := fullAccess;
      ELSE
        mode := noAccess;
      END;
    ELSE
      mode := noAccess;
    END;
    timeout := 0;
    RETURN TRUE;
  END Mediate;

PROCEDURE MediateObjectDefault(    k1, k2   : SecurityManager.SID;
                                VAR legal   : BOOLEAN;
                                VAR timeout : INTEGER ) : BOOLEAN =
  BEGIN
    legal := (     k1 = SecurityManager.TazSid
               AND (    k2 = SecurityManager.TazDataSid
                     OR k2 = SecurityManager.AnonDataSid ) )
             OR
             (     k1 = SecurityManager.AnonSid
               AND k2 = SecurityManager.AnonDataSid );
    timeout := 0;
    RETURN TRUE;
  END MediateObjectDefault;

(*
    Users & Groups
   ----------------
 *)

PROCEDURE GetUserName ( uid : SecurityManager.UID ) : TEXT
  RAISES { SecurityError.T } =
  BEGIN
    IF uid = SecurityManager.TazUid THEN
      RETURN "Taz";
    ELSIF uid = SecurityManager.AnonUid THEN
      RETURN "Anon";
    ELSE
      RAISE SecurityError.T(SecurityError.EC.InvalidId);
    END;
  END GetUserName;

PROCEDURE GetGroupName( gid : SecurityManager.GID ) : TEXT
  RAISES { SecurityError.T } =
  BEGIN
    IF gid = SecurityManager.AllUsersGid THEN
      RETURN "Everyone";
    ELSE
      RAISE SecurityError.T(SecurityError.EC.InvalidId);
    END;
  END GetGroupName;

PROCEDURE GetGroups(     uid     : SecurityManager.UID;
                     VAR groups  : REF ARRAY OF SecurityManager.GID;
                     VAR timeout : INTEGER; )
  : BOOLEAN RAISES { SecurityError.T } =
  BEGIN
    IF uid = SecurityManager.TazUid OR uid = SecurityManager.AnonUid THEN
      groups    := NEW( REF ARRAY OF SecurityManager.GID, 1 );
      groups[0] := SecurityManager.AllUsersGid;
      timeout   := 0;
      RETURN TRUE;
    ELSE
      RAISE SecurityError.T(SecurityError.EC.InvalidId);
    END;
  END GetGroups;

(*
    Security IDs
   --------------
 *)

PROCEDURE GetSidName( sid : SecurityManager.SID ) : TEXT
  RAISES { SecurityError.T } =
  BEGIN
    IF    sid = SecurityManager.TazSid      THEN
      RETURN "Taz";
    ELSIF sid = SecurityManager.TazDataSid  THEN
      RETURN "TazData";
    ELSIF sid = SecurityManager.AnonSid     THEN
      RETURN "Anon";
    ELSIF sid = SecurityManager.AnonDataSid THEN
      RETURN "AnonData";
    ELSE
      RAISE SecurityError.T(SecurityError.EC.InvalidId);
    END;
  END GetSidName;

(*
     Auditing
    ----------
 *)

PROCEDURE FlushAuditLog( <*UNUSED*> log    : AuditLogT;
                         <*UNUSED*> length : INTEGER    ) =
  (*
     The security manager notifies the security policy that it
     needs to clear the audit log buffer "log" whose first "length"
     entries are valid.
   *)
  BEGIN
    (* The simple policy just ignores the audit log. *)
  END FlushAuditLog;

(*
    Initialization
   ----------------
 *)

VAR once : BOOLEAN := FALSE;

PROCEDURE Init( verbose : BOOLEAN := FALSE ) =
  BEGIN
    IF once THEN RETURN; END;
    once := TRUE;
    noAccess   := AccessMode.Create( AccessMode.NoSimpleAccess   );
    fullAccess := AccessMode.Create( AccessMode.FullSimpleAccess );
    IF verbose THEN
      IO.Put("Initialized simple security policy server.\n");
    END;
  END Init;

BEGIN
  Init();
END SecurityPolicy.

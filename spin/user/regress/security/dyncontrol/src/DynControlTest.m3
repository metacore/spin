(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *
 * 04-Feb-98  Robert Grimm (rgrimm) at the University of Washington
 *      Fixed bug so test is independent of SID of code
 *
 * 17-Dec-97  Robert Grimm (rgrimm) at the University of Washington
 *      Broke up into individual procedures
 *
 * 29-Oct-97  Robert Grimm (rgrimm) at the University of Washington
 *      Created.
 *
 *)

MODULE DynControlTest;

IMPORT Dispatcher;
IMPORT SecurityError, Permission, AccessMode;
IMPORT SecurityManager, SecurityManagerPrivate, SecurityPolicy;
IMPORT IO, Fmt;

IMPORT SafeConvert; (* Only import if DEBUG = TRUE *)

IMPORT DynControlTest;

(*
    Convenient Constants
   ----------------------
 *)

CONST
  DEBUG     = TRUE; (* Dump debugging information          *)

  NUM_ARGS  = 4;     (* Number of arguments to "Work"       *)

  KEY1      = 147;   (* Object 1 secret key                 *)
  KEY2      = 258;   (* Object 2 secret key                 *)
  INT_ARG   = 1111;  (* Integer argument to "Work"          *)
  KEY3      = 369;   (* Object 3 secret key                 *)

  OSID1     = 123;   (* Object 1 security identifier        *)
  OSID2     = 456;   (* Object 2 security identifier        *)
  OSID3     = 789;   (* Object 3 security identifier        *)

  REJECT    = 0;     (* Minimal rejection index             *)
  ALLOW     = 1000;  (* Maximal rejection index             *)


  PERM_NONE = AccessMode.SimpleT{};
  PERM_X    = AccessMode.SimpleT{ AccessMode.EXECUTE };

  WORK_NONE = SecurityManagerPrivate.AccessControlT{};
  WORK_D    = SecurityManagerPrivate.AccessControlT{
                SecurityManagerPrivate.AccessCheckT.DomainTransfer  };
  WORK_X    = SecurityManagerPrivate.AccessControlT{
                SecurityManagerPrivate.AccessCheckT.CodeCheck       };
  WORK_DXC  = SecurityManagerPrivate.AccessControlT{
                SecurityManagerPrivate.AccessCheckT.DomainTransfer,
                SecurityManagerPrivate.AccessCheckT.CodeCheck,
                SecurityManagerPrivate.AccessCheckT.ObjectCheck     };
  WORK_XC   = SecurityManagerPrivate.AccessControlT{
                SecurityManagerPrivate.AccessCheckT.CodeCheck,
                SecurityManagerPrivate.AccessCheckT.ObjectCheck     };


(*
    Internal state, used to communicate between policy and testing code
   ---------------------------------------------------------------------
 *)

VAR
  baseSubjectSid         : SecurityManager.SID;
  amNone, amP, amX, amXP : AccessMode.T;
  amList                 : AccessMode.ListT;
  o1, o2, o3             : SecureT;
  binding                : Dispatcher.Binding;
  doExecute              : BOOLEAN;
  domainTransfer         : BOOLEAN;
  allowNIL1              : BOOLEAN;
  allowNIL2              : BOOLEAN;
  allowNIL3              : BOOLEAN;
  didExecute             : BOOLEAN;
  errorExecuting         : BOOLEAN;
  rejectionIndex         : INTEGER;
  rejectedAt             : INTEGER;

(*
    Permission object
   -------------------
 *)

TYPE
  PermT = Permission.T OBJECT
  OVERRIDES
    contains := PermContain;
    toText   := PermToText;
  END;

PROCEDURE PermContain( <*UNUSED*> self : PermT; other : Permission.T )
  : BOOLEAN =
  BEGIN
    IF NOT ISTYPE( other, PermT ) THEN RETURN FALSE; END;
    RETURN TRUE;
  END PermContain;

PROCEDURE PermToText( <*UNUSED*> self : PermT ) : TEXT =
  BEGIN
    RETURN "I am a PermT";
  END PermToText;


(*
    A secure object
   -----------------
 *)

REVEAL
  SecureT = BRANDED Brand REF RECORD
    secretKey : INTEGER;
  END;


(*
    Mediation, the transparent way.
   ---------------------------------
 *)

PROCEDURE Mediate(   k1, k2    : SecurityManager.SID;
                   VAR mode    : AccessMode.T;
                   VAR s, o    : SecurityManager.SID;
                   VAR timeout : INTEGER;             ) : BOOLEAN =
  (* Put the two keys into the permission object *)
  BEGIN
    IF doExecute THEN
      mode := amX;
      doExecute := FALSE;
    ELSE
      IF k2 < rejectionIndex THEN
        mode := amP;
      ELSE
        mode := amNone;
        IF rejectedAt = -1 THEN
          rejectedAt := k2;
        END;
      END;
    END;
    s          := k1 + 1;
    o          := SecurityManager.GetCurrentObjectSid();
    timeout    := 0;
    RETURN FALSE;
  END Mediate;


(*
    My little worker
   ------------------
 *)

PROCEDURE Work(          a1 : SecureT;
                READONLY a2 : SecureT;
                          i : INTEGER;
                     VAR a3 : SecureT; ) =
  BEGIN
    didExecute := TRUE;
    IF DEBUG THEN
      IO.Put("   Work: starting to...\n");
    END;
    IF i # INT_ARG THEN
      errorExecuting := TRUE;
      RETURN;
    END;

    IF allowNIL1 THEN
      IF a1 # NIL AND a1.secretKey # KEY1 THEN
        errorExecuting := TRUE;
        RETURN;
      END;
    ELSE
      IF a1.secretKey # KEY1 THEN
        errorExecuting := TRUE;
        RETURN;
      END;
    END;

    IF allowNIL2 THEN
      IF a2 # NIL AND a2.secretKey # KEY2 THEN
        errorExecuting := TRUE;
        RETURN;
      END;
    ELSE
      IF a2.secretKey # KEY2 THEN
        errorExecuting := TRUE;
        RETURN;
      END;
    END;

    IF allowNIL3 THEN
      IF a3 # NIL AND a3.secretKey # KEY3 THEN
        errorExecuting := TRUE;
        RETURN;
      END;
    ELSE
      IF a3.secretKey # KEY3 THEN
        errorExecuting := TRUE;
        RETURN;
      END;
    END;

    IF DEBUG THEN
      IO.Put("   Work: all arguments are all right\n");
    END;

    IF domainTransfer THEN
      IF SecurityManager.GetCurrentSubjectSid() # baseSubjectSid + 1 THEN
        errorExecuting := TRUE;
        RETURN;
      END;
    ELSE
      IF SecurityManager.GetCurrentSubjectSid() # baseSubjectSid THEN
        errorExecuting := TRUE;
        RETURN;
      END;
    END;

    IF DEBUG THEN
      IO.Put("   Work: subject sid is all right\n");
    END;

    errorExecuting := FALSE;
  END Work;


(*
    Initialization and clean-up
  ------------------------------
*)

VAR
  init       : BOOLEAN := FALSE;
  medDefault : REF ARRAY OF Dispatcher.Binding;
  medNew     : REF ARRAY OF Dispatcher.Binding;

PROCEDURE Start(<*UNUSED*>i: INTEGER): BOOLEAN =
  VAR
    list    : Permission.ListT;
    perm    : Permission.T;
  BEGIN
    (* Tests only makes sense if security is enabled. *)
    IF NOT SecurityManager.CheckSecurity() THEN
      RETURN FALSE;
    END;

    IF NOT init THEN
      TRY
        medDefault    := NEW( REF ARRAY OF Dispatcher.Binding, 1 );
        medNew        := NEW( REF ARRAY OF Dispatcher.Binding, 1 );
        medDefault[0] := Dispatcher.GetOriginalHandler(
                             SecurityPolicy.Mediate);
        medNew[0]     := Dispatcher.Create( SecurityPolicy.Mediate, NIL,
                                            Mediate );
        binding       := Dispatcher.GetOriginalHandler(DynControlTest.Work);
      EXCEPT
      | Dispatcher.Error =>
        BEGIN
          IO.Put("*** Couldn't get handlers on SecurityPolicy.Mediate");
          IO.Put(" and DynControlTest.Work ***\n");
          RETURN FALSE;
        END;
      END;

      list := NEW( REF ARRAY OF Permission.T, 1 );
      init := TRUE;
    END;

    (* Initialize global state. *)

    SecurityManagerPrivate.SetTypeSecurity(TYPECODE(SecureT), TRUE);

    baseSubjectSid := SecurityManager.GetCurrentSubjectSid();
    perm           := NEW( PermT );
    list           := NEW( Permission.ListT, 1 );
    list[0]        := perm;
    amNone         := AccessMode.Create( PERM_NONE       );
    amP            := AccessMode.Create( PERM_NONE, list );
    amX            := AccessMode.Create( PERM_X          );
    amXP           := AccessMode.Create( PERM_X,    list );
    amList         := NEW( AccessMode.ListT, NUM_ARGS );
    amList[0]      := amP;
    amList[1]      := amP;
    amList[2]      := NIL;
    amList[3]      := amP;
    o1             := NEW( SecureT, secretKey := KEY1 );
    o2             := NEW( SecureT, secretKey := KEY2 );
    o3             := NEW( SecureT, secretKey := KEY3 );
    TRY
      SecurityManagerPrivate.SetSid( o1, OSID1 );
      SecurityManagerPrivate.SetSid( o2, OSID2 );
      SecurityManagerPrivate.SetSid( o3, OSID3 );
    EXCEPT
    | SecurityError.T =>
      BEGIN
        IO.Put("*** Couldn't reassign object security identifiers ***\n");
        RETURN FALSE;
      END;
    END;

    TRY
      Dispatcher.AtomicSwap( medDefault, medNew );
    EXCEPT
    | Dispatcher.Error =>
      BEGIN
        IO.Put("*** Couln't swap handlers on SecurityPolicy.Mediate ***\n");
        RETURN FALSE;
      END;
    END;

    SecurityManagerPrivate.Reset();

    RETURN TRUE;
  END Start;

PROCEDURE End(): BOOLEAN =
  BEGIN
    TRY
      Dispatcher.AtomicSwap( medNew, medDefault );
    EXCEPT
    | Dispatcher.Error =>
      BEGIN
        IO.Put("*** Couldn't swap handlers on SecurityPolicy.Mediate ***\n");
        RETURN FALSE;
      END;
    END;

    SecurityManagerPrivate.Reset();

    RETURN TRUE;
  END End;


(*
    The actual regression test
   ----------------------------
*)

PROCEDURE T1() : BOOLEAN =
  VAR
    ok : BOOLEAN;
  BEGIN
    (* Clear mediation cache *)
    SecurityManagerPrivate.ClearMediationCache();

    (* Perform actual tests *)

    ok := TRUE;
    TRY
      IF    o1.secretKey # KEY1
         OR o2.secretKey # KEY2
         OR o3.secretKey # KEY3
         OR SecurityManager.GetSid( o1 ) # OSID1
         OR SecurityManager.GetSid( o2 ) # OSID2
         OR SecurityManager.GetSid( o3 ) # OSID3 THEN
        ok := FALSE;
      END;
    EXCEPT
    | SecurityError.T => ok := FALSE;
    END;
    IF NOT ok THEN RETURN FALSE; END;
    IF DEBUG THEN
      IO.Put("   o1 at " & Fmt.Unsigned(SafeConvert.RefAnyToWord(o1),16) &
             "\n");
      IO.Put("   o2 at " & Fmt.Unsigned(SafeConvert.RefAnyToWord(o2),16) &
             "\n");
      IO.Put("   o3 at " & Fmt.Unsigned(SafeConvert.RefAnyToWord(o3),16) &
             "\n");
      IO.Put("-- Object setup ok\n");
    END;

    RETURN TRUE;
  END T1;

PROCEDURE T2() : BOOLEAN =
  BEGIN
    TRY
      SecurityManagerPrivate.SetAccessControl(binding, WORK_NONE);
    EXCEPT
    | SecurityError.T =>
      BEGIN
        IO.Put("*** Couldn't install access control on ");
        IO.Put("DynControlTest.Work ***\n");
        RETURN FALSE;
      END;
    END;
    IF DEBUG THEN
      IO.Put("-- Made sure no access control is imposed\n");
    END;

    doExecute      := FALSE;
    domainTransfer := FALSE;
    allowNIL1      := FALSE;
    allowNIL2      := FALSE;
    allowNIL3      := FALSE;
    didExecute     := FALSE;
    errorExecuting := FALSE;
    rejectionIndex := ALLOW;
    rejectedAt     := -1;
    DynControlTest.Work( o1, o2, INT_ARG, o3 );
    IF NOT didExecute OR     errorExecuting THEN RETURN FALSE; END;
    IF rejectedAt # -1                      THEN RETURN FALSE; END;
    IF SecurityManager.GetCurrentSubjectSid() # baseSubjectSid THEN
      RETURN FALSE;
    END;
    IF DEBUG THEN
      IO.Put("-- Work-ed without access control checks\n");
    END;
    
    RETURN TRUE;
  END T2;

PROCEDURE T3() : BOOLEAN =
  BEGIN
    TRY
      SecurityManagerPrivate.SetAccessControl(binding, WORK_XC, amX, amList);
    EXCEPT
    | SecurityError.T =>
      BEGIN
        IO.Put("*** Couldn't install access control on ");
        IO.Put("DynControlTest.Work ***\n");
        RETURN FALSE;
      END;
    END;
    IF DEBUG THEN
      IO.Put("-- Added code and object checks\n");
    END;
    
    RETURN TRUE;
  END T3;

PROCEDURE T4() : BOOLEAN =
  BEGIN
    doExecute      := FALSE;
    domainTransfer := FALSE;
    allowNIL1      := FALSE;
    allowNIL2      := FALSE;
    allowNIL3      := FALSE;
    didExecute     := FALSE;
    errorExecuting := FALSE;
    rejectionIndex := ALLOW;
    rejectedAt     := -1;
    DynControlTest.Work( o1, o2, INT_ARG, o3 );
    IF     didExecute                       THEN RETURN FALSE; END;
    IF rejectedAt # -1                      THEN RETURN FALSE; END;
    IF SecurityManager.GetCurrentSubjectSid() # baseSubjectSid THEN
      RETURN FALSE;
    END;
    IF DEBUG THEN
      IO.Put("-- Did not execute due to code check\n");
    END;

    RETURN TRUE;
  END T4;

PROCEDURE T5() : BOOLEAN =
  BEGIN
    doExecute      := TRUE;
    domainTransfer := FALSE;
    allowNIL1      := FALSE;
    allowNIL2      := FALSE;
    allowNIL3      := FALSE;
    didExecute     := FALSE;
    errorExecuting := FALSE;
    rejectionIndex := REJECT + 1;
    rejectedAt     := -1;
    DynControlTest.Work( o1, o2, INT_ARG, o3 );
    IF     didExecute                       THEN RETURN FALSE; END;
    IF rejectedAt # OSID1                   THEN RETURN FALSE; END;
    IF SecurityManager.GetCurrentSubjectSid() # baseSubjectSid THEN
      RETURN FALSE;
    END;
    IF DEBUG THEN
      IO.Put("-- Did not execute due to object 1 check\n");
    END;

    RETURN TRUE;
  END T5;

PROCEDURE T6() : BOOLEAN =
  BEGIN
    doExecute      := TRUE;
    domainTransfer := FALSE;
    allowNIL1      := FALSE;
    allowNIL2      := FALSE;
    allowNIL3      := FALSE;
    didExecute     := FALSE;
    errorExecuting := FALSE;
    rejectionIndex := OSID1 + 1;
    rejectedAt     := -1;
    DynControlTest.Work( o1, o2, INT_ARG, o3 );
    IF     didExecute                       THEN RETURN FALSE; END;
    IF rejectedAt # OSID2                   THEN RETURN FALSE; END;
    IF SecurityManager.GetCurrentSubjectSid() # baseSubjectSid THEN
      RETURN FALSE;
    END;
    IF DEBUG THEN
      IO.Put("-- Did not execute due to object 2 check\n");
    END;

    RETURN TRUE;
  END T6;

PROCEDURE T7() : BOOLEAN =
  BEGIN
    doExecute      := TRUE;
    domainTransfer := FALSE;
    allowNIL1      := FALSE;
    allowNIL2      := FALSE;
    allowNIL3      := FALSE;
    didExecute     := FALSE;
    errorExecuting := FALSE;
    rejectionIndex := OSID2 + 1;
    rejectedAt     := -1;
    DynControlTest.Work( o1, o2, INT_ARG, o3 );
    IF     didExecute                       THEN RETURN FALSE; END;
    IF rejectedAt # OSID3                   THEN RETURN FALSE; END;
    IF SecurityManager.GetCurrentSubjectSid() # baseSubjectSid THEN
      RETURN FALSE;
    END;
    IF DEBUG THEN
      IO.Put("-- Did not execute due to object 3 check\n");
    END;

    RETURN TRUE;
  END T7;

PROCEDURE T8() : BOOLEAN =
  BEGIN
    doExecute      := TRUE;
    domainTransfer := FALSE;
    allowNIL1      := FALSE;
    allowNIL2      := FALSE;
    allowNIL3      := FALSE;
    didExecute     := FALSE;
    errorExecuting := FALSE;
    rejectionIndex := OSID3 + 1;
    rejectedAt     := -1;
    DynControlTest.Work( o1, o2, INT_ARG, o3 );
    IF NOT didExecute OR     errorExecuting THEN RETURN FALSE; END;
    IF rejectedAt # -1                      THEN RETURN FALSE; END;
    IF SecurityManager.GetCurrentSubjectSid() # baseSubjectSid THEN
      RETURN FALSE;
    END;
    IF DEBUG THEN
      IO.Put("-- Did execute\n");
    END;

    RETURN TRUE;
  END T8;

PROCEDURE T9() : BOOLEAN =
  BEGIN
    doExecute      := TRUE;
    domainTransfer := FALSE;
    allowNIL1      := TRUE;
    allowNIL2      := FALSE;
    allowNIL3      := FALSE;
    didExecute     := FALSE;
    errorExecuting := FALSE;
    rejectionIndex := ALLOW;
    rejectedAt     := -1;
    DynControlTest.Work( NIL, o2, INT_ARG, o3 );
    IF NOT didExecute OR     errorExecuting THEN RETURN FALSE; END;
    IF rejectedAt # -1                      THEN RETURN FALSE; END;
    IF SecurityManager.GetCurrentSubjectSid() # baseSubjectSid THEN
      RETURN FALSE;
    END;
    IF DEBUG THEN
      IO.Put("-- Did execute with NIL for first reference\n");
    END;

    RETURN TRUE;
  END T9;

PROCEDURE T10() : BOOLEAN =
  BEGIN
    doExecute      := TRUE;
    domainTransfer := FALSE;
    allowNIL1      := FALSE;
    allowNIL2      := TRUE;
    allowNIL3      := FALSE;
    didExecute     := FALSE;
    errorExecuting := FALSE;
    rejectionIndex := ALLOW;
    rejectedAt     := -1;
    DynControlTest.Work( o1, NIL, INT_ARG, o3 );
    IF NOT didExecute OR     errorExecuting THEN RETURN FALSE; END;
    IF rejectedAt # -1                      THEN RETURN FALSE; END;
    IF SecurityManager.GetCurrentSubjectSid() # baseSubjectSid THEN
      RETURN FALSE;
    END;
    IF DEBUG THEN
      IO.Put("-- Did execute with NIL for second reference\n");
    END;

    RETURN TRUE;
  END T10;

PROCEDURE T11() : BOOLEAN =
  BEGIN
    doExecute      := TRUE;
    domainTransfer := FALSE;
    allowNIL1      := FALSE;
    allowNIL2      := FALSE;
    allowNIL3      := TRUE;
    didExecute     := FALSE;
    errorExecuting := FALSE;
    rejectionIndex := ALLOW;
    rejectedAt     := -1;
    VAR
      nil : SecureT := NIL;
    BEGIN
      DynControlTest.Work( o1, o2, INT_ARG, nil );
    END;
    IF NOT didExecute OR     errorExecuting THEN RETURN FALSE; END;
    IF rejectedAt # -1                      THEN RETURN FALSE; END;
    IF SecurityManager.GetCurrentSubjectSid() # baseSubjectSid THEN
      RETURN FALSE;
    END;
    IF DEBUG THEN
      IO.Put("-- Did execute with NIL for third reference\n");
    END;

    RETURN TRUE;
  END T11;

PROCEDURE T12() : BOOLEAN =
  VAR
    ok : BOOLEAN;
  BEGIN
    amList[2] := amXP;
    ok        := FALSE;
    TRY
      SecurityManagerPrivate.SetAccessControl(binding, WORK_XC, NIL, amList);
    EXCEPT
    | SecurityError.T => ok := TRUE;
    END;
    IF NOT ok THEN RETURN FALSE; END;
    amList[2] := NIL;
    IF DEBUG THEN
      IO.Put("-- Passed type checking for code and object checks\n");
    END;

    RETURN TRUE;
  END T12;

PROCEDURE T13() : BOOLEAN =
  BEGIN
    TRY
      SecurityManagerPrivate.SetAccessControl(binding, WORK_D);
    EXCEPT
    | SecurityError.T =>
      BEGIN
        IO.Put("*** Couldn't install access control on ");
        IO.Put("DynControlTest.Work ***\n");
        RETURN FALSE;
      END;
    END;
    IF DEBUG THEN
      IO.Put("-- Changed access control into domain transfer\n");
    END;

    RETURN TRUE;
  END T13;

PROCEDURE T14() : BOOLEAN =
  BEGIN
    doExecute      := FALSE;
    domainTransfer := TRUE;
    allowNIL1      := FALSE;
    allowNIL2      := FALSE;
    allowNIL3      := FALSE;
    didExecute     := FALSE;
    errorExecuting := FALSE;
    rejectionIndex := ALLOW;
    rejectedAt     := -1;
    DynControlTest.Work( o1, o2, INT_ARG, o3 );
    IF NOT didExecute OR     errorExecuting THEN RETURN FALSE; END;
    IF rejectedAt # -1                      THEN RETURN FALSE; END;
    IF SecurityManager.GetCurrentSubjectSid() # baseSubjectSid THEN
      RETURN FALSE;
    END;
    IF DEBUG THEN
      IO.Put("-- Did execute\n");
    END;

    RETURN TRUE;
  END T14;

PROCEDURE T15() : BOOLEAN =
  BEGIN
    TRY
      SecurityManagerPrivate.SetAccessControl(binding, WORK_DXC);
    EXCEPT
    | SecurityError.T =>
      BEGIN
        IO.Put("*** Couldn't install access control on ");
        IO.Put("DynControlTest.Work ***\n");
        RETURN FALSE;
      END;
    END;
    IF DEBUG THEN
      IO.Put("-- Changed access control into domain transfer + checks\n");
    END;

    RETURN TRUE;
  END T15;

PROCEDURE T16() : BOOLEAN =
  BEGIN
    doExecute      := FALSE;
    domainTransfer := TRUE;
    allowNIL1      := FALSE;
    allowNIL2      := FALSE;
    allowNIL3      := FALSE;
    didExecute     := FALSE;
    errorExecuting := FALSE;
    rejectionIndex := ALLOW;
    rejectedAt     := -1;
    DynControlTest.Work( o1, o2, INT_ARG, o3 );
    IF     didExecute                       THEN RETURN FALSE; END;
    IF rejectedAt # -1                      THEN RETURN FALSE; END;
    IF SecurityManager.GetCurrentSubjectSid() # baseSubjectSid THEN
      RETURN FALSE;
    END;
    IF DEBUG THEN
      IO.Put("-- Did not execute due to code check\n");
    END;

    RETURN TRUE;
  END T16;

PROCEDURE T17() : BOOLEAN =
  BEGIN
    doExecute      := TRUE;
    domainTransfer := TRUE;
    allowNIL1      := FALSE;
    allowNIL2      := FALSE;
    allowNIL3      := FALSE;
    didExecute     := FALSE;
    errorExecuting := FALSE;
    rejectionIndex := REJECT + 1;
    rejectedAt     := -1;
    DynControlTest.Work( o1, o2, INT_ARG, o3 );
    IF     didExecute                       THEN RETURN FALSE; END;
    IF rejectedAt # OSID1                   THEN RETURN FALSE; END;
    IF SecurityManager.GetCurrentSubjectSid() # baseSubjectSid THEN
      RETURN FALSE;
    END;
    IF DEBUG THEN
      IO.Put("-- Did not execute due to object 1 check\n");
    END;

    RETURN TRUE;
  END T17;

PROCEDURE T18() : BOOLEAN =
  BEGIN
    doExecute      := TRUE;
    domainTransfer := TRUE;
    allowNIL1      := FALSE;
    allowNIL2      := FALSE;
    allowNIL3      := FALSE;
    didExecute     := FALSE;
    errorExecuting := FALSE;
    rejectionIndex := OSID1 + 1;
    rejectedAt     := -1;
    DynControlTest.Work( o1, o2, INT_ARG, o3 );
    IF     didExecute                       THEN RETURN FALSE; END;
    IF rejectedAt # OSID2                   THEN RETURN FALSE; END;
    IF SecurityManager.GetCurrentSubjectSid() # baseSubjectSid THEN
      RETURN FALSE;
    END;
    IF DEBUG THEN
      IO.Put("-- Did not execute due to object 2 check\n");
    END;

    RETURN TRUE;
  END T18;

PROCEDURE T19() : BOOLEAN =
  BEGIN
    doExecute      := TRUE;
    domainTransfer := TRUE;
    allowNIL1      := FALSE;
    allowNIL2      := FALSE;
    allowNIL3      := FALSE;
    didExecute     := FALSE;
    errorExecuting := FALSE;
    rejectionIndex := OSID2 + 1;
    rejectedAt     := -1;
    DynControlTest.Work( o1, o2, INT_ARG, o3 );
    IF     didExecute                       THEN RETURN FALSE; END;
    IF rejectedAt # OSID3                   THEN RETURN FALSE; END;
    IF SecurityManager.GetCurrentSubjectSid() # baseSubjectSid THEN
      RETURN FALSE;
    END;
    IF DEBUG THEN
      IO.Put("-- Did not execute due to object 3 check\n");
    END;

    RETURN TRUE;
  END T19;

PROCEDURE T20() : BOOLEAN =
  BEGIN
    doExecute      := TRUE;
    domainTransfer := TRUE;
    allowNIL1      := FALSE;
    allowNIL2      := FALSE;
    allowNIL3      := FALSE;
    didExecute     := FALSE;
    errorExecuting := FALSE;
    rejectionIndex := OSID3 + 1;
    rejectedAt     := -1;
    DynControlTest.Work( o1, o2, INT_ARG, o3 );
    IF NOT didExecute OR     errorExecuting THEN RETURN FALSE; END;
    IF rejectedAt # -1                      THEN RETURN FALSE; END;
    IF SecurityManager.GetCurrentSubjectSid() # baseSubjectSid THEN
      RETURN FALSE;
    END;
    IF DEBUG THEN
      IO.Put("-- Did execute\n");
    END;

    RETURN TRUE;
  END T20;

PROCEDURE T21() : BOOLEAN =
  BEGIN
    TRY
      SecurityManagerPrivate.SetAccessControl(binding, WORK_NONE);
    EXCEPT
    | SecurityError.T =>
      BEGIN
        IO.Put("*** Couldn't install access control on ");
        IO.Put("DynControlTest.Work ***\n");
        RETURN FALSE;
      END;
    END;
    IF DEBUG THEN
      IO.Put("-- Removed all access control\n");
    END;

    RETURN TRUE;
  END T21;

PROCEDURE T22() : BOOLEAN =
  BEGIN
    doExecute      := FALSE;
    domainTransfer := FALSE;
    allowNIL1      := FALSE;
    allowNIL2      := FALSE;
    allowNIL3      := FALSE;
    didExecute     := FALSE;
    errorExecuting := FALSE;
    rejectionIndex := ALLOW;
    rejectedAt     := -1;
    DynControlTest.Work( o1, o2, INT_ARG, o3 );
    IF NOT didExecute OR     errorExecuting THEN RETURN FALSE; END;
    IF rejectedAt # -1                      THEN RETURN FALSE; END;
    IF SecurityManager.GetCurrentSubjectSid() # baseSubjectSid THEN
      RETURN FALSE;
    END;
    IF DEBUG THEN
      IO.Put("-- Work-ed without access control checks\n");
    END;

    RETURN TRUE;
  END T22;

PROCEDURE T23() : BOOLEAN =
  BEGIN
    amList[0] := NIL;
    amList[1] := NIL;
    amList[2] := NIL;
    amList[3] := NIL;
    TRY
      SecurityManagerPrivate.SetAccessControl(binding, WORK_XC, NIL, amList);
    EXCEPT
    | SecurityError.T =>
      BEGIN
        IO.Put("*** Couldn't install access control on ");
        IO.Put("DynControlTest.Work ***\n");
        RETURN FALSE;
      END;
    END;
    IF DEBUG THEN
      IO.Put("-- Added code and object checks\n");
    END;

    RETURN TRUE;
  END T23;

PROCEDURE T24() : BOOLEAN =
  VAR
    m1, m2, m3 : AccessMode.T;
    ml         : AccessMode.ListT;
  BEGIN
    IF   SecurityManagerPrivate.GetAccessControl(binding,m1,ml,m2,m3)
      # WORK_X THEN
      RETURN FALSE;
    END;
    IF DEBUG THEN
      IO.Put("-- Reduced access control operations\n");
    END;
    
    RETURN TRUE;
  END T24;

PROCEDURE T25() : BOOLEAN =
  BEGIN
    doExecute      := TRUE;
    domainTransfer := FALSE;
    allowNIL1      := FALSE;
    allowNIL2      := FALSE;
    allowNIL3      := FALSE;
    didExecute     := FALSE;
    errorExecuting := FALSE;
    rejectionIndex := ALLOW;
    rejectedAt     := -1;
    DynControlTest.Work( o1, o2, INT_ARG, o3 );
    IF NOT didExecute OR     errorExecuting THEN RETURN FALSE; END;
    IF rejectedAt # -1                      THEN RETURN FALSE; END;
    IF SecurityManager.GetCurrentSubjectSid() # baseSubjectSid THEN
      RETURN FALSE;
    END;
    IF DEBUG THEN
      IO.Put("-- Work-ed with NIL access control checks\n");
    END;

    RETURN TRUE;
  END T25;

BEGIN
END DynControlTest.

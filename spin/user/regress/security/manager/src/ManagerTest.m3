(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *
 * 08-Aug-97  Robert Grimm (rgrimm) at the University of Washington
 *      Created.
 *
 *)

MODULE ManagerTest;

IMPORT Fmt, RTIO, IO, Clock, Dispatcher;
IMPORT Thread, ThreadExtra;

IMPORT SecurityError, Permission, AccessMode;
IMPORT SecurityManager, SecurityManagerPrivate, SecurityPolicy;
IMPORT SecurityManagerProtected AS SMP;

IMPORT ManagerTest;

CONST
  DEBUG   : BOOLEAN = FALSE; (* Dump debugging information          *)
  TIMEOUT : INTEGER = 100;   (* Time out for mediation cache        *)
  CHECK   : INTEGER = 300;   (* When to check that timeout happened *)


(*
    Our very own permission object, holds the two keys used in mediation.
   -----------------------------------------------------------------------
 *)

TYPE
  PermT = Permission.T OBJECT
    k1 : SecurityManager.SID;
    k2 : SecurityManager.SID;
  OVERRIDES
    contains := PermContain;
    toText   := PermToText;
  END;

PROCEDURE PermContain( self : PermT; other : Permission.T ) : BOOLEAN =
  BEGIN
    IF NOT ISTYPE( other, PermT ) THEN RETURN FALSE; END;
    WITH o = NARROW( other, PermT ) DO
      RETURN self.k1 = o.k1 AND self.k2 = o.k2;
    END;
  END PermContain;

PROCEDURE PermToText( self : PermT ) : TEXT =
  BEGIN
    RETURN Fmt.Int(self.k1) & "," & Fmt.Int(self.k2);
  END PermToText;


(*
    A secure object
   -----------------
 *)

TYPE
  SecureT = BRANDED "SecureObject" REF RECORD
    secretKey : INTEGER;
  END;


(*
    Mediation, the transparent way.
   ---------------------------------
 *)

PROCEDURE Mediate1(   k1, k2    : SecurityManager.SID;
                    VAR mode    : AccessMode.T;
                    VAR s, o    : SecurityManager.SID;
                    VAR timeout : INTEGER;             ) : BOOLEAN =
  (* Put the two keys into the permission object *)
  VAR
    perm : Permission.T;
  BEGIN
    perm    := NEW( PermT, k1 := k1, k2 := k2 );
    list[0] := perm;
    mode    := AccessMode.Create( AccessMode.SimpleT{}, list );
    s       := k1;
    o       := k2;
    timeout := 0;
    IF k1 >= 1024 AND k1 < 2048 AND k2 >= 1024 AND k2 < 2048 THEN
      timeout := TIMEOUT;
    END;
    RETURN TRUE;
  END Mediate1;

PROCEDURE Mediate2(   k1, k2    : SecurityManager.SID;
                    VAR mode    : AccessMode.T;
                    VAR s, o    : SecurityManager.SID;
                    VAR timeout : INTEGER;             ) : BOOLEAN =
  (* Put the two keys into the permission object *)
  VAR
    perm : Permission.T;
  BEGIN
    perm    := NEW( PermT, k1 := k1, k2 := k2 );
    list[0] := perm;
    mode    := AccessMode.Create( AccessMode.SimpleT{}, list );
    s       := k1 + 1;
    o       := SecurityManager.GetCurrentObjectSid() + 1;
    timeout := 0;
    RETURN TRUE;
  END Mediate2;


(*
    Who am I?
   -----------
 *)

VAR
  baseSubjectSid : SecurityManager.SID;
  baseObjectSid  : SecurityManager.SID;

(*
    One and two stepping
   ----------------------
 *)

PROCEDURE Step( i : INTEGER ) : BOOLEAN =
  BEGIN
    IF DEBUG THEN
      RTIO.PutText(" Step i " & Fmt.Int(i) & "  s-sid " &
                   Fmt.Int(SecurityManager.GetCurrentSubjectSid())
                   & "  o-sid " &
                   Fmt.Int(SecurityManager.GetCurrentObjectSid()) & "\n");
    END;
    IF    SecurityManager.GetCurrentSubjectSid() # baseSubjectSid + i
       OR SecurityManager.GetCurrentObjectSid () # baseObjectSid  + i THEN
      RETURN TRUE;
    END;
    IF i < 5 THEN
      RETURN ManagerTest.Step( i + 1 );
    END;
    IF    SecurityManager.GetCurrentSubjectSid() # baseSubjectSid + i
       OR SecurityManager.GetCurrentObjectSid () # baseObjectSid  + i THEN
      RETURN TRUE;
    END;
    RETURN FALSE;
  END Step;

PROCEDURE StepTwo( i : INTEGER; raise : BOOLEAN ) : BOOLEAN
  RAISES { DasEndeDerWelt } =
  BEGIN
    IF DEBUG THEN
      RTIO.PutText(" StepTwo i " & Fmt.Int(i) & "  s-sid " &
                   Fmt.Int(SecurityManager.GetCurrentSubjectSid())
                   & "  o-sid " &
                   Fmt.Int(SecurityManager.GetCurrentObjectSid()));
      IF raise THEN
        RTIO.PutText("  raise\n");
      ELSE
        RTIO.PutText("  no raise\n");
      END;
    END;
    IF    SecurityManager.GetCurrentSubjectSid() # baseSubjectSid
       OR SecurityManager.GetCurrentObjectSid () # baseObjectSid  THEN
      RETURN TRUE;
    END;
    IF i < 5 THEN
      RETURN ManagerTest.StepTwo( i + 1, raise );
    END;
    IF    SecurityManager.GetCurrentSubjectSid() # baseSubjectSid
       OR SecurityManager.GetCurrentObjectSid () # baseObjectSid  THEN
      RETURN TRUE;
    END;
    IF raise THEN RAISE DasEndeDerWelt; END;
    RETURN FALSE;
  END StepTwo;

PROCEDURE StepThree( i : INTEGER; raise : BOOLEAN ) : BOOLEAN
  RAISES { DasEndeDerWelt } =
  BEGIN
    IF DEBUG THEN
      RTIO.PutText(" StepThree i " & Fmt.Int(i) & "  subject sid " &
                   Fmt.Int(SecurityManager.GetCurrentSubjectSid())
                   & "  o-sid " &
                   Fmt.Int(SecurityManager.GetCurrentObjectSid()));
      IF raise THEN
        RTIO.PutText("  raise\n");
      ELSE
        RTIO.PutText("  no raise\n");
      END;
    END;
    IF    SecurityManager.GetCurrentSubjectSid() # baseSubjectSid + i
       OR SecurityManager.GetCurrentObjectSid () # baseObjectSid  + i THEN
      RETURN TRUE;
    END;
    IF i < 5 THEN
      RETURN ManagerTest.StepThree( i + 1, raise );
    END;
    IF    SecurityManager.GetCurrentSubjectSid() # baseSubjectSid + i
       OR SecurityManager.GetCurrentObjectSid()  # baseObjectSid  + i THEN
      RETURN TRUE;
    END;
    IF raise THEN RAISE DasEndeDerWelt; END;
    RETURN FALSE;
  END StepThree;

PROCEDURE StepFour( i : INTEGER ) : BOOLEAN =
  VAR
    secure : SecureT;
    sid    : SecurityManager.SID;
    abort  : BOOLEAN := FALSE;
  BEGIN
    IF DEBUG THEN
      RTIO.PutText(" StepFour i " & Fmt.Int(i) & "  object sid " &
                   Fmt.Int(SecurityManager.GetCurrentObjectSid()));
    END;
    secure := NEW( SecureT, secretKey := i );
    TRY
      sid := SecurityManager.GetSid( secure );
    EXCEPT
    | SecurityError.T => abort := TRUE;
    END;
    IF DEBUG THEN
      IF abort THEN
        RTIO.PutText("  insecure\n");
      ELSE
        RTIO.PutText("  real sid " & Fmt.Int(sid) & "\n");
      END;
    END;
    IF    abort
       OR SecurityManager.GetCurrentObjectSid() # baseObjectSid + i
       OR SecurityManager.GetCurrentObjectSid() # sid               THEN
      RETURN TRUE;
    END;
    IF i < 5 THEN
      RETURN ManagerTest.StepFour( i + 1 );
    END;
    RETURN FALSE;
  END StepFour;

(*
    Ping-pong
   -----------
*)

REVEAL
  PingPongT = MUTEX BRANDED Brand OBJECT
    error : BOOLEAN := FALSE;
    value : INTEGER := 0;
  END;

VAR
  incremented : Thread.Condition;

PROCEDURE PingSetup() : BOOLEAN =
  VAR
    x : PingPongT;
    t : Thread.T;
  BEGIN
    incremented := NEW(Thread.Condition);
    x           := NEW( PingPongT );
    t           := ThreadExtra.PFork( PongSetup, x );
    ManagerTest.Ping( x, 0 );
    EVAL Thread.Join( t );
    RETURN NOT x.error;
  END PingSetup;

PROCEDURE PongSetup( x : REFANY ) : REFANY =
  BEGIN
    ManagerTest.Pong( NARROW( x, PingPongT ), 1 );
    RETURN NIL;
  END PongSetup;

PROCEDURE Ping( x : PingPongT; i : INTEGER ) =
  VAR
    goDown : BOOLEAN;
  BEGIN
    LOCK x DO
      WHILE x.value # i DO
        IF SecurityManager.GetCurrentSubjectSid() # baseSubjectSid THEN
          x.error := TRUE;
        END;
        IF x.error THEN RETURN; END;
        Thread.Wait( x, incremented );
      END;
      INC( x.value );
      IF DEBUG THEN
        RTIO.PutText(" Ping i " & Fmt.Int(i) & "  new value "
                     & Fmt.Int(x.value) & "  subject sid " &
                     Fmt.Int(SecurityManager.GetCurrentSubjectSid()) & "\n");
      END;
      goDown := x.value < 7;
    END;
    Thread.Signal( incremented );
    IF goDown THEN
      ManagerTest.Ping( x, i + 2 );
    END;
    IF SecurityManager.GetCurrentSubjectSid() # baseSubjectSid THEN
      LOCK x DO
        x.error := TRUE;
      END;
    END;
  END Ping;

PROCEDURE Pong( x : PingPongT; i : INTEGER ) =
  VAR
    goDown : BOOLEAN;
    sid    : INTEGER := baseSubjectSid + ((i - 1) DIV 2) + 1;
  BEGIN
    LOCK x DO
      WHILE x.value # i DO
        IF SecurityManager.GetCurrentSubjectSid() # sid THEN
          x.error := TRUE;
        END;
        IF x.error THEN RETURN; END;
        Thread.Wait( x, incremented );
      END;
      INC( x.value );
      IF DEBUG THEN
        RTIO.PutText(" Pong i " & Fmt.Int(i) & "  new value "
                     & Fmt.Int(x.value) & "  subject sid " &
                     Fmt.Int(SecurityManager.GetCurrentSubjectSid()) & "\n");
      END;
      goDown := x.value < 8;
    END;
    Thread.Signal( incremented );
    IF goDown THEN
      ManagerTest.Pong( x, i + 2 );
    END;
    IF SecurityManager.GetCurrentSubjectSid() # sid THEN
      LOCK x DO
        x.error := TRUE;
      END;
    END;
  END Pong;

PROCEDURE PingTwo( x : PingPongT; i : INTEGER ) : BOOLEAN =
  VAR
    error : BOOLEAN;
    t     : Thread.T;
  BEGIN
    IF DEBUG THEN
      RTIO.PutText(" PingTwo i " & Fmt.Int(i) & "  subject sid " &
             Fmt.Int(SecurityManager.GetCurrentSubjectSid()) & "\n");
    END;
    IF i < 3 THEN
      error := ManagerTest.PingTwo( x, i + 1 );
    ELSE
      t := ThreadExtra.PFork( PongTwo, x );
      EVAL Thread.Join( t );
      error := x.error;
      IF x.value # 1 THEN error := TRUE; END;
    END;
    RETURN error;
  END PingTwo;

PROCEDURE PongTwo( x : REFANY ) : REFANY =
  BEGIN
    IF DEBUG THEN
      RTIO.PutText(" PongTwo subject sid " &
             Fmt.Int(SecurityManager.GetCurrentSubjectSid()) & "\n");
    END;
    WITH pp = NARROW( x, PingPongT ) DO
      IF SecurityManager.GetCurrentSubjectSid() # baseSubjectSid + 3 THEN
        pp.error := TRUE;
      END;
      INC( pp.value );
    END;
    RETURN NIL;
  END PongTwo;


(*
    Initialization and clean-up
  ------------------------------
*)

CONST
  DOMAIN = SecurityManagerPrivate.AccessControlT{
             SecurityManagerPrivate.AccessCheckT.DomainTransfer };

VAR
  init       : BOOLEAN := FALSE;
  medDefault : REF ARRAY OF Dispatcher.Binding;
  medNew1    : REF ARRAY OF Dispatcher.Binding;
  medNew2    : REF ARRAY OF Dispatcher.Binding;
  list       : REF ARRAY OF Permission.T;

PROCEDURE Start(<*UNUSED*>i: INTEGER): BOOLEAN =
  VAR
    binding : Dispatcher.Binding;
  BEGIN
    IF NOT SecurityManager.CheckSecurity() THEN RETURN FALSE; END;

    IF NOT init THEN
      TRY
        medDefault    := NEW( REF ARRAY OF Dispatcher.Binding, 1 );
        medNew1       := NEW( REF ARRAY OF Dispatcher.Binding, 1 );
        medNew2       := NEW( REF ARRAY OF Dispatcher.Binding, 1 );
        medDefault[0] := Dispatcher.GetOriginalHandler(SecurityPolicy.Mediate);
        medNew1[0]    := Dispatcher.Create( SecurityPolicy.Mediate, NIL,
                                            Mediate1 );
        medNew2[0]    := Dispatcher.Create( SecurityPolicy.Mediate, NIL,
                                            Mediate2 );
      EXCEPT
      | Dispatcher.Error =>
        BEGIN
          IO.Put("*** Couldn't get handlers on SecurityPolicy.Mediate ***\n");
          RETURN FALSE;
        END;
      END;
      
      TRY
        binding := Dispatcher.GetOriginalHandler(ManagerTest.Step);
        SecurityManagerPrivate.SetAccessControl(binding, DOMAIN);
      EXCEPT
      | Dispatcher.Error =>
        BEGIN
          IO.Put("*** Couldn't get handler for ManagerTest.Step ***\n");
          RETURN FALSE;
        END;
      | SecurityError.T =>
        BEGIN
          IO.Put("*** Coulnd't install access control on ");
          IO.Put("ManagerTest.Step ***\n");
          RETURN FALSE;
        END;
      END;

      TRY
        binding := Dispatcher.GetOriginalHandler(ManagerTest.StepThree);
        SecurityManagerPrivate.SetAccessControl(binding, DOMAIN);
      EXCEPT
      | Dispatcher.Error =>
        BEGIN
          IO.Put("*** Couldn't get handler for ManagerTest.StepThree ***\n");
          RETURN FALSE;
        END;
      | SecurityError.T =>
        BEGIN
          IO.Put("*** Coulnd't install access control on ");
          IO.Put("ManagerTest.StepThree ***\n");
          RETURN FALSE;
        END;
      END;

      TRY
        binding := Dispatcher.GetOriginalHandler(ManagerTest.StepFour);
        SecurityManagerPrivate.SetAccessControl(binding, DOMAIN);
      EXCEPT
      | Dispatcher.Error =>
        BEGIN
          IO.Put("*** Couldn't get handler for ManagerTest.StepFour ***\n");
          RETURN FALSE;
        END;
      | SecurityError.T =>
        BEGIN
          IO.Put("*** Coulnd't install access control on ");
          IO.Put("ManagerTest.StepFour ***\n");
          RETURN FALSE;
        END;
      END;

      TRY
        binding := Dispatcher.GetOriginalHandler(ManagerTest.Pong);
        SecurityManagerPrivate.SetAccessControl(binding, DOMAIN);
      EXCEPT
      | Dispatcher.Error =>
        BEGIN
          IO.Put("*** Couldn't get handler for ManagerTest.Pong ***\n");
          RETURN FALSE;
        END;
      | SecurityError.T =>
        BEGIN
          IO.Put("*** Coulnd't install access control on ");
          IO.Put("ManagerTest.Pong ***\n");
          RETURN FALSE;
        END;
      END;

      TRY
        binding := Dispatcher.GetOriginalHandler(ManagerTest.PingTwo);
        SecurityManagerPrivate.SetAccessControl(binding, DOMAIN);
      EXCEPT
      | Dispatcher.Error =>
        BEGIN
          IO.Put("*** Couldn't get handler for ManagerTest.PingTwo ***\n");
          RETURN FALSE;
        END;
      | SecurityError.T =>
        BEGIN
          IO.Put("*** Coulnd't install access control on ");
          IO.Put("ManagerTest.PingTwo ***\n");
          RETURN FALSE;
        END;
      END;

      list := NEW( REF ARRAY OF Permission.T, 1 );
      init := TRUE;
    END;

    TRY
      Dispatcher.AtomicSwap( medDefault, medNew1 );
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
      Dispatcher.AtomicSwap( medNew2, medDefault );
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
    The actual regression tests
   -----------------------------
*)

PROCEDURE T1() : BOOLEAN =
  VAR
    mode0 : AccessMode.T;
    mode1 : AccessMode.T;
    mode2 : AccessMode.T;
    mode3 : AccessMode.T;
    perm  : PermT;
    s, o  : SecurityManager.SID;
  BEGIN
    (* Clear mediation cache *)
    SecurityManagerPrivate.ClearMediationCache();

    (* Fill mediation cache to capacity *)
    FOR i := 0 TO 7 DO
      FOR j := 0 TO 7 DO
        (* Mediate once *)
        mode1 := SMP.Mediate( i, j, s, o );
        IF s # i OR o # j THEN RETURN FALSE; END;
        perm := AccessMode.GetPermission( mode1, 0 );
        IF perm.k1 # i OR perm.k2 # j THEN RETURN FALSE; END;
        (* Mediate twice, this time from cache *)
        mode2 := SMP.Mediate( i, j, s, o );
        IF mode1 # mode2 THEN RETURN FALSE; END;
      END;
    END;

    (* Satisfied from cache *)
    mode0 := SMP.Mediate( 0, 0, s, o );
    IF s # 0 OR o # 0 THEN RETURN FALSE; END;
    perm := AccessMode.GetPermission( mode0, 0 );
    IF perm.k1 # 0 OR perm.k2 # 0 THEN RETURN FALSE; END;
    
    (* Satisfied from cache *)
    mode1 := SMP.Mediate( 0, 1, s, o );
    IF s # 0 OR o # 1 THEN RETURN FALSE; END;
    perm := AccessMode.GetPermission( mode1, 0 );
    IF perm.k1 # 0 OR perm.k2 # 1 THEN RETURN FALSE; END;

    (* Replaces 0,0 entry in cache *)
    mode2 := SMP.Mediate( 8, 8, s, o );
    IF s # 8 OR o # 8 THEN RETURN FALSE; END;
    perm := AccessMode.GetPermission( mode2, 0 );
    IF perm.k1 # 8 OR perm.k2 # 8 THEN RETURN FALSE; END;
    
    (* Replaces 0,1 entry in cache *)
    mode3 := SMP.Mediate( 0, 0, s, o );
    IF s # 0 OR o # 0 THEN RETURN FALSE; END;
    perm := AccessMode.GetPermission( mode3, 0 );
    IF perm.k1 # 0 OR perm.k2 # 0 THEN RETURN FALSE; END;
    IF mode3 = mode0 THEN RETURN FALSE; END;

    (* Satisfied from cache *)
    mode3 := SMP.Mediate( 8, 8, s, o );
    IF s # 8 OR o # 8 THEN RETURN FALSE; END;
    perm := AccessMode.GetPermission( mode3, 0 );
    IF perm.k1 # 8 OR perm.k2 # 8 THEN RETURN FALSE; END;
    IF mode3 # mode2 THEN RETURN FALSE; END;

    (* Replaces 0,2 entry in cache *)
    mode3 := SMP.Mediate( 0, 1, s, o );
    IF s # 0 OR o # 1 THEN RETURN FALSE; END;
    perm := AccessMode.GetPermission( mode3, 0 );
    IF perm.k1 # 0 OR perm.k2 # 1 THEN RETURN FALSE; END;
    IF mode3 = mode1 THEN RETURN FALSE; END;

    RETURN TRUE;
  END T1;

PROCEDURE T2() : BOOLEAN =
  BEGIN
    TRY
      SecurityManagerPrivate.SetMediationCacheSize( 2, 2 );
    EXCEPT
    | SecurityError.T => RETURN FALSE;
    END;

    RETURN TRUE;
  END T2;

PROCEDURE T3() : BOOLEAN =
  VAR
    mode0 : AccessMode.T;
    mode1 : AccessMode.T;
    mode2 : AccessMode.T;
    mode3 : AccessMode.T;
    mode4 : AccessMode.T;
    perm  : PermT;
    s, o  : SecurityManager.SID;
  BEGIN
    SecurityManagerPrivate.ClearMediationCache();

    mode0 := SMP.Mediate( 0, 0, s, o );
    IF s # 0 OR o # 0 THEN RETURN FALSE; END;
    perm := AccessMode.GetPermission( mode0, 0 );
    IF perm.k1 # 0 OR perm.k2 # 0 THEN RETURN FALSE; END;

    mode1 := SMP.Mediate( 0, 1, s, o );
    IF s # 0 OR o # 1 THEN RETURN FALSE; END;
    perm := AccessMode.GetPermission( mode1, 0 );
    IF perm.k1 # 0 OR perm.k2 # 1 THEN RETURN FALSE; END;

    mode2 := SMP.Mediate( 1, 0, s, o );
    IF s # 1 OR o # 0 THEN RETURN FALSE; END;
    perm := AccessMode.GetPermission( mode2, 0 );
    IF perm.k1 # 1 OR perm.k2 # 0 THEN RETURN FALSE; END;

    mode3 := SMP.Mediate( 1, 1, s, o );
    IF s # 1 OR o # 1 THEN RETURN FALSE; END;
    perm := AccessMode.GetPermission( mode3, 0 );
    IF perm.k1 # 1 OR perm.k2 # 1 THEN RETURN FALSE; END;

    (* And, once over again *)
    mode4 := SMP.Mediate( 0, 0, s, o );
    IF s # 0 OR o # 0 THEN RETURN FALSE; END;
    perm := AccessMode.GetPermission( mode4, 0 );
    IF perm.k1 # 0 OR perm.k2 # 0 THEN RETURN FALSE; END;
    IF mode4 # mode0 THEN RETURN FALSE; END;

    mode4 := SMP.Mediate( 0, 1, s, o );
    IF s # 0 OR o # 1 THEN RETURN FALSE; END;
    perm := AccessMode.GetPermission( mode4, 0 );
    IF perm.k1 # 0 OR perm.k2 # 1 THEN RETURN FALSE; END;
    IF mode4 # mode1 THEN RETURN FALSE; END;

    mode4 := SMP.Mediate( 1, 0, s, o );
    IF s # 1 OR o # 0 THEN RETURN FALSE; END;
    perm := AccessMode.GetPermission( mode4, 0 );
    IF perm.k1 # 1 OR perm.k2 # 0 THEN RETURN FALSE; END;
    IF mode4 # mode2 THEN RETURN FALSE; END;

    mode4 := SMP.Mediate( 1, 1, s, o );
    IF s # 1 OR o # 1 THEN RETURN FALSE; END;
    perm := AccessMode.GetPermission( mode4, 0 );
    IF perm.k1 # 1 OR perm.k2 # 1 THEN RETURN FALSE; END;
    IF mode4 # mode3 THEN RETURN FALSE; END;

    mode4 := SMP.Mediate( 0, 2, s, o );
    IF s # 0 OR o # 2 THEN RETURN FALSE; END;
    perm := AccessMode.GetPermission( mode4, 0 );
    IF perm.k1 # 0 OR perm.k2 # 2 THEN RETURN FALSE; END;

    mode4 := SMP.Mediate( 0, 0, s, o );
    IF s # 0 OR o # 0 THEN RETURN FALSE; END;
    perm := AccessMode.GetPermission( mode4, 0 );
    IF perm.k1 # 0 OR perm.k2 # 0 THEN RETURN FALSE; END;
    IF mode4 = mode0 THEN RETURN FALSE; END;

    RETURN TRUE;
  END T3;

PROCEDURE T4() : BOOLEAN =
  BEGIN
    TRY
      SecurityManagerPrivate.SetMediationCacheSize( 8, 8 );
    EXCEPT
    | SecurityError.T => RETURN FALSE;
    END;

    RETURN TRUE;
  END T4;

PROCEDURE T5() : BOOLEAN =
  VAR
    mode0 : AccessMode.T;
    mode1 : AccessMode.T;
    mode2 : AccessMode.T;
    mode3 : AccessMode.T;
    mode4 : AccessMode.T;
    perm  : PermT;
    s, o  : SecurityManager.SID;
  BEGIN
    SecurityManagerPrivate.ClearMediationCache();

    (* Fill mediation cache to capacity *)
    FOR i := 0 TO 7 DO
      FOR j := 0 TO 7 DO
        (* Mediate once *)
        mode1 := SMP.Mediate( i, j, s, o );
        IF s # i OR o # j THEN RETURN FALSE; END;
        perm := AccessMode.GetPermission( mode1, 0 );
        IF perm.k1 # i OR perm.k2 # j THEN RETURN FALSE; END;
        (* Mediate twice, this time from cache *)
        mode2 := SMP.Mediate( i, j, s, o );
        IF mode1 # mode2 THEN RETURN FALSE; END;
      END;
    END;

    (* Satisfied from cache *)
    mode0 := SMP.Mediate( 0, 0, s, o );
    IF s # 0 OR o # 0 THEN RETURN FALSE; END;
    perm := AccessMode.GetPermission( mode0, 0 );
    IF perm.k1 # 0 OR perm.k2 # 0 THEN RETURN FALSE; END;
    
    (* Satisfied from cache *)
    mode1 := SMP.Mediate( 0, 1, s, o );
    IF s # 0 OR o # 1 THEN RETURN FALSE; END;
    perm := AccessMode.GetPermission( mode1, 0 );
    IF perm.k1 # 0 OR perm.k2 # 1 THEN RETURN FALSE; END;

    (* Satisfied from cache *)
    mode4 := SMP.Mediate( 0, 2, s, o );
    IF s # 0 OR o # 2 THEN RETURN FALSE; END;
    perm := AccessMode.GetPermission( mode4, 0 );
    IF perm.k1 # 0 OR perm.k2 # 2 THEN RETURN FALSE; END;

    (* Boot 0,1 entry from cache *)
    IF NOT SecurityManagerPrivate.ClearMediationCacheEntry( 0, 1 ) THEN
      RETURN FALSE;
    END;

    (* Added to cache *)
    mode2 := SMP.Mediate( 8, 8, s, o );
    IF s # 8 OR o # 8 THEN RETURN FALSE; END;
    perm := AccessMode.GetPermission( mode2, 0 );
    IF perm.k1 # 8 OR perm.k2 # 8 THEN RETURN FALSE; END;
    
    (* Satisfied from cache *)
    mode3 := SMP.Mediate( 0, 0, s, o );
    IF s # 0 OR o # 0 THEN RETURN FALSE; END;
    perm := AccessMode.GetPermission( mode3, 0 );
    IF perm.k1 # 0 OR perm.k2 # 0 THEN RETURN FALSE; END;
    IF mode3 # mode0 THEN RETURN FALSE; END;

    (* Satisfied from cache *)
    mode3 := SMP.Mediate( 8, 8, s, o );
    IF s # 8 OR o # 8 THEN RETURN FALSE; END;
    perm := AccessMode.GetPermission( mode3, 0 );
    IF perm.k1 # 8 OR perm.k2 # 8 THEN RETURN FALSE; END;
    IF mode3 # mode2 THEN RETURN FALSE; END;

    (* Replaces 8,8 entry in cache *)
    mode3 := SMP.Mediate( 0, 1, s, o );
    IF s # 0 OR o # 1 THEN RETURN FALSE; END;
    perm := AccessMode.GetPermission( mode3, 0 );
    IF perm.k1 # 0 OR perm.k2 # 1 THEN RETURN FALSE; END;
    IF mode3 = mode1 THEN RETURN FALSE; END;
    mode1 := mode3;

    (* Replaces 0,2 entry in cache since 0,1 entry is marked as used *)
    mode3 := SMP.Mediate( 8, 8, s, o );
    IF s # 8 OR o # 8 THEN RETURN FALSE; END;
    perm := AccessMode.GetPermission( mode3, 0 );
    IF perm.k1 # 8 OR perm.k2 # 8 THEN RETURN FALSE; END;
    IF mode3 = mode2 THEN RETURN FALSE; END;
    
    (* Satisfied from cache *)
    mode3 := SMP.Mediate( 0, 1, s, o );
    IF s # 0 OR o # 1 THEN RETURN FALSE; END;
    perm := AccessMode.GetPermission( mode3, 0 );
    IF perm.k1 # 0 OR perm.k2 # 1 THEN RETURN FALSE; END;
    IF mode3 # mode1 THEN RETURN FALSE; END;

    (* Requires new cache entry *)
    mode3 := SMP.Mediate( 0, 2, s, o );
    IF s # 0 OR o # 2 THEN RETURN FALSE; END;
    perm := AccessMode.GetPermission( mode3, 0 );
    IF perm.k1 # 0 OR perm.k2 # 2 THEN RETURN FALSE; END;
    IF mode3 = mode4 THEN RETURN FALSE; END;

    RETURN TRUE;
  END T5;

PROCEDURE T6() : BOOLEAN =
  VAR
    mode0 : AccessMode.T;
    mode1 : AccessMode.T;
    mode2 : AccessMode.T;
    mode3 : AccessMode.T;
    perm  : PermT;
    s, o  : SecurityManager.SID;
  BEGIN
    SecurityManagerPrivate.ClearMediationCache();

    (* Fill mediation cache to capacity *)
    FOR i := 0 TO 7 DO
      FOR j := 0 TO 7 DO
        (* Mediate once *)
        mode1 := SMP.Mediate( i, j, s, o );
        IF s # i OR o # j THEN RETURN FALSE; END;
        perm := AccessMode.GetPermission( mode1, 0 );
        IF perm.k1 # i OR perm.k2 # j THEN RETURN FALSE; END;
        (* Mediate twice, this time from cache *)
        mode2 := SMP.Mediate( i, j, s, o );
        IF mode1 # mode2 THEN RETURN FALSE; END;
      END;
    END;

    (* Satisfied from cache *)
    mode0 := SMP.Mediate( 0, 0, s, o );
    IF s # 0 OR o # 0 THEN RETURN FALSE; END;
    perm := AccessMode.GetPermission( mode0, 0 );
    IF perm.k1 # 0 OR perm.k2 # 0 THEN RETURN FALSE; END;
    
    (* Satisfied from cache *)
    mode1 := SMP.Mediate( 0, 1, s, o );
    IF s # 0 OR o # 1 THEN RETURN FALSE; END;
    perm := AccessMode.GetPermission( mode1, 0 );
    IF perm.k1 # 0 OR perm.k2 # 1 THEN RETURN FALSE; END;

    (* Boot 0,1 entry from cache *)
    IF NOT SecurityManagerPrivate.ClearMediationCacheEntry( 0, 1 ) THEN
      RETURN FALSE;
    END;

    (* Added to cache *)
    mode2 := SMP.Mediate( 8, 8, s, o );
    IF s # 8 OR o # 8 THEN RETURN FALSE; END;
    perm := AccessMode.GetPermission( mode2, 0 );
    IF perm.k1 # 8 OR perm.k2 # 8 THEN RETURN FALSE; END;
    
    (* Satisfied from cache *)
    mode3 := SMP.Mediate( 0, 0, s, o );
    IF s # 0 OR o # 0 THEN RETURN FALSE; END;
    perm := AccessMode.GetPermission( mode3, 0 );
    IF perm.k1 # 0 OR perm.k2 # 0 THEN RETURN FALSE; END;
    IF mode3 # mode0 THEN RETURN FALSE; END;

    (* Satisfied from cache *)
    mode3 := SMP.Mediate( 8, 8, s, o );
    IF s # 8 OR o # 8 THEN RETURN FALSE; END;
    perm := AccessMode.GetPermission( mode3, 0 );
    IF perm.k1 # 8 OR perm.k2 # 8 THEN RETURN FALSE; END;
    IF mode3 # mode2 THEN RETURN FALSE; END;

    (* Boot 0,0 entry from cache *)
    IF NOT SecurityManagerPrivate.ClearMediationCacheEntry( 0, 0 ) THEN
      RETURN FALSE;
    END;

    (* Added to cache *)
    mode3 := SMP.Mediate( 0, 1, s, o );
    IF s # 0 OR o # 1 THEN RETURN FALSE; END;
    perm := AccessMode.GetPermission( mode3, 0 );
    IF perm.k1 # 0 OR perm.k2 # 1 THEN RETURN FALSE; END;
    IF mode3 = mode1 THEN RETURN FALSE; END;
    mode1 := mode3;

    (* Satisfied from cache *)
    mode3 := SMP.Mediate( 8, 8, s, o );
    IF s # 8 OR o # 8 THEN RETURN FALSE; END;
    perm := AccessMode.GetPermission( mode3, 0 );
    IF perm.k1 # 8 OR perm.k2 # 8 THEN RETURN FALSE; END;
    IF mode3 # mode2 THEN RETURN FALSE; END;

    (* Replaces 8,8 entry in cache *)
    mode3 := SMP.Mediate( 0, 0, s, o );
    IF s # 0 OR o # 0 THEN RETURN FALSE; END;
    perm := AccessMode.GetPermission( mode3, 0 );
    IF perm.k1 # 0 OR perm.k2 # 0 THEN RETURN FALSE; END;
    IF mode3 = mode0 THEN RETURN FALSE; END;

    (* Replaces 0,1 entry in cache *)
    mode3 := SMP.Mediate( 8, 8, s, o );
    IF s # 8 OR o # 8 THEN RETURN FALSE; END;
    perm := AccessMode.GetPermission( mode3, 0 );
    IF perm.k1 # 8 OR perm.k2 # 8 THEN RETURN FALSE; END;
    IF mode3 = mode2 THEN RETURN FALSE; END;
    
    (* Added to cache *)
    mode3 := SMP.Mediate( 0, 1, s, o );
    IF s # 0 OR o # 1 THEN RETURN FALSE; END;
    perm := AccessMode.GetPermission( mode3, 0 );
    IF perm.k1 # 0 OR perm.k2 # 1 THEN RETURN FALSE; END;
    IF mode3 = mode1 THEN RETURN FALSE; END;

    RETURN TRUE;
  END T6;

PROCEDURE T7() : BOOLEAN =
  VAR
    mode1 : AccessMode.T;
    mode2 : AccessMode.T;
    perm  : PermT;
    s, o  : SecurityManager.SID;
    start : INTEGER;
    ticks : INTEGER;
  BEGIN
    SecurityManagerPrivate.ClearMediationCache();

    (* Fill mediation cache to capacity *)
    FOR i := 0 TO 7 DO
      FOR j := 0 TO 7 DO
        (* Mediate once *)
        mode1 := SMP.Mediate( i, j, s, o );
        IF s # i OR o # j THEN RETURN FALSE; END;
        perm := AccessMode.GetPermission( mode1, 0 );
        IF perm.k1 # i OR perm.k2 # j THEN RETURN FALSE; END;
        (* Mediate twice, this time from cache *)
        mode2 := SMP.Mediate( i, j, s, o );
        IF mode1 # mode2 THEN RETURN FALSE; END;
      END;
    END;

    (* Add entry that will timeout *)
    mode1 := SMP.Mediate( 1024, 1024, s, o );
    IF s # 1024 OR o # 1024 THEN RETURN FALSE; END;
    perm := AccessMode.GetPermission( mode1, 0 );
    IF perm.k1 # 1024 OR perm.k2 # 1024 THEN RETURN FALSE; END;

    (* Get entry again just to be sure *)
    mode2 := SMP.Mediate( 1024, 1024, s, o );
    IF s # 1024 OR o # 1024 THEN RETURN FALSE; END;
    perm := AccessMode.GetPermission( mode2, 0 );
    IF perm.k1 # 1024 OR perm.k2 # 1024 THEN RETURN FALSE; END;
    IF mode1 # mode2 THEN RETURN FALSE; END;

    (* Now, wait until entry should have timeout-ed, but keep cache busy *)
    start := Clock.ReadTicks();

    LOOP
      mode2 := SMP.Mediate( 1, 1, s, o );
      IF s # 1 OR o # 1 THEN RETURN FALSE; END;
      perm := AccessMode.GetPermission( mode2, 0 );
      IF perm.k1 # 1 OR perm.k2 # 1 THEN RETURN FALSE; END;
      
      (* Should we terminate? *)
      ticks := Clock.ReadTicks();
      IF ticks - start > CHECK THEN EXIT; END;
    END;

    (* Did the entry really timeout ? *)
    mode2 := SMP.Mediate( 1024, 1024, s, o );
    IF s # 1024 OR o # 1024 THEN RETURN FALSE; END;
    perm := AccessMode.GetPermission( mode2, 0 );
    IF perm.k1 # 1024 OR perm.k2 # 1024 THEN RETURN FALSE; END;
    IF mode1 = mode2 THEN RETURN FALSE; END;

    RETURN TRUE;
  END T7;

PROCEDURE T8() : BOOLEAN =
  BEGIN
    TRY
      Dispatcher.AtomicSwap( medNew1, medNew2 );
    EXCEPT
    | Dispatcher.Error =>
      BEGIN
        IO.Put("*** Couldn't swap handlers on SecurityPolicy.Mediate ***\n");
        RETURN FALSE;
      END;
    END;

    baseSubjectSid := SecurityManager.GetCurrentSubjectSid();
    baseObjectSid  := SecurityManager.GetCurrentObjectSid();
    SecurityManagerPrivate.Reset();

    IF DEBUG THEN
      RTIO.PutText(" baseSubjectSid " & Fmt.Int(baseSubjectSid) & "\n");
      RTIO.PutText(" baseObjectSid  " & Fmt.Int(baseObjectSid ) & "\n");
      SecurityManagerPrivate.DumpCurrentSIDStack();
    END;

    RETURN TRUE;
  END T8;

PROCEDURE T9() : BOOLEAN =
  BEGIN
    IF ManagerTest.Step( 1 ) THEN RETURN FALSE; END;
    IF    SecurityManager.GetCurrentSubjectSid() # baseSubjectSid
       OR SecurityManager.GetCurrentObjectSid () # baseObjectSid  THEN
      RETURN FALSE;
    END;
    IF DEBUG THEN
      SecurityManagerPrivate.DumpCurrentSIDStack();
    END;
    RETURN TRUE;
  END T9;

PROCEDURE T10() : BOOLEAN =
  VAR
    ok : BOOLEAN;
  BEGIN
    TRY
      ok := NOT ManagerTest.StepTwo( 1, FALSE );
    EXCEPT
    | DasEndeDerWelt => RETURN FALSE;
    END;
    IF NOT ok THEN RETURN FALSE; END;
    IF    SecurityManager.GetCurrentSubjectSid() # baseSubjectSid
       OR SecurityManager.GetCurrentObjectSid () # baseObjectSid  THEN
      RETURN FALSE;
    END;
    IF DEBUG THEN
      SecurityManagerPrivate.DumpCurrentSIDStack();
    END;
    
    TRY
      EVAL ManagerTest.StepTwo( 1, TRUE );
      ok := FALSE;
    EXCEPT
    | DasEndeDerWelt => ok := TRUE;
    END;
    IF NOT ok THEN RETURN FALSE; END;
    IF    SecurityManager.GetCurrentSubjectSid() # baseSubjectSid
       OR SecurityManager.GetCurrentObjectSid () # baseObjectSid  THEN
      RETURN FALSE;
    END;
    IF DEBUG THEN
      SecurityManagerPrivate.DumpCurrentSIDStack();
    END;

    TRY
      ok := NOT ManagerTest.StepThree( 1, FALSE );
    EXCEPT
    | DasEndeDerWelt => RETURN FALSE;
    END;
    IF NOT ok THEN RETURN FALSE; END;
    IF    SecurityManager.GetCurrentSubjectSid() # baseSubjectSid
       OR SecurityManager.GetCurrentObjectSid () # baseObjectSid  THEN
      RETURN FALSE;
    END;
    IF DEBUG THEN
      SecurityManagerPrivate.DumpCurrentSIDStack();
    END;
    
    TRY
      EVAL ManagerTest.StepThree( 1, TRUE );
      ok := FALSE;
    EXCEPT
    | DasEndeDerWelt => ok := TRUE;
    END;
    IF NOT ok THEN RETURN FALSE; END;
    IF    SecurityManager.GetCurrentSubjectSid() # baseSubjectSid
       OR SecurityManager.GetCurrentObjectSid () # baseObjectSid  THEN
      RETURN FALSE;
    END;
    IF DEBUG THEN
      SecurityManagerPrivate.DumpCurrentSIDStack();
    END;

    RETURN TRUE;
  END T10;

PROCEDURE T11() : BOOLEAN =
  BEGIN
    IF NOT PingSetup() THEN RETURN FALSE; END;
    IF DEBUG THEN
      SecurityManagerPrivate.DumpCurrentSIDStack();
    END;
    RETURN TRUE;
  END T11;

PROCEDURE T12() : BOOLEAN =
  VAR
    x : PingPongT;
  BEGIN
    x := NEW( PingPongT );
    IF ManagerTest.PingTwo( x, 1 ) THEN RETURN FALSE; END;
    IF DEBUG THEN
      SecurityManagerPrivate.DumpCurrentSIDStack();
    END;
    RETURN TRUE;
  END T12;

PROCEDURE T13() : BOOLEAN =
  VAR
    s   : SecureT;
    ok  : BOOLEAN;
    sid : SecurityManager.SID;
  BEGIN
    SecurityManagerPrivate.SetTypeSecurity(TYPECODE(SecureT), FALSE);
    s  := NEW( SecureT, secretKey := 123 );
    ok := FALSE;
    TRY
      sid := SecurityManager.GetSid( s );
      IF DEBUG THEN
        IO.Put(" T13: Got object sid " & Fmt.Int(sid) &
          " for insecure object\n");
      END;
    EXCEPT
    | SecurityError.T => ok := TRUE;
    END;
    IF NOT ok THEN RETURN FALSE; END;
    SecurityManagerPrivate.SetTypeSecurity(TYPECODE(SecureT), TRUE);
    IF ManagerTest.StepFour( 1 ) THEN RETURN FALSE; END;
    SecurityManagerPrivate.SetTypeSecurity(TYPECODE(SecureT), FALSE);
    s  := NEW( SecureT, secretKey := 123 );
    ok := FALSE;
    TRY
      sid := SecurityManager.GetSid( s );
      IF DEBUG THEN
        IO.Put(" T13: Got object sid " & Fmt.Int(sid) &
          " for insecure object\n");
      END;
      EXCEPT
      | SecurityError.T => ok := TRUE;
      END;
    IF NOT ok THEN RETURN FALSE; END;
    RETURN TRUE;
  END T13;

BEGIN
END ManagerTest.

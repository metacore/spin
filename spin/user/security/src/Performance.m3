(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *
 * 10-Nov-97  Robert Grimm (rgrimm) at the University of Washington
 *      Created.
 *
 *)

MODULE Performance;

IMPORT Dispatcher, CPU, Spy;
IMPORT SecurityError, Permission, AccessMode;
IMPORT SecurityManager, SecurityManagerPrivate, SecurityPolicy;
IMPORT IO;

IMPORT Performance;


(*
    Constant Pool
   ---------------
 *)

CONST
  MODE_X    = AccessMode.SimpleT{ AccessMode.EXECUTE };

  WORK_NONE = SecurityManagerPrivate.AccessControlT{};
  WORK_D    = SecurityManagerPrivate.AccessControlT{
                SecurityManagerPrivate.AccessCheckT.DomainTransfer  };
  WORK_X    = SecurityManagerPrivate.AccessControlT{
                SecurityManagerPrivate.AccessCheckT.CodeCheck       };
  WORK_DX   = SecurityManagerPrivate.AccessControlT{
                SecurityManagerPrivate.AccessCheckT.DomainTransfer,
                SecurityManagerPrivate.AccessCheckT.CodeCheck       };
  WORK_C    = SecurityManagerPrivate.AccessControlT{
                SecurityManagerPrivate.AccessCheckT.ObjectCheck     };
  WORK_DC   = SecurityManagerPrivate.AccessControlT{
                SecurityManagerPrivate.AccessCheckT.DomainTransfer,
                SecurityManagerPrivate.AccessCheckT.ObjectCheck     };

  PERM_NUM  = 10;

  OBJ_NUM   = 8;

  TEST1     = "Null";
  TEST2     = "Null D";
  TEST3     = "Null X1";
  TEST4     = "Null X10";
  TEST5     = "Null DX1";
  TEST6     = "Null DX10";
  TEST10    = "1Arg";
  TEST11    = "2Arg";
  TEST12    = "4Arg";
  TEST13    = "8Arg";

  COLD      = " Cold";
  WARM      = " Warm";
  PERM1     = " C1";
  PERM10    = " C10";
  DOMAIN    = " D";


(*
    Global Variables
   ------------------
 *)

VAR
  amAll          : AccessMode.T;
  amRequired1    : AccessMode.T;
  amRequired10   : AccessMode.T;
  init           : BOOLEAN := FALSE;
  medDefault     : REF ARRAY OF Dispatcher.Binding;
  medNew         : REF ARRAY OF Dispatcher.Binding;
  bindings       : REF ARRAY OF Dispatcher.Binding;
  o              : REF ARRAY OF SecureT;


(*
    Permission Objects
   --------------------
 *)

TYPE
  T1   = [1..32];
  SetT = SET OF T1;

CONST
  FULL_SET = SetT{ 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,
                   19,20,21,22,23,24,25,26,27,28,29,30,31,32 };

TYPE
  PermT = Permission.T BRANDED "10" OBJECT
    key : SetT;
  OVERRIDES
    contains := PermContain;
    toText   := PermToText;
  END;

  P1 = Permission.T BRANDED "1" OBJECT key : SetT;
  OVERRIDES contains := PermContain1; toText := PermToText1;
  END;

  P2 = Permission.T BRANDED "2" OBJECT key : SetT;
  OVERRIDES contains := PermContain2; toText := PermToText2;
  END;

  P3 = Permission.T BRANDED "3" OBJECT key : SetT;
  OVERRIDES contains := PermContain3; toText := PermToText3;
  END;

  P4 = Permission.T BRANDED "4" OBJECT key : SetT;
  OVERRIDES contains := PermContain4; toText := PermToText4;
  END;

  P5 = Permission.T BRANDED "5" OBJECT key : SetT;
  OVERRIDES contains := PermContain5; toText := PermToText5;
  END;

  P6 = Permission.T BRANDED "6" OBJECT key : SetT;
  OVERRIDES contains := PermContain6; toText := PermToText6;
  END;

  P7 = Permission.T BRANDED "7" OBJECT key : SetT;
  OVERRIDES contains := PermContain7; toText := PermToText7;
  END;

  P8 = Permission.T BRANDED "8" OBJECT key : SetT;
  OVERRIDES contains := PermContain8; toText := PermToText8;
  END;

  P9 = Permission.T BRANDED "9" OBJECT key : SetT;
  OVERRIDES contains := PermContain9; toText := PermToText9;
  END;

PROCEDURE PermContain( self : PermT; other : Permission.T )
  : BOOLEAN =
  BEGIN 
    WITH o = NARROW( other, PermT ) DO
      RETURN self.key >= o.key;
    END;
  END PermContain;
  
PROCEDURE PermToText( <*UNUSED*> self : PermT ) : TEXT =
  BEGIN
    RETURN "I am a PermT";
  END PermToText;

PROCEDURE PermContain1( self : P1; other : Permission.T ) : BOOLEAN =
  BEGIN
    WITH o = NARROW(other,P1) DO RETURN self.key >= o.key; END;
  END PermContain1;

PROCEDURE PermToText1( <*UNUSED*> self : P1 ) : TEXT = 
  BEGIN
    RETURN "";
  END PermToText1;

PROCEDURE PermContain2( self : P2; other : Permission.T ) : BOOLEAN =
  BEGIN
    WITH o = NARROW(other,P2) DO RETURN self.key >= o.key; END;
  END PermContain2;

PROCEDURE PermToText2( <*UNUSED*> self : P2 ) : TEXT = 
  BEGIN
    RETURN "";
  END PermToText2;

PROCEDURE PermContain3( self : P3; other : Permission.T ) : BOOLEAN =
  BEGIN
    WITH o = NARROW(other,P3) DO RETURN self.key >= o.key; END;
  END PermContain3;

PROCEDURE PermToText3( <*UNUSED*> self : P3 ) : TEXT = 
  BEGIN
    RETURN "";
  END PermToText3;

PROCEDURE PermContain4( self : P4; other : Permission.T ) : BOOLEAN =
  BEGIN
    WITH o = NARROW(other,P4) DO RETURN self.key >= o.key; END;
  END PermContain4;

PROCEDURE PermToText4( <*UNUSED*> self : P4 ) : TEXT = 
  BEGIN
    RETURN "";
  END PermToText4;

PROCEDURE PermContain5( self : P5; other : Permission.T ) : BOOLEAN =
  BEGIN
    WITH o = NARROW(other,P5) DO RETURN self.key >= o.key; END;
  END PermContain5;

PROCEDURE PermToText5( <*UNUSED*> self : P5 ) : TEXT = 
  BEGIN
    RETURN "";
  END PermToText5;

PROCEDURE PermContain6( self : P6; other : Permission.T ) : BOOLEAN =
  BEGIN
    WITH o = NARROW(other,P6) DO RETURN self.key >= o.key; END;
  END PermContain6;

PROCEDURE PermToText6( <*UNUSED*> self : P6 ) : TEXT = 
  BEGIN
    RETURN "";
  END PermToText6;

PROCEDURE PermContain7( self : P7; other : Permission.T ) : BOOLEAN =
  BEGIN
    WITH o = NARROW(other,P7) DO RETURN self.key >= o.key; END;
  END PermContain7;

PROCEDURE PermToText7( <*UNUSED*> self : P7 ) : TEXT = 
  BEGIN
    RETURN "";
  END PermToText7;

PROCEDURE PermContain8( self : P8; other : Permission.T ) : BOOLEAN =
  BEGIN
    WITH o = NARROW(other,P8) DO RETURN self.key >= o.key; END;
  END PermContain8;

PROCEDURE PermToText8( <*UNUSED*> self : P8 ) : TEXT = 
  BEGIN
    RETURN "";
  END PermToText8;

PROCEDURE PermContain9( self : P9; other : Permission.T ) : BOOLEAN =
  BEGIN
    WITH o = NARROW(other,P9) DO RETURN self.key >= o.key; END;
  END PermContain9;

PROCEDURE PermToText9( <*UNUSED*> self : P9 ) : TEXT = 
  BEGIN
    RETURN "";
  END PermToText9;


(*
    A secure object
   -----------------
 *)

REVEAL
  SecureT = BRANDED Brand REF RECORD
    secretKey : INTEGER;
  END;


(*
    Mediation
   -----------
 *)

PROCEDURE Mediate(     k1      : SecurityManager.SID;
                   <*UNUSED*> k2 : SecurityManager.SID;
                   VAR mode    : AccessMode.T;
                   VAR s, o    : SecurityManager.SID;
                   VAR timeout : INTEGER;             ) : BOOLEAN =
  (* Put the two keys into the permission object *)
  BEGIN
    mode    := amAll;
    s       := k1 + 1;
    o       := SecurityManager.GetCurrentObjectSid();
    timeout := 0;
    RETURN TRUE;
  END Mediate;


(*
    Run Performance Tests
   -----------------------
 *)

PROCEDURE Run() =
  (* Run all performance tests. *)
  VAR
    start, stop : INTEGER;
    spy         : Spy.T;
  BEGIN
    (* Test only makes sense if type security is enabled. *)
    IF NOT SecurityManagerPrivate.CheckTypeSecurity() THEN
      IO.Put("*** Type security is disabled ***\n");
      RETURN;
    END;

    SecurityManagerPrivate.SetTypeSecurity(TYPECODE(SecureT), TRUE);

    IF NOT init THEN
      TRY
        medDefault    := NEW( REF ARRAY OF Dispatcher.Binding, 1 );
        medNew        := NEW( REF ARRAY OF Dispatcher.Binding, 1 );
        medDefault[0] := Dispatcher.GetOriginalHandler(
                             SecurityPolicy.Mediate);
        medNew[0]     := Dispatcher.Create( SecurityPolicy.Mediate, NIL,
                                            Mediate );
      EXCEPT
      | Dispatcher.Error =>
        BEGIN
          IO.Put("*** Couldn't get handler on SecurityPolicy.Mediate");
          IO.Put(" ***\n");
          RETURN;
        END;
      END;

      VAR
        list : Permission.ListT;
      BEGIN
        amRequired1    := AccessMode.Create( MODE_X );
        list           := NEW( REF ARRAY OF Permission.T, PERM_NUM );
        list[0]        := NEW( PermT, key := SetT{10} );
        amRequired10   := AccessMode.Create( AccessMode.SimpleT{}, list );
        list[0]        := NEW( P1,    key := FULL_SET );
        list[1]        := NEW( P2,    key := FULL_SET );
        list[2]        := NEW( P3,    key := FULL_SET );
        list[3]        := NEW( P4,    key := FULL_SET );
        list[4]        := NEW( P5,    key := FULL_SET );
        list[5]        := NEW( P6,    key := FULL_SET );
        list[6]        := NEW( P7,    key := FULL_SET );
        list[7]        := NEW( P8,    key := FULL_SET );
        list[8]        := NEW( P9,    key := FULL_SET );
        list[9]        := NEW( PermT, key := FULL_SET );
        amAll          := AccessMode.Create( AccessMode.FullSimpleAccess,
                                             list );
      END;

      o      := NEW( REF ARRAY OF SecureT, OBJ_NUM );
      FOR i := 0 TO OBJ_NUM - 1 DO
        o[i] := NEW( SecureT, secretKey := i );
      END;
      
      bindings     := NEW( REF ARRAY OF Dispatcher.Binding, 4 );
      VAR
        binding1, binding2, binding3 : Dispatcher.Binding;
        binding4, binding5, binding6 : Dispatcher.Binding;
      BEGIN
        TRY
          binding1    := Dispatcher.GetOriginalHandler(Performance.Null );
          binding2    := Dispatcher.GetOriginalHandler(Performance.Null2);
          binding3    := Dispatcher.GetOriginalHandler(Performance.Null3);
          binding4    := Dispatcher.GetOriginalHandler(Performance.Null4);
          binding5    := Dispatcher.GetOriginalHandler(Performance.Null5);
          binding6    := Dispatcher.GetOriginalHandler(Performance.Null6);
          bindings[0] := Dispatcher.GetOriginalHandler(Performance.One  );
          bindings[1] := Dispatcher.GetOriginalHandler(Performance.Two  );
          bindings[2] := Dispatcher.GetOriginalHandler(Performance.Four );
          bindings[3] := Dispatcher.GetOriginalHandler(Performance.Eight);
        EXCEPT
        | Dispatcher.Error =>
          BEGIN
            IO.Put("*** Couldn't get handlers on Performance.Null and");
            IO.Put(" One/Two/Four/Eight ***\n");
            RETURN;
          END;
        END;
        
        TRY
          SecurityManagerPrivate.SetAccessControl( binding1,
                                                   WORK_D    );
          SecurityManagerPrivate.SetAccessControl( binding2,
                                                   WORK_D    );
          SecurityManagerPrivate.SetAccessControl( binding3,
                                                   WORK_X,
                                                   amRequired1 );
          SecurityManagerPrivate.SetAccessControl( binding4,
                                                   WORK_X,
                                                   amRequired10 );
          SecurityManagerPrivate.SetAccessControl( binding5,
                                                   WORK_DX,
                                                   amRequired1 );
          SecurityManagerPrivate.SetAccessControl( binding6,
                                                   WORK_DX,
                                                   amRequired10 );
        EXCEPT
        | SecurityError.T =>
          BEGIN
            IO.Put("*** Couldn't install access control on Performance.");
            IO.Put("Null ***\n");
            RETURN;
          END;
        END;
      END;

      init := TRUE;
    END;

    (* Initialize global state. *)

    TRY
      Dispatcher.AtomicSwap( medDefault, medNew );
    EXCEPT
    | Dispatcher.Error =>
      BEGIN
        IO.Put("*** Couln't swap handlers on SecurityPolicy.Mediate ***\n");
        RETURN;
      END;
    END;

    SecurityManagerPrivate.Reset();

    (* Execute the step test to help with instruction counting *)
    
    IO.Put("Stepping...\n");
    Step(); (* Warm up cache            *)
    Step(); (* Single step through here *)

    (* Execute the actual performance tests. *)

    IO.Put(TEST1 & COLD & "\n");
    spy := Spy.Create(TEST1 & COLD);
    Performance.Null1();
    FOR i := 1 TO N_ITER DO
      CPU.FlushInstructionCache();
      CPU.FlushDataCache();
      start := CPU.CycleCounter();
      Performance.Null1();
      stop  := CPU.CycleCounter();
      Spy.Hit( spy, 1000 * start, 1000 * stop );
    END;

    IO.Put(TEST1 & WARM & "\n");
    spy := Spy.Create(TEST1 & WARM);
    Performance.Null1();
    start := CPU.CycleCounter();
    FOR i := 1 TO N_ITER DO
      Performance.Null1();
    END;
    stop := CPU.CycleCounter();
    Spy.Hit( spy, start, stop );

    IO.Put(TEST2 & COLD & "\n");
    spy := Spy.Create(TEST2 & COLD);
    Performance.Null2();
    FOR i := 1 TO N_ITER DO
      CPU.FlushInstructionCache();
      CPU.FlushDataCache();
      start := CPU.CycleCounter();
      Performance.Null2();
      stop  := CPU.CycleCounter();
      Spy.Hit( spy, 1000 * start, 1000 * stop );
    END;

    IO.Put(TEST2 & WARM & "\n");
    spy := Spy.Create(TEST2 & WARM);
    Performance.Null2();
    start := CPU.CycleCounter();
    FOR i := 1 TO N_ITER DO
      Performance.Null2();
    END;
    stop := CPU.CycleCounter();
    Spy.Hit( spy, start, stop );

    IO.Put(TEST3 & COLD & "\n");
    spy := Spy.Create(TEST3 & COLD);
    Performance.Null3();
    FOR i := 1 TO N_ITER DO
      CPU.FlushInstructionCache();
      CPU.FlushDataCache();
      start := CPU.CycleCounter();
      Performance.Null3();
      stop  := CPU.CycleCounter();
      Spy.Hit( spy, 1000 * start, 1000 * stop );
    END;

    IO.Put(TEST3 & WARM & "\n");
    spy := Spy.Create(TEST3 & WARM);
    Performance.Null3();
    start := CPU.CycleCounter();
    FOR i := 1 TO N_ITER DO
      Performance.Null3();
    END;
    stop := CPU.CycleCounter();
    Spy.Hit( spy, start, stop );

    IO.Put(TEST4 & COLD & "\n");
    spy := Spy.Create(TEST4 & COLD);
    Performance.Null4();
    FOR i := 1 TO N_ITER DO
      CPU.FlushInstructionCache();
      CPU.FlushDataCache();
      start := CPU.CycleCounter();
      Performance.Null4();
      stop  := CPU.CycleCounter();
      Spy.Hit( spy, 1000 * start, 1000 * stop );
    END;

    IO.Put(TEST4 & WARM & "\n");
    spy := Spy.Create(TEST4 & WARM);
    Performance.Null4();
    start := CPU.CycleCounter();
    FOR i := 1 TO N_ITER DO
      Performance.Null4();
    END;
    stop := CPU.CycleCounter();
    Spy.Hit( spy, start, stop );

    IO.Put(TEST5 & COLD & "\n");
    spy := Spy.Create(TEST5 & COLD);
    Performance.Null5();
    FOR i := 1 TO N_ITER DO
      CPU.FlushInstructionCache();
      CPU.FlushDataCache();
      start := CPU.CycleCounter();
      Performance.Null5();
      stop  := CPU.CycleCounter();
      Spy.Hit( spy, 1000 * start, 1000 * stop );
    END;

    IO.Put(TEST5 & WARM & "\n");
    spy := Spy.Create(TEST5 & WARM);
    Performance.Null5();
    start := CPU.CycleCounter();
    FOR i := 1 TO N_ITER DO
      Performance.Null5();
    END;
    stop := CPU.CycleCounter();
    Spy.Hit( spy, start, stop );

    IO.Put(TEST6 & COLD & "\n");
    spy := Spy.Create(TEST6 & COLD);
    Performance.Null6();
    FOR i := 1 TO N_ITER DO
      CPU.FlushInstructionCache();
      CPU.FlushDataCache();
      start := CPU.CycleCounter();
      Performance.Null6();
      stop  := CPU.CycleCounter();
      Spy.Hit( spy, 1000 * start, 1000 * stop );
    END;

    IO.Put(TEST6 & WARM & "\n");
    spy := Spy.Create(TEST6 & WARM);
    Performance.Null6();
    start := CPU.CycleCounter();
    FOR i := 1 TO N_ITER DO
      Performance.Null6();
    END;
    stop := CPU.CycleCounter();
    Spy.Hit( spy, start, stop );

    VAR
      args : INTEGER := 1;
      list : AccessMode.ListT;
    BEGIN
      FOR i := FIRST(bindings^) TO LAST(bindings^) DO
        list      := NEW( AccessMode.ListT, args );
        FOR j := FIRST(list^) TO LAST(list^) DO
          list[j] := amRequired1;
        END;
        TRY
          SecurityManagerPrivate.SetAccessControl( bindings[i],
                                                   WORK_NONE    );
        EXCEPT
        | SecurityError.T =>
          BEGIN
            IO.Put("*** Couldn't install access control on One/Two/Four");
            IO.Put("/Eight ***\n");
            RETURN;
          END;
        END;
        args := args * 2;
      END;
    END;

    IO.Put(TEST10 & COLD & "\n");
    spy := Spy.Create(TEST10 & COLD);
    Performance.One(o[0]);
    FOR i := 1 TO N_ITER DO
      CPU.FlushInstructionCache();
      CPU.FlushDataCache();
      start := CPU.CycleCounter();
      Performance.One(o[0]);
      stop  := CPU.CycleCounter();
      Spy.Hit( spy, 1000 * start, 1000 * stop );
    END;

    IO.Put(TEST10 & WARM & "\n");
    spy := Spy.Create(TEST10 & WARM);
    Performance.One(o[0]);
    start := CPU.CycleCounter();
    FOR i := 1 TO N_ITER DO
      Performance.One(o[0]);
    END;
    stop := CPU.CycleCounter();
    Spy.Hit( spy, start, stop );

    IO.Put(TEST11 & COLD & "\n");
    spy := Spy.Create(TEST11 & COLD);
    Performance.Two(o[0],o[1]);
    FOR i := 1 TO N_ITER DO
      CPU.FlushInstructionCache();
      CPU.FlushDataCache();
      start := CPU.CycleCounter();
      Performance.Two(o[0],o[1]);
      stop  := CPU.CycleCounter();
      Spy.Hit( spy, 1000 * start, 1000 * stop );
    END;

    IO.Put(TEST11 & WARM & "\n");
    spy := Spy.Create(TEST11 & WARM);
    Performance.Two(o[0],o[1]);
    start := CPU.CycleCounter();
    FOR i := 1 TO N_ITER DO
      Performance.Two(o[0],o[1]);
    END;
    stop := CPU.CycleCounter();
    Spy.Hit( spy, start, stop );

    IO.Put(TEST12 & COLD & "\n");
    spy := Spy.Create(TEST12 & COLD);
    Performance.Four(o[0],o[1],o[2],o[3]);
    FOR i := 1 TO N_ITER DO
      CPU.FlushInstructionCache();
      CPU.FlushDataCache();
      start := CPU.CycleCounter();
      Performance.Four(o[0],o[1],o[2],o[3]);
      stop  := CPU.CycleCounter();
      Spy.Hit( spy, 1000 * start, 1000 * stop );
    END;

    IO.Put(TEST12 & WARM & "\n");
    spy := Spy.Create(TEST12 & WARM);
    Performance.Four(o[0],o[1],o[2],o[3]);
    start := CPU.CycleCounter();
    FOR i := 1 TO N_ITER DO
      Performance.Four(o[0],o[1],o[2],o[3]);
    END;
    stop := CPU.CycleCounter();
    Spy.Hit( spy, start, stop );

    IO.Put(TEST13 & COLD & "\n");
    spy := Spy.Create(TEST13 & COLD);
    Performance.Eight(o[0],o[1],o[2],o[3],o[4],o[5],o[6],o[7]);
    FOR i := 1 TO N_ITER DO
      CPU.FlushInstructionCache();
      CPU.FlushDataCache();
      start := CPU.CycleCounter();
      Performance.Eight(o[0],o[1],o[2],o[3],o[4],o[5],o[6],o[7]);
      stop  := CPU.CycleCounter();
      Spy.Hit( spy, 1000 * start, 1000 * stop );
    END;

    IO.Put(TEST13 & WARM & "\n");
    spy := Spy.Create(TEST13 & WARM);
    Performance.Eight(o[0],o[1],o[2],o[3],o[4],o[5],o[6],o[7]);
    start := CPU.CycleCounter();
    FOR i := 1 TO N_ITER DO
      Performance.Eight(o[0],o[1],o[2],o[3],o[4],o[5],o[6],o[7]);
    END;
    stop := CPU.CycleCounter();
    Spy.Hit( spy, start, stop );

    VAR
      args : INTEGER := 1;
      list : AccessMode.ListT;
    BEGIN
      FOR i := FIRST(bindings^) TO LAST(bindings^) DO
        list      := NEW( AccessMode.ListT, args );
        FOR j := FIRST(list^) TO LAST(list^) DO
          list[j] := amRequired1;
        END;
        TRY
          SecurityManagerPrivate.SetAccessControl( bindings[i],
                                                   WORK_C,
                                                   NIL,
                                                   list         );
        EXCEPT
        | SecurityError.T =>
          BEGIN
            IO.Put("*** Couldn't install access control on One/Two/Four");
            IO.Put("/Eight ***\n");
            RETURN;
          END;
        END;
        args := args * 2;
      END;
    END;

    IO.Put(TEST10 & PERM1 & COLD & "\n");
    spy := Spy.Create(TEST10 & PERM1 & COLD);
    Performance.One(o[0]);
    FOR i := 1 TO N_ITER DO
      CPU.FlushInstructionCache();
      CPU.FlushDataCache();
      start := CPU.CycleCounter();
      Performance.One(o[0]);
      stop  := CPU.CycleCounter();
      Spy.Hit( spy, 1000 * start, 1000 * stop );
    END;

    IO.Put(TEST10 & PERM1 & WARM & "\n");
    spy := Spy.Create(TEST10 & PERM1 & WARM);
    Performance.One(o[0]);
    start := CPU.CycleCounter();
    FOR i := 1 TO N_ITER DO
      Performance.One(o[0]);
    END;
    stop := CPU.CycleCounter();
    Spy.Hit( spy, start, stop );

    IO.Put(TEST11 & PERM1 & COLD & "\n");
    spy := Spy.Create(TEST11 & PERM1 & COLD);
    Performance.Two(o[0],o[1]);
    FOR i := 1 TO N_ITER DO
      CPU.FlushInstructionCache();
      CPU.FlushDataCache();
      start := CPU.CycleCounter();
      Performance.Two(o[0],o[1]);
      stop  := CPU.CycleCounter();
      Spy.Hit( spy, 1000 * start, 1000 * stop );
    END;

    IO.Put(TEST11 & PERM1 & WARM & "\n");
    spy := Spy.Create(TEST11 & PERM1 & WARM);
    Performance.Two(o[0],o[1]);
    start := CPU.CycleCounter();
    FOR i := 1 TO N_ITER DO
      Performance.Two(o[0],o[1]);
    END;
    stop := CPU.CycleCounter();
    Spy.Hit( spy, start, stop );

    IO.Put(TEST12 & PERM1 & COLD & "\n");
    spy := Spy.Create(TEST12 & PERM1 & COLD);
    Performance.Four(o[0],o[1],o[2],o[3]);
    FOR i := 1 TO N_ITER DO
      CPU.FlushInstructionCache();
      CPU.FlushDataCache();
      start := CPU.CycleCounter();
      Performance.Four(o[0],o[1],o[2],o[3]);
      stop  := CPU.CycleCounter();
      Spy.Hit( spy, 1000 * start, 1000 * stop );
    END;

    IO.Put(TEST12 & PERM1 & WARM & "\n");
    spy := Spy.Create(TEST12 & PERM1 & WARM);
    Performance.Four(o[0],o[1],o[2],o[3]);
    start := CPU.CycleCounter();
    FOR i := 1 TO N_ITER DO
      Performance.Four(o[0],o[1],o[2],o[3]);
    END;
    stop := CPU.CycleCounter();
    Spy.Hit( spy, start, stop );

    IO.Put(TEST13 & PERM1 & COLD & "\n");
    spy := Spy.Create(TEST13 & PERM1 & COLD);
    Performance.Eight(o[0],o[1],o[2],o[3],o[4],o[5],o[6],o[7]);
    FOR i := 1 TO N_ITER DO
      CPU.FlushInstructionCache();
      CPU.FlushDataCache();
      start := CPU.CycleCounter();
      Performance.Eight(o[0],o[1],o[2],o[3],o[4],o[5],o[6],o[7]);
      stop  := CPU.CycleCounter();
      Spy.Hit( spy, 1000 * start, 1000 * stop );
    END;

    IO.Put(TEST13 & PERM1 & WARM & "\n");
    spy := Spy.Create(TEST13 & PERM1 & WARM);
    Performance.Eight(o[0],o[1],o[2],o[3],o[4],o[5],o[6],o[7]);
    start := CPU.CycleCounter();
    FOR i := 1 TO N_ITER DO
      Performance.Eight(o[0],o[1],o[2],o[3],o[4],o[5],o[6],o[7]);
    END;
    stop := CPU.CycleCounter();
    Spy.Hit( spy, start, stop );

    VAR
      args : INTEGER := 1;
      list : AccessMode.ListT;
    BEGIN
      FOR i := FIRST(bindings^) TO LAST(bindings^) DO
        list      := NEW( AccessMode.ListT, args );
        FOR j := FIRST(list^) TO LAST(list^) DO
          list[j] := amRequired10;
        END;
        TRY
          SecurityManagerPrivate.SetAccessControl( bindings[i],
                                                   WORK_C,
                                                   NIL,
                                                   list         );
        EXCEPT
        | SecurityError.T =>
          BEGIN
            IO.Put("*** Couldn't install access control on One/Two/Four");
            IO.Put("/Eight ***\n");
            RETURN;
          END;
        END;
        args := args * 2;
      END;
    END;

    IO.Put(TEST10 & PERM10 & COLD & "\n");
    spy := Spy.Create(TEST10 & PERM10 & COLD);
    Performance.One(o[0]);
    FOR i := 1 TO N_ITER DO
      CPU.FlushInstructionCache();
      CPU.FlushDataCache();
      start := CPU.CycleCounter();
      Performance.One(o[0]);
      stop  := CPU.CycleCounter();
      Spy.Hit( spy, 1000 * start, 1000 * stop );
    END;

    IO.Put(TEST10 & PERM10 & WARM & "\n");
    spy := Spy.Create(TEST10 & PERM10 & WARM);
    Performance.One(o[0]);
    start := CPU.CycleCounter();
    FOR i := 1 TO N_ITER DO
      Performance.One(o[0]);
    END;
    stop := CPU.CycleCounter();
    Spy.Hit( spy, start, stop );

    IO.Put(TEST11 & PERM10 & COLD & "\n");
    spy := Spy.Create(TEST11 & PERM10 & COLD);
    Performance.Two(o[0],o[1]);
    FOR i := 1 TO N_ITER DO
      CPU.FlushInstructionCache();
      CPU.FlushDataCache();
      start := CPU.CycleCounter();
      Performance.Two(o[0],o[1]);
      stop  := CPU.CycleCounter();
      Spy.Hit( spy, 1000 * start, 1000 * stop );
    END;

    IO.Put(TEST11 & PERM10 & WARM & "\n");
    spy := Spy.Create(TEST11 & PERM10 & WARM);
    Performance.Two(o[0],o[1]);
    start := CPU.CycleCounter();
    FOR i := 1 TO N_ITER DO
      Performance.Two(o[0],o[1]);
    END;
    stop := CPU.CycleCounter();
    Spy.Hit( spy, start, stop );

    IO.Put(TEST12 & PERM10 & COLD & "\n");
    spy := Spy.Create(TEST12 & PERM10 & COLD);
    Performance.Four(o[0],o[1],o[2],o[3]);
    FOR i := 1 TO N_ITER DO
      CPU.FlushInstructionCache();
      CPU.FlushDataCache();
      start := CPU.CycleCounter();
      Performance.Four(o[0],o[1],o[2],o[3]);
      stop  := CPU.CycleCounter();
      Spy.Hit( spy, 1000 * start, 1000 * stop );
    END;

    IO.Put(TEST12 & PERM10 & WARM & "\n");
    spy := Spy.Create(TEST12 & PERM10 & WARM);
    Performance.Four(o[0],o[1],o[2],o[3]);
    start := CPU.CycleCounter();
    FOR i := 1 TO N_ITER DO
      Performance.Four(o[0],o[1],o[2],o[3]);
    END;
    stop := CPU.CycleCounter();
    Spy.Hit( spy, start, stop );

    IO.Put(TEST13 & PERM10 & COLD & "\n");
    spy := Spy.Create(TEST13 & PERM10 & COLD);
    Performance.Eight(o[0],o[1],o[2],o[3],o[4],o[5],o[6],o[7]);
    FOR i := 1 TO N_ITER DO
      CPU.FlushInstructionCache();
      CPU.FlushDataCache();
      start := CPU.CycleCounter();
      Performance.Eight(o[0],o[1],o[2],o[3],o[4],o[5],o[6],o[7]);
      stop  := CPU.CycleCounter();
      Spy.Hit( spy, 1000 * start, 1000 * stop );
    END;

    IO.Put(TEST13 & PERM10 & WARM & "\n");
    spy := Spy.Create(TEST13 & PERM10 & WARM);
    Performance.Eight(o[0],o[1],o[2],o[3],o[4],o[5],o[6],o[7]);
    start := CPU.CycleCounter();
    FOR i := 1 TO N_ITER DO
      Performance.Eight(o[0],o[1],o[2],o[3],o[4],o[5],o[6],o[7]);
    END;
    stop := CPU.CycleCounter();
    Spy.Hit( spy, start, stop );

    VAR
      args : INTEGER := 1;
      list : AccessMode.ListT;
    BEGIN
      FOR i := FIRST(bindings^) TO LAST(bindings^) DO
        list      := NEW( AccessMode.ListT, args );
        FOR j := FIRST(list^) TO LAST(list^) DO
          list[j] := amRequired1;
        END;
        TRY
          SecurityManagerPrivate.SetAccessControl( bindings[i],
                                                   WORK_DC,
                                                   NIL,
                                                   list         );
        EXCEPT
        | SecurityError.T =>
          BEGIN
            IO.Put("*** Couldn't install access control on One/Two/Four");
            IO.Put("/Eight ***\n");
            RETURN;
          END;
        END;
        args := args * 2;
      END;
    END;

    IO.Put(TEST10 & PERM1 & DOMAIN & COLD & "\n");
    spy := Spy.Create(TEST10 & PERM1 & DOMAIN & COLD);
    Performance.One(o[0]);
    FOR i := 1 TO N_ITER DO
      CPU.FlushInstructionCache();
      CPU.FlushDataCache();
      start := CPU.CycleCounter();
      Performance.One(o[0]);
      stop  := CPU.CycleCounter();
      Spy.Hit( spy, 1000 * start, 1000 * stop );
    END;

    IO.Put(TEST10 & PERM1 & DOMAIN & WARM & "\n");
    spy := Spy.Create(TEST10 & PERM1 & DOMAIN & WARM);
    Performance.One(o[0]);
    start := CPU.CycleCounter();
    FOR i := 1 TO N_ITER DO
      Performance.One(o[0]);
    END;
    stop := CPU.CycleCounter();
    Spy.Hit( spy, start, stop );

    IO.Put(TEST11 & PERM1 & DOMAIN & COLD & "\n");
    spy := Spy.Create(TEST11 & PERM1 & DOMAIN & COLD);
    Performance.Two(o[0],o[1]);
    FOR i := 1 TO N_ITER DO
      CPU.FlushInstructionCache();
      CPU.FlushDataCache();
      start := CPU.CycleCounter();
      Performance.Two(o[0],o[1]);
      stop  := CPU.CycleCounter();
      Spy.Hit( spy, 1000 * start, 1000 * stop );
    END;

    IO.Put(TEST11 & PERM1 & DOMAIN & WARM & "\n");
    spy := Spy.Create(TEST11 & PERM1 & DOMAIN & WARM);
    Performance.Two(o[0],o[1]);
    start := CPU.CycleCounter();
    FOR i := 1 TO N_ITER DO
      Performance.Two(o[0],o[1]);
    END;
    stop := CPU.CycleCounter();
    Spy.Hit( spy, start, stop );

    IO.Put(TEST12 & PERM1 & DOMAIN & COLD & "\n");
    spy := Spy.Create(TEST12 & PERM1 & DOMAIN & COLD);
    Performance.Four(o[0],o[1],o[2],o[3]);
    FOR i := 1 TO N_ITER DO
      CPU.FlushInstructionCache();
      CPU.FlushDataCache();
      start := CPU.CycleCounter();
      Performance.Four(o[0],o[1],o[2],o[3]);
      stop  := CPU.CycleCounter();
      Spy.Hit( spy, 1000 * start, 1000 * stop );
    END;

    IO.Put(TEST12 & PERM1 & DOMAIN & WARM & "\n");
    spy := Spy.Create(TEST12 & PERM1 & DOMAIN & WARM);
    Performance.Four(o[0],o[1],o[2],o[3]);
    start := CPU.CycleCounter();
    FOR i := 1 TO N_ITER DO
      Performance.Four(o[0],o[1],o[2],o[3]);
    END;
    stop := CPU.CycleCounter();
    Spy.Hit( spy, start, stop );

    IO.Put(TEST13 & PERM1 & DOMAIN & COLD & "\n");
    spy := Spy.Create(TEST13 & PERM1 & DOMAIN & COLD);
    Performance.Eight(o[0],o[1],o[2],o[3],o[4],o[5],o[6],o[7]);
    FOR i := 1 TO N_ITER DO
      CPU.FlushInstructionCache();
      CPU.FlushDataCache();
      start := CPU.CycleCounter();
      Performance.Eight(o[0],o[1],o[2],o[3],o[4],o[5],o[6],o[7]);
      stop  := CPU.CycleCounter();
      Spy.Hit( spy, 1000 * start, 1000 * stop );
    END;

    IO.Put(TEST13 & PERM1 & DOMAIN & WARM & "\n");
    spy := Spy.Create(TEST13 & PERM1 & DOMAIN & WARM);
    Performance.Eight(o[0],o[1],o[2],o[3],o[4],o[5],o[6],o[7]);
    start := CPU.CycleCounter();
    FOR i := 1 TO N_ITER DO
      Performance.Eight(o[0],o[1],o[2],o[3],o[4],o[5],o[6],o[7]);
    END;
    stop := CPU.CycleCounter();
    Spy.Hit( spy, start, stop );

    VAR
      args : INTEGER := 1;
      list : AccessMode.ListT;
    BEGIN
      FOR i := FIRST(bindings^) TO LAST(bindings^) DO
        list      := NEW( AccessMode.ListT, args );
        FOR j := FIRST(list^) TO LAST(list^) DO
          list[j] := amRequired10;
        END;
        TRY
          SecurityManagerPrivate.SetAccessControl( bindings[i],
                                                   WORK_DC,
                                                   NIL,
                                                   list         );
        EXCEPT
        | SecurityError.T =>
          BEGIN
            IO.Put("*** Couldn't install access control on One/Two/Four");
            IO.Put("/Eight ***\n");
            RETURN;
          END;
        END;
        args := args * 2;
      END;
    END;

    IO.Put(TEST10 & PERM10 & DOMAIN & COLD & "\n");
    spy := Spy.Create(TEST10 & PERM10 & DOMAIN & COLD);
    Performance.One(o[0]);
    FOR i := 1 TO N_ITER DO
      CPU.FlushInstructionCache();
      CPU.FlushDataCache();
      start := CPU.CycleCounter();
      Performance.One(o[0]);
      stop  := CPU.CycleCounter();
      Spy.Hit( spy, 1000 * start, 1000 * stop );
    END;

    IO.Put(TEST10 & PERM10 & DOMAIN & WARM & "\n");
    spy := Spy.Create(TEST10 & PERM10 & DOMAIN & WARM);
    Performance.One(o[0]);
    start := CPU.CycleCounter();
    FOR i := 1 TO N_ITER DO
      Performance.One(o[0]);
    END;
    stop := CPU.CycleCounter();
    Spy.Hit( spy, start, stop );

    IO.Put(TEST11 & PERM10 & DOMAIN & COLD & "\n");
    spy := Spy.Create(TEST11 & PERM10 & DOMAIN & COLD);
    Performance.Two(o[0],o[1]);
    FOR i := 1 TO N_ITER DO
      CPU.FlushInstructionCache();
      CPU.FlushDataCache();
      start := CPU.CycleCounter();
      Performance.Two(o[0],o[1]);
      stop  := CPU.CycleCounter();
      Spy.Hit( spy, 1000 * start, 1000 * stop );
    END;

    IO.Put(TEST11 & PERM10 & DOMAIN & WARM & "\n");
    spy := Spy.Create(TEST11 & PERM10 & DOMAIN & WARM);
    Performance.Two(o[0],o[1]);
    start := CPU.CycleCounter();
    FOR i := 1 TO N_ITER DO
      Performance.Two(o[0],o[1]);
    END;
    stop := CPU.CycleCounter();
    Spy.Hit( spy, start, stop );

    IO.Put(TEST12 & PERM10 & DOMAIN & COLD & "\n");
    spy := Spy.Create(TEST12 & PERM10 & DOMAIN & COLD);
    Performance.Four(o[0],o[1],o[2],o[3]);
    FOR i := 1 TO N_ITER DO
      CPU.FlushInstructionCache();
      CPU.FlushDataCache();
      start := CPU.CycleCounter();
      Performance.Four(o[0],o[1],o[2],o[3]);
      stop  := CPU.CycleCounter();
      Spy.Hit( spy, 1000 * start, 1000 * stop );
    END;

    IO.Put(TEST12 & PERM10 & DOMAIN & WARM & "\n");
    spy := Spy.Create(TEST12 & PERM10 & DOMAIN & WARM);
    Performance.Four(o[0],o[1],o[2],o[3]);
    start := CPU.CycleCounter();
    FOR i := 1 TO N_ITER DO
      Performance.Four(o[0],o[1],o[2],o[3]);
    END;
    stop := CPU.CycleCounter();
    Spy.Hit( spy, start, stop );

    IO.Put(TEST13 & PERM10 & DOMAIN & COLD & "\n");
    spy := Spy.Create(TEST13 & PERM10 & DOMAIN & COLD);
    Performance.Eight(o[0],o[1],o[2],o[3],o[4],o[5],o[6],o[7]);
    FOR i := 1 TO N_ITER DO
      CPU.FlushInstructionCache();
      CPU.FlushDataCache();
      start := CPU.CycleCounter();
      Performance.Eight(o[0],o[1],o[2],o[3],o[4],o[5],o[6],o[7]);
      stop  := CPU.CycleCounter();
      Spy.Hit( spy, 1000 * start, 1000 * stop );
    END;

    IO.Put(TEST13 & PERM10 & DOMAIN & WARM & "\n");
    spy := Spy.Create(TEST13 & PERM10 & DOMAIN & WARM);
    Performance.Eight(o[0],o[1],o[2],o[3],o[4],o[5],o[6],o[7]);
    start := CPU.CycleCounter();
    FOR i := 1 TO N_ITER DO
      Performance.Eight(o[0],o[1],o[2],o[3],o[4],o[5],o[6],o[7]);
    END;
    stop := CPU.CycleCounter();
    Spy.Hit( spy, start, stop );

    (* Clean Up. *)

    SecurityManagerPrivate.SetTypeSecurity(TYPECODE(SecureT), FALSE);

    TRY
      Dispatcher.AtomicSwap( medNew, medDefault );
    EXCEPT
    | Dispatcher.Error =>
      BEGIN
        IO.Put("*** Couldn't swap handlers on SecurityPolicy.Mediate ***\n");
        RETURN;
      END;
    END;

    SecurityManagerPrivate.Reset();
  END Run;


(*
    My Little Helpers
   -------------------
 *)

PROCEDURE Step()  =
  VAR
    i : INTEGER;
  BEGIN
    i := 4;
    Performance.Null();
    i := 5;
  END Step;

PROCEDURE Null()  =
  BEGIN
  END Null;

PROCEDURE Null1() =
  BEGIN
  END Null1;

PROCEDURE Null2() =
  BEGIN
  END Null2;

PROCEDURE Null3() =
  BEGIN
  END Null3;

PROCEDURE Null4() =
  BEGIN
  END Null4;

PROCEDURE Null5() =
  BEGIN
  END Null5;

PROCEDURE Null6() =
  BEGIN
  END Null6;

PROCEDURE One  ( <*NOWARN*> a1                             : SecureT ) =
  BEGIN
  END One;

PROCEDURE Two  ( <*NOWARN*> a1, a2                         : SecureT ) =
  BEGIN
  END Two;

PROCEDURE Four ( <*NOWARN*> a1, a2, a3, a4                 : SecureT ) =
  BEGIN
  END Four;

PROCEDURE Eight( <*NOWARN*> a1, a2, a3, a4, a5, a6, a7, a8 : SecureT ) =
  BEGIN
  END Eight;

BEGIN
END Performance.

(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *
 * 18-Nov-97  Robert Grimm (rgrimm) at the University of Washington
 *      Changed tests to accomodate generalized simple permissions
 *
 * 27-Oct-97  Robert Grimm (rgrimm) at the University of Washington
 *      Added more regression tests
 *      to test "contains" and "Contain" functionality.
 *
 * 06-Aug-97  Robert Grimm (rgrimm) at the University of Washington
 *      Created.
 *
 *)

MODULE AccessModeTest;

IMPORT SpinException, Text, Permission, AccessMode;

TYPE
  PermT = Permission.T OBJECT
    s : TEXT;
  OVERRIDES
    contains := PermContain;
    toText   := PermToText;
  END;

PROCEDURE PermContain( self : PermT; other : Permission.T ) : BOOLEAN =
  BEGIN
    IF NOT ISTYPE( other, PermT ) THEN RETURN FALSE; END;
    WITH o = NARROW( other, PermT ) DO
      RETURN Text.Equal( self.s, o.s );
    END;
  END PermContain;

PROCEDURE PermToText( self : PermT ) : TEXT =
  BEGIN
    RETURN self.s;
  END PermToText;

TYPE
  PT = Permission.T BRANDED "Blah" OBJECT
    s : TEXT;
  OVERRIDES
    contains := PContain;
    toText   := PToText;
  END;

PROCEDURE PContain( self : PT; other : Permission.T ) : BOOLEAN =
  BEGIN
    IF NOT ISTYPE( other, PT ) THEN RETURN FALSE; END;
    WITH o = NARROW( other, PT ) DO
      RETURN Text.Equal( self.s, o.s );
    END;
  END PContain;

PROCEDURE PToText( self : PT ) : TEXT = 
  BEGIN
    RETURN self.s;
  END PToText;

TYPE
  LABEL = { A, B, C };
  MYSET = SET OF LABEL;
  P2T = Permission.T BRANDED "YetAnother" OBJECT
    s : MYSET;
  OVERRIDES
    contains := P2Contain;
    toText   := P2ToText;
  END;

PROCEDURE P2Contain( self : P2T; other : Permission.T ) : BOOLEAN =
  BEGIN
    IF NOT ISTYPE( other, P2T ) THEN RETURN FALSE; END;
    WITH o = NARROW( other, P2T ) DO
      RETURN self.s >= o.s;
    END;
  END P2Contain;

PROCEDURE P2ToText( self : P2T ) : TEXT =
  VAR
    s : TEXT := "";
  BEGIN
    IF LABEL.A IN self.s THEN s := Text.Cat(s, "A"); END;
    IF LABEL.B IN self.s THEN s := Text.Cat(s, "B"); END;
    IF LABEL.C IN self.s THEN s := Text.Cat(s, "C"); END;
    RETURN s;
  END P2ToText;

VAR
  perm1, perm2, perm3 : PermT;
  pA, pB, pAB, pBC, pABC : P2T;

PROCEDURE T1() : BOOLEAN = 
  VAR
    perm : PermT;
  BEGIN
    perm := NEW( PermT, s := "second" );
    IF NOT perm1.contains( perm1 ) THEN RETURN FALSE; END;
    IF     perm1.contains( perm2 ) THEN RETURN FALSE; END;
    IF     perm1.contains( perm3 ) THEN RETURN FALSE; END;
    IF     perm1.contains( perm  ) THEN RETURN FALSE; END;
    IF     perm2.contains( perm1 ) THEN RETURN FALSE; END;
    IF NOT perm2.contains( perm2 ) THEN RETURN FALSE; END;
    IF     perm2.contains( perm3 ) THEN RETURN FALSE; END;
    IF NOT perm2.contains( perm  ) THEN RETURN FALSE; END;
    IF     perm3.contains( perm1 ) THEN RETURN FALSE; END;
    IF     perm3.contains( perm2 ) THEN RETURN FALSE; END;
    IF NOT perm3.contains( perm3 ) THEN RETURN FALSE; END;
    IF     perm3.contains( perm  ) THEN RETURN FALSE; END;
    IF NOT pABC.contains ( pABC  ) THEN RETURN FALSE; END;
    IF NOT pABC.contains ( pAB   ) THEN RETURN FALSE; END;
    IF NOT pABC.contains ( pBC   ) THEN RETURN FALSE; END;
    IF NOT pABC.contains ( pA    ) THEN RETURN FALSE; END;
    IF NOT pABC.contains ( pB    ) THEN RETURN FALSE; END;
    IF NOT pAB.contains  ( pAB   ) THEN RETURN FALSE; END;
    IF NOT pAB.contains  ( pA    ) THEN RETURN FALSE; END;
    IF NOT pAB.contains  ( pB    ) THEN RETURN FALSE; END;
    IF     pAB.contains  ( pABC  ) THEN RETURN FALSE; END;
    IF     pA.contains   ( pABC  ) THEN RETURN FALSE; END;
    IF     pA.contains   ( pAB   ) THEN RETURN FALSE; END;
    RETURN TRUE;
  END T1;

PROCEDURE T2() : BOOLEAN = 
  VAR
    list : Permission.ListT;
    mode : AccessMode.T;
    perm : Permission.T;
    ok   : BOOLEAN;
  BEGIN
    list    := NEW( Permission.ListT, 6 );
    list[2] := perm2;
    list[4] := perm3;
    mode    := AccessMode.Create( AccessMode.SimpleT{}, list );
    IF     AccessMode.HasPermission( mode, perm1 ) THEN RETURN FALSE; END;
    IF NOT AccessMode.HasPermission( mode, perm2 ) THEN RETURN FALSE; END;
    IF NOT AccessMode.HasPermission( mode, perm3 ) THEN RETURN FALSE; END;
    IF AccessMode.GetSimplePermissions( mode ) # AccessMode.SimpleT{} THEN
      RETURN FALSE;
    END;
    IF AccessMode.GetPermissionNumber( mode ) # 2 THEN RETURN FALSE; END;
    perm := AccessMode.GetPermission( mode, 0 );
    IF NOT perm.contains( perm2 ) THEN RETURN FALSE; END;
    perm := AccessMode.GetPermission( mode, 1 );
    IF NOT perm.contains( perm3 ) THEN RETURN FALSE; END;
    ok := FALSE;
    TRY
      perm := AccessMode.GetPermission( mode, 2 );
    EXCEPT
    | SpinException.Exception(info) =>
      IF info.code = SpinException.ExceptionCode.SubscriptOutOfRange THEN
        ok := TRUE;
      END;
    ELSE (* Just catch everything else *)
    END;
    IF NOT ok THEN RETURN FALSE; END;
    perm := NEW( PT, s := "second" );
    IF   AccessMode.HasPermission( mode, perm ) THEN RETURN FALSE; END;

    list    := NEW( Permission.ListT, 10 );
    list[2] := pA;
    list[4] := pA;
    mode    := AccessMode.Create( AccessMode.SimpleT{}, list );
    IF NOT AccessMode.HasPermission( mode, pA   ) THEN RETURN FALSE; END;
    IF     AccessMode.HasPermission( mode, pB   ) THEN RETURN FALSE; END;
    IF     AccessMode.HasPermission( mode, pAB  ) THEN RETURN FALSE; END;
    IF     AccessMode.HasPermission( mode, pBC  ) THEN RETURN FALSE; END;
    IF     AccessMode.HasPermission( mode, pABC ) THEN RETURN FALSE; END;
    IF AccessMode.GetPermissionNumber( mode ) # 1 THEN RETURN FALSE; END;
    perm := AccessMode.GetPermission( mode, 0 );
    IF NOT pA.contains( perm ) THEN RETURN FALSE; END;
    IF NOT perm.contains( pA ) THEN RETURN FALSE; END;

    list[6] := pAB;
    list[8] := pBC;
    mode    := AccessMode.Create( AccessMode.SimpleT{}, list );
    IF NOT AccessMode.HasPermission( mode, pA   ) THEN RETURN FALSE; END;
    IF NOT AccessMode.HasPermission( mode, pB   ) THEN RETURN FALSE; END;
    IF NOT AccessMode.HasPermission( mode, pAB  ) THEN RETURN FALSE; END;
    IF NOT AccessMode.HasPermission( mode, pBC  ) THEN RETURN FALSE; END;
    IF     AccessMode.HasPermission( mode, pABC ) THEN RETURN FALSE; END;
    IF AccessMode.GetPermissionNumber( mode ) # 2 THEN RETURN FALSE; END;
    perm := AccessMode.GetPermission( mode, 0 );
    IF NOT pAB.contains( perm ) THEN RETURN FALSE; END;
    IF NOT perm.contains( pAB ) THEN RETURN FALSE; END;
    perm := AccessMode.GetPermission( mode, 1 );
    IF NOT pBC.contains( perm ) THEN RETURN FALSE; END;
    IF NOT perm.contains( pBC ) THEN RETURN FALSE; END;
    
    list[7] := pABC;
    list[9] := pABC;
    mode    := AccessMode.Create( AccessMode.SimpleT{}, list );
    IF NOT AccessMode.HasPermission( mode, pA   ) THEN RETURN FALSE; END;
    IF NOT AccessMode.HasPermission( mode, pB   ) THEN RETURN FALSE; END;
    IF NOT AccessMode.HasPermission( mode, pAB  ) THEN RETURN FALSE; END;
    IF NOT AccessMode.HasPermission( mode, pBC  ) THEN RETURN FALSE; END;
    IF NOT AccessMode.HasPermission( mode, pABC ) THEN RETURN FALSE; END;
    IF AccessMode.GetPermissionNumber( mode ) # 1 THEN RETURN FALSE; END;
    perm := AccessMode.GetPermission( mode, 0 );
    IF NOT pABC.contains( perm ) THEN RETURN FALSE; END;
    IF NOT perm.contains( pABC ) THEN RETURN FALSE; END;

    RETURN TRUE;
  END T2;

PROCEDURE T3() : BOOLEAN =
  VAR
    list : Permission.ListT;
    mode1, mode2 : AccessMode.T;
  BEGIN
    list    := NEW( Permission.ListT, 5);
    mode1   := AccessMode.Create( AccessMode.SimpleT{}, list );
    mode2   := AccessMode.Create( AccessMode.SimpleT{}, NIL  );
    IF NOT AccessMode.Contain( mode1, mode2 ) THEN RETURN FALSE; END;
    IF NOT AccessMode.Contain( mode2, mode1 ) THEN RETURN FALSE; END;
    
    list    := NEW( Permission.ListT, 5);
    list[3] := perm2;
    mode2   := AccessMode.Create( AccessMode.SimpleT{}, list );
    IF     AccessMode.Contain( mode1, mode2 ) THEN RETURN FALSE; END;
    IF NOT AccessMode.Contain( mode2, mode1 ) THEN RETURN FALSE; END;
    
    list    := NEW( Permission.ListT, 10);
    list[6] := perm2;
    mode1   := AccessMode.Create( AccessMode.SimpleT{}, list );
    IF NOT AccessMode.Contain( mode1, mode2 ) THEN RETURN FALSE; END;
    IF NOT AccessMode.Contain( mode2, mode1 ) THEN RETURN FALSE; END;

    list    := NEW( Permission.ListT, 2);
    list[0] := pA;
    list[1] := pB;
    mode1   := AccessMode.Create( AccessMode.SimpleT{}, list );
    list[0] := pAB;
    list[1] := NIL;
    mode2   := AccessMode.Create( AccessMode.SimpleT{}, list );
    IF NOT AccessMode.Contain( mode2, mode1 ) THEN RETURN FALSE; END;
    IF     AccessMode.Contain( mode1, mode2 ) THEN RETURN FALSE; END;

    list[0] := pABC;
    mode1   := AccessMode.Create( AccessMode.SimpleT{}, list );
    IF NOT AccessMode.Contain( mode1, mode2 ) THEN RETURN FALSE; END;
    IF     AccessMode.Contain( mode2, mode1 ) THEN RETURN FALSE; END;

    RETURN TRUE;
  END T3;

PROCEDURE T4() : BOOLEAN =
  VAR
    list : Permission.ListT;
    mode1, mode2, mode3 : AccessMode.T;
    ok : BOOLEAN;
    perm : Permission.T;
  BEGIN
    list     := NEW( Permission.ListT, 100 );
    list[10] := perm2;
    list[11] := perm2;
    list[23] := perm2;
    list[56] := perm2;
    list[88] := perm2;
    mode1 := AccessMode.Create( AccessMode.SimpleT{ AccessMode.EXECUTE },
                                list );
    list     := NEW( Permission.ListT, 50 );
    list[34] := perm2;
    list[37] := perm1;
    list[46] := perm1;
    mode2 := AccessMode.Create( AccessMode.SimpleT{ AccessMode.EXTEND },
                                list );

    mode3 := AccessMode.Combine( mode1,
                                 AccessMode.Create( AccessMode.SimpleT{} ) );
    IF   AccessMode.GetSimplePermissions( mode3 )
       # AccessMode.SimpleT{ AccessMode.EXECUTE } THEN
      RETURN FALSE;
    END;
    IF AccessMode.GetPermissionNumber( mode3 ) # 1 THEN RETURN FALSE; END;
    IF NOT AccessMode.GetPermission( mode3, 0 ).contains(perm2) THEN
      RETURN FALSE;
    END;
    ok := FALSE;
    TRY
      EVAL AccessMode.GetPermission( mode3, 1 );
    EXCEPT
    | SpinException.Exception(info) =>
      IF info.code = SpinException.ExceptionCode.SubscriptOutOfRange THEN
        ok := TRUE;
      END;
    ELSE (* Just catch everything else *)
    END;
    IF NOT ok THEN RETURN FALSE; END;

    mode3 := AccessMode.Combine( AccessMode.Create( AccessMode.SimpleT{} ),
                                 mode1 );
    IF   AccessMode.GetSimplePermissions( mode3 )
       # AccessMode.SimpleT{ AccessMode.EXECUTE } THEN
      RETURN FALSE;
    END;
    IF AccessMode.GetPermissionNumber( mode3 ) # 1 THEN RETURN FALSE; END;
    IF NOT AccessMode.GetPermission( mode3, 0 ).contains(perm2) THEN
      RETURN FALSE;
    END;
    ok := FALSE;
    TRY
      EVAL AccessMode.GetPermission( mode3, 1 );
    EXCEPT
    | SpinException.Exception(info) =>
      IF info.code = SpinException.ExceptionCode.SubscriptOutOfRange THEN
        ok := TRUE;
      END;
    ELSE (* Just catch everything else *)
    END;
    IF NOT ok THEN RETURN FALSE; END;

    mode3 := AccessMode.Combine( mode1, mode2 );
    IF   AccessMode.GetSimplePermissions( mode3 )
       # AccessMode.SimpleT{ AccessMode.EXECUTE, AccessMode.EXTEND } THEN
      RETURN FALSE;
    END;
    IF AccessMode.GetPermissionNumber( mode3 ) # 2 THEN RETURN FALSE; END;
    IF NOT AccessMode.GetPermission( mode3, 0 ).contains(perm2) THEN
      RETURN FALSE;
    END;
    IF NOT AccessMode.GetPermission( mode3, 1 ).contains(perm1) THEN
      RETURN FALSE;
    END;
    ok := FALSE;
    TRY
      EVAL AccessMode.GetPermission( mode3, 2 );
    EXCEPT
    | SpinException.Exception(info) =>
      IF info.code = SpinException.ExceptionCode.SubscriptOutOfRange THEN
        ok := TRUE;
      END;
    ELSE (* Just catch everything else *)
    END;
    IF NOT ok THEN RETURN FALSE; END;

    mode3 := AccessMode.Combine( mode2, mode1 );
    IF   AccessMode.GetSimplePermissions( mode3 )
       # AccessMode.SimpleT{ AccessMode.EXECUTE, AccessMode.EXTEND } THEN
      RETURN FALSE;
    END;
    IF AccessMode.GetPermissionNumber( mode3 ) # 2 THEN RETURN FALSE; END;
    IF NOT AccessMode.GetPermission( mode3, 0 ).contains(perm1) THEN
      RETURN FALSE;
    END;
    IF NOT AccessMode.GetPermission( mode3, 1 ).contains(perm2) THEN
      RETURN FALSE;
    END;
    ok := FALSE;
    TRY
      EVAL AccessMode.GetPermission( mode3, 2 );
    EXCEPT
    | SpinException.Exception(info) =>
      IF info.code = SpinException.ExceptionCode.SubscriptOutOfRange THEN
        ok := TRUE;
      END;
    ELSE (* Just catch everything else *)
    END;

    list := NEW( Permission.ListT, 2 );
    list[0] := pBC;
    list[1] := pAB;
    mode1   := AccessMode.Create( AccessMode.SimpleT{}, list );
    list[0] := pA;
    list[1] := pBC;
    mode2   := AccessMode.Create( AccessMode.SimpleT{}, list );
    mode3   := AccessMode.Combine( mode1, mode2 );
    IF AccessMode.GetPermissionNumber( mode3 ) # 2 THEN RETURN FALSE; END;
    perm := AccessMode.GetPermission ( mode3, 0 );
    IF NOT perm.contains( pAB ) THEN RETURN FALSE; END;
    IF NOT pAB.contains( perm ) THEN RETURN FALSE; END;
    perm := AccessMode.GetPermission ( mode3, 1 );
    IF NOT perm.contains( pBC ) THEN RETURN FALSE; END;
    IF NOT pBC.contains( perm ) THEN RETURN FALSE; END;
    mode3   := AccessMode.Combine( mode2, mode1 );
    IF AccessMode.GetPermissionNumber( mode3 ) # 2 THEN RETURN FALSE; END;
    perm := AccessMode.GetPermission ( mode3, 0 );
    IF NOT perm.contains( pBC ) THEN RETURN FALSE; END;
    IF NOT pBC.contains( perm ) THEN RETURN FALSE; END;
    perm := AccessMode.GetPermission ( mode3, 1 );
    IF NOT perm.contains( pAB ) THEN RETURN FALSE; END;
    IF NOT pAB.contains( perm ) THEN RETURN FALSE; END;
    
    RETURN ok;
  END T4;

PROCEDURE T5() : BOOLEAN = 
  VAR
    simple : AccessMode.SimpleT;
  BEGIN
    simple := AccessMode.SimpleT{};
    FOR i := FIRST(AccessMode.T1) TO LAST(AccessMode.T1) DO
      simple := simple + AccessMode.SimpleT{ i };
    END;
    RETURN simple = AccessMode.FullSimpleAccess;
  END T5;

PROCEDURE Start(<*UNUSED*>i: INTEGER): BOOLEAN =
  BEGIN
    perm1 := NEW( PermT, s := "first");
    perm2 := NEW( PermT, s := "second");
    perm3 := NEW( PermT, s := "third");
    pA    := NEW( P2T, s := MYSET{ LABEL.A                   } );
    pB    := NEW( P2T, s := MYSET{          LABEL.B          } );
    pAB   := NEW( P2T, s := MYSET{ LABEL.A, LABEL.B          } );
    pBC   := NEW( P2T, s := MYSET{          LABEL.B, LABEL.C } );
    pABC  := NEW( P2T, s := MYSET{ LABEL.A, LABEL.B, LABEL.C } );
    RETURN TRUE;
  END Start;

PROCEDURE End(): BOOLEAN =
  BEGIN
    perm1 := NIL;
    perm2 := NIL;
    perm3 := NIL;
    RETURN TRUE;
  END End;

BEGIN
END AccessModeTest.

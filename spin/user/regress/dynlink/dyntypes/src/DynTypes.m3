(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *
 * 27-Dec-95  Przemek Pardyak (pardy) at the University of Washington
 *	Created. Regression test.
 *
 *)

MODULE DynTypes;
IMPORT IO, Fmt, Dyn;
(* IMPORT Cache (*, VirtAddr*);*)

REVEAL
  OT1 = T BRANDED OBJECT
    x: INTEGER;
  OVERRIDES
    m1 := M1;
    m2 := M2;
  END;

(* this type has to be an exact copy of Cache.T type *)
(* or any other non-branded reference type used in comparison below *)
(*
TYPE 
  myCacheT = REF RECORD
    dataSize: INTEGER;
    instSize: INTEGER;
  END;    
*)

(* this type has to be an exact copy of VirtAddr.T type *)
(* or any other branded reference type  used in comparison below *)
(*
TYPE
  myVirtAddrT = BRANDED "VirtAddrT" REF RECORD
    beginAddress   : VirtAddr.Address;
    endAddress     : VirtAddr.Address;
  END;  
*)

PROCEDURE NarrowTest(): BOOLEAN =
  BEGIN
    RETURN Dyn.NarrowTest();
  END NarrowTest;

PROCEDURE TypecaseTest(): BOOLEAN =
  BEGIN
    RETURN Dyn.TypecaseTest();
  END TypecaseTest;

PROCEDURE M1(self: OT1) =
  BEGIN
    IO.Put ("DynTypes.M1: " & Fmt.Int(self.f1) & "\n");
  END M1;

PROCEDURE M2(self: OT1; i: INTEGER): INTEGER =
  VAR 
    k := self.f1;
  BEGIN
    IO.Put ("DynTypes.M2: " & Fmt.Int(i) & " " & Fmt.Int(self.f1) & "\n");
    self.f1 := i;
    RETURN k;
  END M2;

PROCEDURE TypecodeTest (): BOOLEAN =
  VAR
    result: BOOLEAN := TRUE;
  BEGIN
(*
    IO.Put (Fmt.Int(TYPECODE(Cache.T)) & " " & 
                Fmt.Int(TYPECODE(myCacheT)) & "\n");
    IF TYPECODE(Cache.T) # TYPECODE(myCacheT) THEN
      IO.Put ("TYPECODE(Cache.T) # TYPECODE(myCacheT) => ERROR\n");
      result := FALSE;
    END;
*)
    (*  THIS TEST DISABLED BECAUSE RUNTIME EPOCHS NOT IMPLEMENTED
        IO.Put(Fmt.Int(TYPECODE(VirtAddr.T)) & " " & 
        Fmt.Int(TYPECODE(myVirtAddrT)) & "\n");
        IF TYPECODE(VirtAddr.T) = TYPECODE(myVirtAddrT) THEN
        IO.Put ("TYPECODE(VirtAddr.T) = TYPECODE(myVirtAddrT) => ERROR\n");
        result := FALSE;
        END;
    *)  

    IO.Put (Fmt.Int(TYPECODE(OT1)) & " " & 
                Fmt.Int(TYPECODE(T)) & "\n");
    IF TYPECODE(OT1) = TYPECODE(T) THEN
      IO.Put ("TYPECODE(OT1) = TYPECODE(T) => ERROR\n");
      result := FALSE;
    END;
    
    IO.Put (Fmt.Int(TYPECODE(Dyn.OT1)) & " " & 
                Fmt.Int(TYPECODE(Dyn.T)) & "\n");
    IF TYPECODE(Dyn.OT1) = TYPECODE(Dyn.T) THEN
      IO.Put("TYPECODE(Dyn.OT1) = TYPECODE(Dyn.T) => ERROR\n");
      result := FALSE;
    END;
    
    IO.Put (Fmt.Int(TYPECODE(NEW(OT1))) & " " &
      Fmt.Int(TYPECODE(NEW(T))) & "\n" &
      Fmt.Int(TYPECODE(NEW(Dyn.OT1))) & " " &
      Fmt.Int(TYPECODE(NEW(Dyn.T))) & "\n");
    
    IF TYPECODE(NEW(OT1)) # TYPECODE(OT1) THEN
      IO.Put("TYPECODE(NEW(OT1)) # TYPECODE(OT1) => ERROR\n");
      result := FALSE;
    END;
    
    IF TYPECODE(NEW(T)) # TYPECODE(T) THEN
      IO.Put("TYPECODE(NEW(T)) # TYPECODE(T) => ERROR\n");
      result := FALSE;
    END;
    
    IF TYPECODE(NEW(Dyn.OT1)) # TYPECODE(Dyn.OT1) THEN
      IO.Put("TYPECODE(NEW(Dyn.OT1)) # TYPECODE(Dyn.OT1) => ERROR\n");
      result := FALSE;
    END;
    
    IF TYPECODE(NEW(Dyn.T)) # TYPECODE(Dyn.T) THEN
      IO.Put("TYPECODE(NEW(Dyn.T)) # TYPECODE(Dyn.T) => ERROR\n");
      result := FALSE;
    END;
    RETURN result;
  END TypecodeTest;

PROCEDURE ExecTest (): BOOLEAN =
  VAR
    x: REFANY;
    o1: OT1;
    o2: Dyn.OT1;
  BEGIN
    x := NEW(OT1);  
    o1 := x;
    o1.m1();
    EVAL o1.m2(11);
    x := NEW(Dyn.OT1);
    o1 := x;
    o1.m1();
    EVAL o1.m2(13);
    o2 := x;
    o2.m1();
    EVAL o2.m2(17);

    RETURN TRUE;
  END ExecTest; 

PROCEDURE Start(<* UNUSED *>i: INTEGER): BOOLEAN =
  BEGIN
    RETURN TRUE;
  END Start;

PROCEDURE End(): BOOLEAN =
  BEGIN
    RETURN TRUE;
  END End;

BEGIN
END DynTypes.






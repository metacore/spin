(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 14-Oct-96  Przemek Pardyak (pardy) at the University of Washington
 *	Removed unnecessary print commands.
 *
 * 19-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	make guards FUNCTIONAL
 *
 * 14-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Merging changes from Przemek to support the new convention for passing
 *	callee-saved registers.  A reference to a structure containing the 
 *	values in the registers is passed instead of each of the registers
 *	individually.  This permits an architecture-independent interface.
 *
 * 20-May-96  Przemek Pardyak (pardy) at the University of Washington
 *	Changed the dispatcher interface.
 *
 * 20-Apr-96  Wilson Hsieh (whsieh) at the University of Washington
 *	PROCANY change
 *
 * 15-Jan-95  Przemek Pardyak (pardy) at the University of Washington
 *	Created. Regression test.
 *
 *)

UNSAFE MODULE DispSave;
IMPORT IO, Sirpa, Dispatcher, Fmt, DispatcherPrivate, CPU;

<* FATAL Dispatcher.Error *>

VAR
  test: INTEGER;
  
CONST
  MaxDispFrameSize = 5 * 1024;
  MaxDoSaveSize    = 5 * 1024;

PROCEDURE CheckRegs(regs: UNTRACED REF CPU.CalleeSavedRegs)
                    RAISES { Sirpa.Exception } =
  VAR x: INTEGER := LOOPHOLE(ADR(x), INTEGER);
  VAR y: INTEGER := LOOPHOLE(DoSave, INTEGER);
  BEGIN
    IO.Putx(regs.sp); IO.Put(" - ");
    IO.Putx(regs.ra); IO.Put("\n");
    IF regs.sp < x OR regs.sp > x + MaxDispFrameSize THEN
      IO.Put ("SP\n"); RAISE Sirpa.Exception; 
    END;
    IF regs.ra < y OR regs.ra > y + MaxDoSaveSize THEN
      IO.Put ("RA\n"); RAISE Sirpa.Exception; 
    END;
  END CheckRegs;

PROCEDURE Sirpa0 (regs: UNTRACED REF CPU.CalleeSavedRegs)
                  RAISES { Sirpa.Exception } =
  BEGIN
    IO.Put ("----> Save Sirpa0 <----\n" );
    CheckRegs(regs);
  END Sirpa0;

PROCEDURE Sirpa1 (a1 : INTEGER; 
                  regs: UNTRACED REF CPU.CalleeSavedRegs)
                  RAISES { Sirpa.Exception } =
  BEGIN
    IO.Put ("----> Save Sirpa1 : " );
    IO.Put (Fmt.Int (a1) & "\n");
    IF a1 # 111 THEN RAISE Sirpa.Exception; END;
    CheckRegs(regs);
  END Sirpa1;

PROCEDURE Sirpa2 (a1 : INTEGER; a2 : INTEGER;
                  regs: UNTRACED REF CPU.CalleeSavedRegs)
                  RAISES { Sirpa.Exception } =
  BEGIN
    IO.Put ("----> Save Sirpa2 : " );
    IO.Put (Fmt.Int (a1) & " ");
    IO.Put (Fmt.Int (a2) & "\n");
    IF a1 # 111 THEN RAISE Sirpa.Exception; END;
    IF a2 # 222 THEN RAISE Sirpa.Exception; END;
    CheckRegs(regs);
  END Sirpa2;

PROCEDURE Sirpa5 (a1 : INTEGER; a2 : INTEGER; a3 : INTEGER; a4 : INTEGER; 
                  a5 : INTEGER;
                  regs: UNTRACED REF CPU.CalleeSavedRegs)
                  RAISES { Sirpa.Exception } =
  BEGIN
    IO.Put ("----> Save Sirpa5 : " );
    IO.Put (Fmt.Int (a1) & " ");
    IO.Put (Fmt.Int (a2) & " ");
    IO.Put (Fmt.Int (a3) & " ");
    IO.Put (Fmt.Int (a4) & " ");
    IO.Put (Fmt.Int (a5) & "\n");
    IF a1 # 111 THEN RAISE Sirpa.Exception; END;
    IF a2 # 222 THEN RAISE Sirpa.Exception; END;
    IF a3 # 333 THEN RAISE Sirpa.Exception; END;
    IF a4 # 444 THEN RAISE Sirpa.Exception; END;
    IF a5 # 555 THEN RAISE Sirpa.Exception; END;
    CheckRegs(regs);
  END Sirpa5;

<* UNUSED *>
PROCEDURE Sirpa10 (a1 : INTEGER; a2 : INTEGER; a3 : INTEGER; a4 : INTEGER; 
                   a5 : INTEGER; a6 : INTEGER; a7 : INTEGER; a8 : INTEGER; 
                   a9 : INTEGER; a10 : INTEGER;
                   regs: UNTRACED REF CPU.CalleeSavedRegs)
                   RAISES { Sirpa.Exception } =
  BEGIN
    IO.Put ("----> Save Sirpa10 : " );
    IO.Put (Fmt.Int (a1) & " ");
    IO.Put (Fmt.Int (a2) & " ");
    IO.Put (Fmt.Int (a3) & " ");
    IO.Put (Fmt.Int (a4) & " ");
    IO.Put (Fmt.Int (a5) & " ");
    IO.Put (Fmt.Int (a6) & " ");
    IO.Put (Fmt.Int (a7) & " ");
    IO.Put (Fmt.Int (a8) & " ");
    IO.Put (Fmt.Int (a9) & " ");
    IO.Put (Fmt.Int (a10) & "\n");
    IF a1 # 111 THEN RAISE Sirpa.Exception; END;
    IF a2 # 222 THEN RAISE Sirpa.Exception; END;
    IF a3 # 333 THEN RAISE Sirpa.Exception; END;
    IF a4 # 444 THEN RAISE Sirpa.Exception; END;
    IF a5 # 555 THEN RAISE Sirpa.Exception; END;
    IF a6 # 666 THEN RAISE Sirpa.Exception; END;
    IF a7 # 777 THEN RAISE Sirpa.Exception; END;
    IF a8 # 888 THEN RAISE Sirpa.Exception; END;
    IF a9 # 999 THEN RAISE Sirpa.Exception; END;
    IF a10 # 101010 THEN RAISE Sirpa.Exception; END;
    CheckRegs(regs);
  END Sirpa10;

FUNCTIONAL PROCEDURE Guard0 (): BOOLEAN
                  RAISES { Sirpa.Exception } =
  BEGIN
(*
    IO.Put ("----> Save Guard0 <----\n" );
*)
    IF FALSE THEN RAISE Sirpa.Exception; END;
    RETURN TRUE;
  END Guard0;

FUNCTIONAL PROCEDURE Guard1 (a1 : INTEGER): BOOLEAN
                  RAISES { Sirpa.Exception } =
  BEGIN
(*
    IO.Put ("----> Save Guard1 : " );
    IO.Put (Fmt.Int (a1) & "\n");
*)
    IF a1 # 111 THEN RAISE Sirpa.Exception; END;
    RETURN TRUE;
  END Guard1;

FUNCTIONAL PROCEDURE Guard2 (a1 : INTEGER; a2 : INTEGER): BOOLEAN
                  RAISES { Sirpa.Exception } =
  BEGIN
(*
    IO.Put ("----> Save Guard2 : " );
    IO.Put (Fmt.Int (a1) & " ");
    IO.Put (Fmt.Int (a2) & "\n");
*)
    IF a1 # 111 THEN RAISE Sirpa.Exception; END;
    IF a2 # 222 THEN RAISE Sirpa.Exception; END;
    RETURN TRUE;
  END Guard2;

FUNCTIONAL PROCEDURE Guard5 (a1 : INTEGER; a2 : INTEGER; a3 : INTEGER; a4 : INTEGER; 
                  a5 : INTEGER): BOOLEAN
                  RAISES { Sirpa.Exception } =
  BEGIN
(*
    IO.Put ("----> Save Guard5 : " );
    IO.Put (Fmt.Int (a1) & " ");
    IO.Put (Fmt.Int (a2) & " ");
    IO.Put (Fmt.Int (a3) & " ");
    IO.Put (Fmt.Int (a4) & " ");
    IO.Put (Fmt.Int (a5) & "\n");
*)
    IF a1 # 111 THEN RAISE Sirpa.Exception; END;
    IF a2 # 222 THEN RAISE Sirpa.Exception; END;
    IF a3 # 333 THEN RAISE Sirpa.Exception; END;
    IF a4 # 444 THEN RAISE Sirpa.Exception; END;
    IF a5 # 555 THEN RAISE Sirpa.Exception; END;
    RETURN TRUE;
  END Guard5;

<* UNUSED *>
FUNCTIONAL PROCEDURE Guard10 (a1 : INTEGER; a2 : INTEGER; a3 : INTEGER; a4 : INTEGER; 
                   a5 : INTEGER; a6 : INTEGER; a7 : INTEGER; a8 : INTEGER; 
                   a9 : INTEGER; a10 : INTEGER): BOOLEAN
                   RAISES { Sirpa.Exception } =
  BEGIN
(*
    IO.Put ("----> Save Guard10 : " );
    IO.Put (Fmt.Int (a1) & " ");
    IO.Put (Fmt.Int (a2) & " ");
    IO.Put (Fmt.Int (a3) & " ");
    IO.Put (Fmt.Int (a4) & " ");
    IO.Put (Fmt.Int (a5) & " ");
    IO.Put (Fmt.Int (a6) & " ");
    IO.Put (Fmt.Int (a7) & " ");
    IO.Put (Fmt.Int (a8) & " ");
    IO.Put (Fmt.Int (a9) & " ");
    IO.Put (Fmt.Int (a10) & "\n");
*)
    IF a1 # 111 THEN RAISE Sirpa.Exception; END;
    IF a2 # 222 THEN RAISE Sirpa.Exception; END;
    IF a3 # 333 THEN RAISE Sirpa.Exception; END;
    IF a4 # 444 THEN RAISE Sirpa.Exception; END;
    IF a5 # 555 THEN RAISE Sirpa.Exception; END;
    IF a6 # 666 THEN RAISE Sirpa.Exception; END;
    IF a7 # 777 THEN RAISE Sirpa.Exception; END;
    IF a8 # 888 THEN RAISE Sirpa.Exception; END;
    IF a9 # 999 THEN RAISE Sirpa.Exception; END;
    IF a10 # 101010 THEN RAISE Sirpa.Exception; END;
    RETURN TRUE;
  END Guard10;

PROCEDURE DoSave (): BOOLEAN =
  VAR
    binding1, binding2 : Dispatcher.Binding;
    ok: BOOLEAN;
    result: BOOLEAN := TRUE;
  BEGIN
    ok := TRUE;
    TRY
      Sirpa.Sirpa0();
      binding1 := Dispatcher.InstallHandler (Sirpa.Sirpa0, NIL, Sirpa.Sirpa0);
      Sirpa.Sirpa0();
      binding2 := Dispatcher.InstallHandler (Sirpa.Sirpa0, Guard0, Sirpa0);
      Sirpa.Sirpa0();
      Dispatcher.Uninstall (binding1);
      Dispatcher.Uninstall (binding2);
      Sirpa.Sirpa0();
    EXCEPT
    | Sirpa.Exception => ok := FALSE;
    END;
    IF ok THEN
      IO.Put("=> OK\n");
    ELSE
      IO.Put("=> ERROR\n");
      result := FALSE;
    END;

    ok := TRUE;
    TRY
      Sirpa.Sirpa1(111);
      binding1 := Dispatcher.InstallHandler (Sirpa.Sirpa1, NIL, Sirpa.Sirpa1);
      Sirpa.Sirpa1(111);
      binding2 := Dispatcher.InstallHandler (Sirpa.Sirpa1, Guard1, Sirpa1);
      Sirpa.Sirpa1(111);
      Dispatcher.Uninstall (binding1);
      Dispatcher.Uninstall (binding2);
      Sirpa.Sirpa1(111);
    EXCEPT
    | Sirpa.Exception => ok := FALSE;
    END;
    IF ok THEN
      IO.Put("=> OK\n");
    ELSE
      IO.Put("=> ERROR\n");
      result := FALSE;
    END;

    ok := TRUE;
    TRY
      Sirpa.Sirpa2(111,222);
      binding1 := Dispatcher.InstallHandler (Sirpa.Sirpa2, NIL, Sirpa.Sirpa2);
      Sirpa.Sirpa2(111,222);
      binding2 := Dispatcher.InstallHandler (Sirpa.Sirpa2, Guard2, Sirpa2);
      Sirpa.Sirpa2(111,222);
      Dispatcher.Uninstall (binding1);
      Dispatcher.Uninstall (binding2);
      Sirpa.Sirpa2(111,222);
    EXCEPT
    | Sirpa.Exception => ok := FALSE;
    END;
    IF ok THEN
      IO.Put("=> OK\n");
    ELSE
      IO.Put("=> ERROR\n");
      result := FALSE;
    END;

    ok := TRUE;
    TRY
      Sirpa.Sirpa5(111,222,333,444,555);
      binding1 := Dispatcher.InstallHandler (Sirpa.Sirpa5, NIL, Sirpa.Sirpa5);
      Sirpa.Sirpa5(111,222,333,444,555);
      binding2 := Dispatcher.InstallHandler (Sirpa.Sirpa5, Guard5, Sirpa5);
      Sirpa.Sirpa5(111,222,333,444,555);
      Dispatcher.Uninstall (binding1);
      Dispatcher.Uninstall (binding2);
      Sirpa.Sirpa5(111,222,333,444,555);
    EXCEPT
    | Sirpa.Exception => ok := FALSE;
    END;
    IF ok THEN
      IO.Put("=> OK\n");
    ELSE
      IO.Put("=> ERROR\n");
      result := FALSE;
    END;

    RETURN result;
  END DoSave;

PROCEDURE Test1 (): BOOLEAN =
  BEGIN
    test := 1;
    RETURN DoSave();
  END Test1;

PROCEDURE Test2 (): BOOLEAN =
  BEGIN
    test := 2;
    RETURN DoSave();
  END Test2;

PROCEDURE Test3 (): BOOLEAN =
  VAR ok1: BOOLEAN := FALSE;
  VAR ok2: BOOLEAN := FALSE;
  BEGIN  
    (* too many arguments *)
    TRY
      DispatcherPrivate.SetSave(Sirpa.Sirpa15);
    EXCEPT
    | Dispatcher.Error(code) =>
      IF code = Dispatcher.ErrorCode.SaveRegsError THEN
        ok1 := TRUE;
      END;
    END;

    IF NOT ok1 THEN
      IO.Put("ERROR >> Test3 (1)\n");
    END;

    (* not the first operation *)
    TRY
      DispatcherPrivate.SetSave(Sirpa.Sirpa0);
    EXCEPT
    | Dispatcher.Error(code) =>
      IF code = Dispatcher.ErrorCode.SaveRegsError THEN
        ok2 := TRUE;
      END;
    END;

    IF NOT ok2 THEN
      IO.Put("ERROR >> Test3 (2)\n");
    END;

    RETURN ok1 AND ok2;
  END Test3;

PROCEDURE Start(i: INTEGER): BOOLEAN =
  BEGIN
    DispatcherPrivate.SetOptLevel(Sirpa.Sirpa0, i-1);
    DispatcherPrivate.SetOptLevel(Sirpa.Sirpa1, i-1);
    DispatcherPrivate.SetOptLevel(Sirpa.Sirpa2, i-1);
    DispatcherPrivate.SetOptLevel(Sirpa.Sirpa5, i-1);
    DispatcherPrivate.SetOptLevel(Sirpa.Sirpa15, i-1);
    RETURN TRUE;
  END Start;

PROCEDURE End(): BOOLEAN =
  BEGIN
    RETURN TRUE;
  END End;

BEGIN
  DispatcherPrivate.SetSave(Sirpa.Sirpa0);
  DispatcherPrivate.SetSave(Sirpa.Sirpa1);
  DispatcherPrivate.SetSave(Sirpa.Sirpa2);
  DispatcherPrivate.SetSave(Sirpa.Sirpa5);
END DispSave. 

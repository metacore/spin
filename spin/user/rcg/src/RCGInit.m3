(*
 *
 * Copyright 1996, 1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *)

(*
 * HISTORY
 * 17-Oct-96  Wilson Hsieh (whsieh) at the University of Washington
 *	moved code from RCG.m3
 *      created test code
 *
 *)

UNSAFE MODULE RCGInit EXPORTS RCG;

IMPORT Stdio, Wr, Thread, Fmt;
IMPORT BootIO, Stitcher, Dispatcher, ParseParams;
IMPORT RCGTest, RCGCmd, RCGNet;
IMPORT Guard;

(* for integrating with dispatcher
IMPORT DispatcherPrivate, SAL, MachineCPUPrivate, MachineCPU;
*)

<* FATAL Thread.Alerted, Wr.Failure *>

(*
PROCEDURE OptimizeHandler(proc: PROCANY; 
                          <* UNUSED *>optLevel: INTEGER): PROCANY =
  BEGIN
    RETURN Clone(proc, "clone");
  END OptimizeHandler;


FUNCTIONAL
PROCEDURE OptimizeGuard(<* UNUSED *> proc: PROCANY; 
                        optLevel: INTEGER): BOOLEAN =
  BEGIN
    RETURN optLevel = 6;
  END OptimizeGuard;
*)

PROCEDURE CloneTest (x, y: INTEGER; z: BOOLEAN) : INTEGER =
  BEGIN
    IF z THEN
      RETURN x*y;
    ELSE
      RETURN x + y;
    END;
  END CloneTest;

VAR
  zapped: BOOLEAN := FALSE;

PROCEDURE Run (pp: ParseParams.T): BOOLEAN =
  VAR
    p : PROCANY;
    x : INTEGER;
  BEGIN
    IF zapped THEN
      RETURN FALSE;
    END;

    TRY
      pp.reset();
      pp.skipNext();

      IF pp.testNext("zap") THEN
        Uninstall();
        zapped := TRUE;
      ELSIF pp.testNext ("clone") THEN
        p := Clone (CloneTest, "clone test");

        x := LOOPHOLE (p, PROCEDURE (x, y: INTEGER; z: BOOLEAN) : INTEGER) (4,5, TRUE);
        Wr.PutText(Stdio.stdout, "x is " & Fmt.Int (x) & ", should be 20\n");
        x := LOOPHOLE (p, PROCEDURE (x, y: INTEGER; z: BOOLEAN) : INTEGER) (4,5, FALSE);
        
        Wr.PutText(Stdio.stdout, "x is " & Fmt.Int (x) & ", should be 9\n");

      ELSIF pp.testNext("all") THEN
        Wr.PutText (Stdio.stdout, "Linear test\n");
        RCGTest.LinearTest ();
        Wr.PutText (Stdio.stdout, "JT1 test\n");
        RCGTest.TestJT1 ();
        Wr.PutText (Stdio.stdout, "JT2 test\n");
        RCGTest.TestJT2 ();
        Wr.PutText (Stdio.stdout, "JT3 test\n");
        RCGTest.TestJT3 ();
        Wr.PutText (Stdio.stdout, "Raise test\n");
        RCGTest.RaiseTest ();
        Wr.PutText (Stdio.stdout, "Try test\n");
        RCGTest.TryTest ();
        Wr.PutText (Stdio.stdout, "Lock test\n");
        RCGTest.LockTest ();

      ELSIF pp.testNext("iptest1") THEN
        RCGNet.NetTest1 ();

      ELSIF pp.testNext("iptest2") THEN
        RCGNet.NetTest2 ();

      ELSIF pp.testNext("alt") THEN
        RCGTest.AltTest ();

      ELSIF pp.testNext("measure") THEN
        RCGTest.MeasureJT1 ();

      ELSIF pp.testNext("raise") THEN
        RCGTest.RaiseTest ();

      ELSIF pp.testNext("try") THEN
        RCGTest.TryTest ();

      ELSIF pp.testNext("lock") THEN
        RCGTest.LockTest ();

      ELSIF pp.testNext("linear") THEN
        RCGTest.LinearTest ();

      ELSIF pp.testNext("jt1") THEN
        RCGTest.TestJT1 ();

      ELSIF pp.testNext("jt2") THEN
        RCGTest.TestJT2 ();
        
      ELSIF pp.testNext("jt3") THEN
        RCGTest.TestJT3 ();
        
      ELSIF pp.testNext("kill") THEN
        RCGCmd.Uninstall ();

      ELSIF pp.testNext("verbose") THEN
        verbose := NOT verbose;
        IF verbose THEN
          Wr.PutText (Stdio.stdout, "verbosity on\n");
        ELSE
          Wr.PutText (Stdio.stdout, "verbosity off\n");
        END;

      ELSIF pp.testNext("help") THEN
        Wr.PutText (Stdio.stdout, CommandName & " " & CommandHelp & "\n");
      END;
    EXCEPT
    | ParseParams.Error =>
      Wr.PutText(Stdio.stdout, "Unknown rcg command\n");
      Wr.PutText(Stdio.stdout, CommandName & " " & CommandHelp & "\n");
    END;

    RETURN TRUE;
  END Run;

(*
 * Initialization
 *)

VAR
  binding1, binding2: Dispatcher.Binding;

(*
PROCEDURE GuardOptimizer (guards: REF ARRAY OF Stitcher.GuardDesc;
                          <* UNUSED *> nArgs: INTEGER;
                          <* UNUSED *> res: BOOLEAN;
                          <* UNUSED *> saveRegs: BOOLEAN)
  : PROCANY =
  VAR
    tmp: REF ARRAY OF PROCANY;
    p: PROCANY;
  BEGIN
    tmp := NEW (REF ARRAY OF PROCANY, NUMBER (guards^));

    FOR i := 0 TO LAST (tmp^) DO
      tmp[i] := guards[i].guard;
      (* ignore other fields *)
    END;

    TRY
      p := Guard.OptimizeAll (tmp);
    EXCEPT ELSE
      Wr.PutText (Stdio.stdout, "Could not optimize guards\n");
      RETURN NIL;
    END;

    RETURN p;
  END GuardOptimizer;
*)

PROCEDURE Init (output: Wr.T) =
  BEGIN
    Stdio.Init(output);
    IF verbose THEN
      Wr.PutText(Stdio.stdout, "RCG >> Initializing\n");
    END;
    TRY
      (*
      EVAL Dispatcher.InstallHandler(Stitcher.GuardOptimizations,
                                     NIL,
                                     GuardOptimizer);
    *)
    EXCEPT
    | Dispatcher.Error => 
      Wr.PutText(Stdio.stdout, "ERROR >> RCG: installation failed\n");
    END;

    IF verbose THEN
      Wr.PutText(Stdio.stdout, "RCG >> Initialized\n");
    END;
  END Init;

(*
 * Deactivation
 *)

PROCEDURE Uninstall () =
  BEGIN
    TRY
      Dispatcher.Uninstall(binding1);
      Dispatcher.Uninstall(binding2);
    EXCEPT
    | Dispatcher.Error => 
      Wr.PutText(Stdio.stdout, "ERROR >> RCG: uninstallation failed\n");
    END;
  END Uninstall;


BEGIN
  Init (BootIO.Writer ());
  Wr.PutText (Stdio.stdout, "RCG: installed\n");
END RCGInit.

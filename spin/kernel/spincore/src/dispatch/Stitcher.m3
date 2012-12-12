(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 29-Jul-96  Charles Garrett (garrett) at the University of Washington
 *	All stub cloning procedures have been changed to take the
 *	 saveRegs argument, because the profiled versions all need to
 *	 use it.
 *
 * 20-May-96  Przemek Pardyak (pardy) at the University of Washington
 *	Changed the dispatcher interface.
 *
 * 20-Apr-95  Przemek Pardyak (pardy) at the University of Washington
 *	Converted to real PROCANY-s.
 *
 * 09-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	Removed uses of obsolete interface Clib.
 *
 * 28-Oct-95  Brian Bershad (bershad) at the University of Washington
 *	Created as passthru to implement safe interface.
 *
 *)
UNSAFE MODULE Stitcher;
IMPORT MachineStitcher, Dispatcher, AliasDesc, IO, MachineDispatcher, Fmt;
IMPORT DispatcherPrivate, Stitcher, Word;
IMPORT StrongRef; <* NOWARN *>

VAR
  mutex: MUTEX;

CONST
  verbose: BOOLEAN = FALSE;

PROCEDURE CloneStub(desc: AliasDesc.T; 
                    optLevel: INTEGER;
                    nArgs: INTEGER; 
                    res: BOOLEAN; 
                    saveRegs: BOOLEAN;
                    trace: BOOLEAN): PROCANY
                    RAISES { Dispatcher.Error }=
  VAR 
    clone     : PROCANY;
    guardList : UNTRACED REF ARRAY OF Word.T; <* NOWARN *>
    guardCnt  : INTEGER; <* NOWARN *>
    proc: PROCANY;
    ptr: Word.T;
    doLevel5 : BOOLEAN;
  BEGIN
    IF verbose THEN 
      IO.Put("Stitcher: optLevel " & Fmt.Int(optLevel) & " for 0x" &
        Fmt.Unsigned(LOOPHOLE(desc.eventDesc.event,INTEGER)) & "\n"); 
      IF optLevel < DispatcherPrivate.DefaultOptLevel THEN
        IO.Put("Stitcher: depreciated optLevel " & Fmt.Int(optLevel) & 
        " from " & Fmt.Int(DispatcherPrivate.DefaultOptLevel) & 
        " for 0x" & Fmt.Unsigned(LOOPHOLE(desc.eventDesc.event,INTEGER)) & 
        "\n"); 
      END;
    END;
    LOCK mutex DO 
      CASE optLevel OF
      | 0 => 
        clone := MachineStitcher.CloneTrampolineStub(desc,
                                        MachineDispatcher.DebugDispatchOffset,
                                        saveRegs);
      | 1 => 
        clone := MachineStitcher.CloneDefaultStub(desc, nArgs, res, saveRegs);
      | 2 => 
        clone := MachineStitcher.CloneStub(desc, nArgs, res,
                                          desc.eventDesc.nHandlers, saveRegs);
      | 3 =>
        clone := MachineStitcher.CloneUnrolledStub(desc, 
                                                   desc.handlers,
                                                   nArgs, 
                                                   res, 
                                                   desc.eventDesc.nHandlers, 
                                                   saveRegs,
                                                   FALSE,
                                                   NIL,
                                                   0);
      | 4 => 
        clone := MachineStitcher.CloneInlinedStub(desc, 
                                                  desc.handlers,
                                                  nArgs, 
                                                  res, 
                                                  desc.eventDesc.nHandlers, 
                                                  saveRegs,
                                                  FALSE,
                                                  trace);
      | 5 =>
        clone := MachineStitcher.CloneInlinedStub(desc, 
                                                  desc.handlers,
                                                  nArgs, 
                                                  res, 
                                                  desc.eventDesc.nHandlers, 
                                                  saveRegs,
                                                  TRUE,
                                                  trace);
      | 6 =>
        guardCnt  := desc.eventDesc.nHandlers;
        doLevel5 := FALSE;
        IF guardCnt < 2 THEN
          IF TRUE OR verbose THEN
            IO.Put("Stitcher: not enough guards to optimize for event 0x" &
              Fmt.Unsigned(LOOPHOLE(desc.eventDesc.event,INTEGER)) & "\n");
          END;
          doLevel5 := TRUE;
        ELSE
          IF TRUE OR verbose THEN
            IO.Put("Stitcher: optimizing guards for event 0x" &
              Fmt.Unsigned(LOOPHOLE(desc.eventDesc.event,INTEGER)) & "\n");
          END;

	  (* one extra for the pointer where combined guards go *)
	  guardList := NEW(UNTRACED REF ARRAY OF Word.T, 1+3*guardCnt);

          (* generate the outer clone and collect all the guards *)
	  clone := MachineStitcher.CloneUnrolledStub(desc, 
						     desc.handlers,
						     nArgs, 
						     res, 
						     desc.eventDesc.nHandlers, 
						     saveRegs,
						     TRUE,
						     ADR(guardList[0]),
						     guardCnt);

	  IF clone = NIL THEN
            IF TRUE OR verbose THEN 
              IO.Put("ERROR >> guard optimizations impossible for event: " & 
                Fmt.Unsigned(LOOPHOLE(clone,INTEGER)) & "\n");
	    END;
            doLevel5 := TRUE;
          ELSE
            IO.Put("Stitcher: clone for optimized guards - 0x" &
              Fmt.Unsigned(LOOPHOLE(clone,INTEGER)) & "\n");

            IF TRUE OR verbose THEN
              IO.Put("Stitcher: doing guard optimizations\n");
            END;

            (* we will patch the clone at this address *)
            ptr := guardList[0];
            WITH guards = NEW(REF ARRAY OF GuardDesc, guardCnt) DO
              FOR i := 0 TO guardCnt-1 DO
                guards[i].guard      := LOOPHOLE(guardList[1+i*3], PROCANY);
                guards[i].closure    := LOOPHOLE(guardList[1+i*3+1], REFANY);
                guards[i].useClosure := LOOPHOLE(guardList[1+i*3+2], 
                                                 BITS BITSIZE(Word.T) FOR BOOLEAN);
              END;
              proc := Stitcher.GuardOptimizations(guards,
                                                  nArgs, res, saveRegs);
              IF proc = NIL THEN
                IF TRUE OR verbose THEN 
                  IO.Put("ERROR >> guard optimizations failed for event: " & 
                    Fmt.Unsigned(LOOPHOLE(desc.eventDesc.event,INTEGER)) &
                    "\n");
                END;
                doLevel5 := TRUE;
              ELSE
                IO.Put("Stitcher: optimized guards - 0x" &
                  Fmt.Unsigned(LOOPHOLE(proc,INTEGER)) & "\n");

                (* put it into the original clone *)
                MachineStitcher.PatchCall(ptr, proc);

                IF TRUE OR verbose THEN 
                  IO.Put("Stitcher: guards optimized for event: " & 
                    Fmt.Unsigned(LOOPHOLE(desc.eventDesc.event,INTEGER)) &
                    "\n");
                END;
              END;
	    END;
	  END;
        END;
      | -1 =>
        clone := MachineStitcher.CloneTrampolineStub(desc, 
                                       MachineDispatcher.AsynchDispatchOffset,
                                       saveRegs);
      ELSE
        clone := NIL;
      END;

      IF doLevel5 THEN
        clone := MachineStitcher.CloneInlinedStub(desc, 
                                                  desc.handlers,
                                                  nArgs, 
                                                  res, 
                                                  desc.eventDesc.nHandlers, 
                                                  saveRegs,
                                                  TRUE,
                                                  trace);
      END;

      IF clone = NIL THEN 
        IO.PutError("Stitcher: code generation failed\n");
        RAISE Dispatcher.Error(Dispatcher.ErrorCode.CodeGenerationError);
      END;
    END;

    IF verbose THEN 
      IO.Put("Stitcher: optLevel " & Fmt.Int(optLevel) & " for 0x" &
        Fmt.Unsigned(LOOPHOLE(desc.eventDesc.event,INTEGER)) & " -> 0x" &
        Fmt.Unsigned(LOOPHOLE(clone,INTEGER)) & "\n"); 
    END;

    RETURN clone;
  END CloneStub;

PROCEDURE Optimize(<* UNUSED *> clone: PROCANY; 
                   <* UNUSED *> optLevel: INTEGER): PROCANY =
  BEGIN
    RETURN NIL;
  END Optimize;

PROCEDURE GuardOptimizations(<*UNUSED*> guards: REF ARRAY OF GuardDesc;
			     <*UNUSED*> nArgs: INTEGER; 
			     <*UNUSED*> res: BOOLEAN; 
			     <*UNUSED*> saveRegs: BOOLEAN):PROCANY=
  BEGIN
    IF TRUE OR verbose THEN
      IO.Put("Stitcher: no guard optimizations\n");
    END;

    RETURN NIL;
  END GuardOptimizations;

PROCEDURE TestGuardOptimizations(guards: REF ARRAY OF GuardDesc;
			         nArgs: INTEGER; 
			         res: BOOLEAN; 
			         saveRegs: BOOLEAN): PROCANY =
  VAR
    proc: PROCANY;
  BEGIN
    IF TRUE OR verbose THEN
      IO.Put("Stitcher: test guard optimizations\n");
    END;

    (* create a procedure that calls all of those guards *)
    proc := MachineStitcher.CloneFromList(ADR(guards[0]),
                                          NUMBER(guards^),
                                          nArgs, res, saveRegs);

    RETURN proc;
  END TestGuardOptimizations;

PROCEDURE Generate(<* UNUSED *> outProc: PROCANY;
                   <* UNUSED *> inProcs: REF ARRAY OF PROCANY;
                   <* UNUSED *> inPos: REF ARRAY OF ADDRESS): PROCANY =
  BEGIN
    RETURN NIL;
  END Generate;

PROCEDURE GetLinear (proc: PROCANY;
                     nArgs: INTEGER;
                     useClosure: BOOLEAN;
                     closure: REFANY): PROCANY =
  BEGIN
    RETURN MachineStitcher.GetLinear(proc, nArgs, useClosure, closure);
  END GetLinear;

PROCEDURE Init () =
  BEGIN
    mutex := NEW(MUTEX);
    LOCK mutex DO
      MachineStitcher.Init();
    END;
    TRY
      EVAL Dispatcher.InstallHandler(Stitcher.GuardOptimizations,
                                     NIL,
                                     TestGuardOptimizations);
    EXCEPT
    | Dispatcher.Error =>
      IO.PutError("Stitcher failed to install test guard optimizations\n");
    END;
  END Init;

BEGIN
END Stitcher.

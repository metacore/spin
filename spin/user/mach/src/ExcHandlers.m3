(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 04-Apr-96  David Dion (ddion) at the University of Washington
 *	Updated for spin-20
 *
 * 28-Dec-95  Emin Gun Sirer (egs) at the University of Washington
 *	Changed to conform to the new syscall interface.
 *
 * 15-Dec-95  David Dion (ddion) at the University of Washington
 *	Updated to spin-8 for CVS.
 *
 * 22-Aug-95  David Dion (ddion) at the University of Washington
 *       Created.
 *
 *)

MODULE ExcHandlers;
IMPORT CPU, Strand, Word, UserSpaceThread, Space;
IMPORT MarshallExc;
IMPORT Syscall;
IMPORT HandlerUtils;
IMPORT TimeHandlers;
(*
FROM HandlerUtils IMPORT Debug;
FROM HandlerUtils IMPORT Print;
*)

CONST
  (* Exception types - swiped from pre-processed osf/server/alpha/trap.c *)
  EXC_SYSCALL = 6;
  (*EXC_MACH_SYCALL = 7;*)
  WordSize = BYTESIZE(Word.T);
  StateSize = BYTESIZE(MarshallExc.AlphaThreadState);

VAR
  (* Identity check.  Should probably incorporate this sometime. *)
  serverTask: Word.T;


(*
 * Catch UNIX exceptions and ship them back up to the server.
 *)
PROCEDURE exc_task_get_next_exc(strand: Strand.T; 
                                VAR ms: CPU.SavedState) =(* -90 *)
  VAR
    serverUth: UserSpaceThread.T;
    serverSpace: Space.T;
    extraArgs: ARRAY[6..7] OF Word.T;
    excThread: UserSpaceThread.T;
    excType: Word.T;
    code0, code1: Word.T;
    statePtr: REF MarshallExc.AlphaThreadState;
    stateCount: CARDINAL;
    stateCopy: Word.T;
    handle: MarshallExc.Handle;
  BEGIN
    IF TimeHandlers.TimersEnabled THEN
      TimeHandlers.TimerSample(4, strand);
    END;
    IF ms.a1 = 0 AND ms.a2 = 0 AND ms.a3 = 0 AND ms.a4 = 0 THEN
      (* 
       * This is the result of the use_simple_exception_interface() call.
       *
       * The idea is that there is a server thread handling system calls
       * for every address space.  So, this call should register the
       * current thread to handle syscalls for the task in ms.a0.  We
       * are not tracking this until the Mach task extension is available.
       *)
      serverTask := ms.a0;
    ELSE
      (*
       * This thread has been dropped to catch a UNIX exception and ship it
       * back up to the server to be processed.  This thread also carries
       * in its instate (arg6 = extraArgs6) the result of the previous system 
       * call it handled.  
       *)

      (*
       * Get the caller space.
       *)
      serverUth := NARROW(strand, UserSpaceThread.T);
      serverSpace := UserSpaceThread.GetSpace(serverUth);

      (*
       * There are eight parameters for this system call.  The first six come
       * through ms.a0 - ms.a5.  The other two are pushed on the stack. 
       * Get these off the stack now.
       *)
      IF NOT HandlerUtils.GetNArgsOffStack(serverSpace, 
                                           ms.usp,
                                           NUMBER(extraArgs), 
                                           extraArgs) THEN
        HandlerUtils.PrintError("exc_task_get_next_exc:  can't get args " &
          "off stack.\n");
        ms.v0 := Syscall.Failure;
        RETURN;
      END;

      (*
       * If this thread just handled an application system call, then it
       * is already registered on a marshalling handle.  Get that handle
       * now.  If TRUE returned, handle found.  Otherwise, this server
       * thread is looking for its first system call.
       *)
      IF MarshallExc.ServerGetHandleFromThread(handle, 
                                               statePtr, 
                                               serverUth) THEN
        (*
         * Get the result state from the server's address state.
         *)
        WITH stateArray = VIEW(statePtr^, ARRAY[0..StateSize-1] OF CHAR) DO
          IF NOT HandlerUtils.CopyIn(serverSpace, 
                                     extraArgs[6], 
                                     stateArray,
                                     StateSize) THEN
            HandlerUtils.PrintError("exc_task_get_next_exc: " &
              "CopyIn exception.\n");
            ms.v0 := Syscall.Failure;
            RETURN;
          END;
        END;

        (*
         * Return result to client.
         *)
        MarshallExc.ServerRequestCompleted(handle);
      END;

      (*
       * Get another request handle.  That is, offer to handle the
       * next system call.  For this, we need to know the task this
       * thread handles syscalls for.  The external ref for that
       * task is in register a0.
       *)
      IF TimeHandlers.TimersEnabled THEN
        TimeHandlers.TimerSample(5, strand);
      END;
      WITH appSpace = HandlerUtils.Internalize(serverUth, ms.a0) DO
        handle := MarshallExc.ServerGetNextHandle(serverUth, appSpace);
      END;
      IF TimeHandlers.TimersEnabled THEN
        TimeHandlers.TimerSample(2, strand);
      END;

      (*
       * Get request data using the handle.
       *)
      MarshallExc.ServerGetSyscallData(handle,
                                       excThread,
                                       excType,
                                       code0,
                                       code1,
                                       statePtr,
                                       stateCount);
      (*
       * Using VM operations, get these arguments up to the server's address
       * space and return.  The following data (type) will be copied:
       *
       * excThread - Mach port (Word.T)
       * excType - C int (32-bit integer)
       * code0 - C int (32-bit integer)
       * code1 - C int (32-bit integer)
       * state - AlphaThreadState (256-byte MarshallExc.AlphaThreadState)
       * stateCount - C long (Word.T)
       *)
      WITH excThreadExtern = HandlerUtils.Externalize(serverUth, excThread),
           copyArray = VIEW(excThreadExtern, ARRAY[0..WordSize-1] OF CHAR) DO
        IF NOT HandlerUtils.CopyOut(serverSpace, copyArray, ms.a1, WordSize)
         THEN
          HandlerUtils.PrintError("exc_thread_get_next_exc: " & 
            "CopyOut exception.\n");
          ms.v0 := Syscall.Failure;
          RETURN;
        END;
      END;
      WITH copyArray = VIEW(excType, ARRAY[0..3] OF CHAR) DO
        IF NOT HandlerUtils.CopyOut(serverSpace, copyArray, ms.a2, 4)
         THEN
          HandlerUtils.PrintError("exc_thread_get_next_exc: " & 
            "CopyOut exception.\n");
          ms.v0 := Syscall.Failure;
          RETURN;
        END;
      END;
      WITH copyArray = VIEW(code0, ARRAY[0..3] OF CHAR) DO
        IF NOT HandlerUtils.CopyOut(serverSpace, copyArray, ms.a3, 4)
         THEN
          HandlerUtils.PrintError("exc_thread_get_next_exc: " & 
            "CopyOut exception.\n");
          ms.v0 := Syscall.Failure;
          RETURN;
        END;
      END;
      WITH copyArray = VIEW(code1, ARRAY[0..3] OF CHAR) DO
        IF NOT HandlerUtils.CopyOut(serverSpace, copyArray, ms.a4, 4)
         THEN
          HandlerUtils.PrintError("exc_thread_get_next_exc: " & 
            "CopyOut exception.\n");
          ms.v0 := Syscall.Failure;
          RETURN;
        END;
      END;
      WITH stateArray = VIEW(statePtr^, ARRAY[0..StateSize-1] OF CHAR) DO
        IF NOT HandlerUtils.CopyOut(serverSpace, 
                                    stateArray, 
                                    extraArgs[6], 
                                    StateSize) THEN
          HandlerUtils.PrintError("exc_thread_get_next_exc: " & 
            "CopyOut exception.\n");
          ms.v0 := Syscall.Failure;
          RETURN;
        END;
      END;
      stateCopy := stateCount; (* cannot view a CARDINAL *)
      WITH copyArray = VIEW(stateCopy, ARRAY[0..WordSize-1] OF CHAR) DO
        IF NOT HandlerUtils.CopyOut(serverSpace, 
                                    copyArray, 
                                    extraArgs[7], 
                                    WordSize) THEN
          HandlerUtils.PrintError("exc_thread_get_next_exc: " & 
            "CopyOut exception.\n");
          ms.v0 := Syscall.Failure;
          RETURN;
        END;
      END;
    END;
    ms.v0 := Syscall.KERN_SUCCESS;
    IF TimeHandlers.TimersEnabled THEN
      TimeHandlers.TimerSample(3, strand);
    END;
  END exc_task_get_next_exc;

PROCEDURE syscall_handler(strand: Strand.T; VAR ms: CPU.SavedState) =
  VAR 
    syscallStatePtr: REF MarshallExc.AlphaThreadState;
  BEGIN
    IF TimeHandlers.TimersEnabled THEN
      TimeHandlers.TimerSample(1, strand);
    END;
    (*
     * Start by getting a handle out of the pool.
     *)
    WITH handle = MarshallExc.ClientGetHandle(syscallStatePtr) DO
      (*
       * Construct a syscall state structure.  All registers must be copied.
       * Some syscalls, such as fork(), need the entire state.
       *)
      syscallStatePtr^.v0 := ms.v0;
      syscallStatePtr^.t0 := ms.t0;
      syscallStatePtr^.t1 := ms.t1;
      syscallStatePtr^.t2 := ms.t2;
      syscallStatePtr^.t3 := ms.t3;
      syscallStatePtr^.t4 := ms.t4;
      syscallStatePtr^.t5 := ms.t5;
      syscallStatePtr^.t6 := ms.t6;
      syscallStatePtr^.t7 := ms.t7;
      syscallStatePtr^.s0 := ms.s0;
      syscallStatePtr^.s1 := ms.s1;
      syscallStatePtr^.s2 := ms.s2;
      syscallStatePtr^.s3 := ms.s3;
      syscallStatePtr^.s4 := ms.s4;
      syscallStatePtr^.s5 := ms.s5;
      syscallStatePtr^.s6 := ms.s6;
      syscallStatePtr^.a0 := ms.a0;
      syscallStatePtr^.a1 := ms.a1;
      syscallStatePtr^.a2 := ms.a2;
      syscallStatePtr^.a3 := ms.a3;
      syscallStatePtr^.a4 := ms.a4;
      syscallStatePtr^.a5 := ms.a5;
      syscallStatePtr^.t8 := ms.t8;
      syscallStatePtr^.t9 := ms.t9;
      syscallStatePtr^.t10 := ms.t10;
      syscallStatePtr^.t11 := ms.t11;
      syscallStatePtr^.ra := ms.ra;
      syscallStatePtr^.pv := ms.pv;
      syscallStatePtr^.at := ms.at;
      syscallStatePtr^.gp := ms.gp;
      syscallStatePtr^.sp := ms.usp;
      syscallStatePtr^.pc := ms.pc;
      (*
       * Send the marshalled data to the server.  (Note that the state
       * has already been copied over through a ref.)
       *)
      MarshallExc.ClientSetSyscallData(handle,
                                       NARROW(strand, UserSpaceThread.T),
                                       EXC_SYSCALL,
                                       ms.v0,
                                       16_11, (* stolen from OSF/1 on Mach *)
                                       32(* num Word.Ts in AlphaThreadState*));

      (*
       * Kick the server thread which handles syscalls for this task.
       * Identify this task by its Space.T.
       *)
      WITH uth = NARROW(strand, UserSpaceThread.T),
           space = UserSpaceThread.GetSpace(uth) DO
        MarshallExc.ClientKickServerAndWaitForState(handle, space);
      END;

      (*
       * Copy results in syscallState back to machine saved state.
       * Since not all registers are restored after a syscall, we only
       * need to copy back those which are.  If TRAP_spin_returnsyncinterrupt
       * in Core.s changes to restore more regs, we need to deal with that
       * here.
       *)
      ms.v0 := syscallStatePtr^.v0;
      ms.t0 := syscallStatePtr^.t0;
      ms.t1 := syscallStatePtr^.t1;
      ms.t2 := syscallStatePtr^.t2;
      ms.t3 := syscallStatePtr^.t3;
      ms.t4 := syscallStatePtr^.t4;
      ms.t5 := syscallStatePtr^.t5;
      ms.t6 := syscallStatePtr^.t6;
      ms.t7 := syscallStatePtr^.t7;
      ms.s0 := syscallStatePtr^.s0;
      ms.s1 := syscallStatePtr^.s1;
      ms.s2 := syscallStatePtr^.s2;
      ms.s3 := syscallStatePtr^.s3;
      ms.s4 := syscallStatePtr^.s4;
      ms.s5 := syscallStatePtr^.s5;
      ms.s6 := syscallStatePtr^.s6;
      ms.a0 := syscallStatePtr^.a0;
      ms.a1 := syscallStatePtr^.a1;
      ms.a2 := syscallStatePtr^.a2;
      ms.a3 := syscallStatePtr^.a3;
      ms.a4 := syscallStatePtr^.a4;
      ms.a5 := syscallStatePtr^.a5;
      ms.t8 := syscallStatePtr^.t8;
      ms.t9 := syscallStatePtr^.t9;
      ms.t10 := syscallStatePtr^.t10;
      ms.t11 := syscallStatePtr^.t11;
      ms.ra := syscallStatePtr^.ra;
      ms.pv := syscallStatePtr^.pv;
      ms.at := syscallStatePtr^.at;
      ms.gp := syscallStatePtr^.gp;
      ms.usp := syscallStatePtr^.sp;
      ms.pc := syscallStatePtr^.pc;

      (*
       * Give back handle.
       *)
      MarshallExc.ClientReturnHandle(handle);
    END;
    IF TimeHandlers.TimersEnabled THEN
      TimeHandlers.TimerSample(6, strand);
    END;
  END syscall_handler;


BEGIN
  MarshallExc.Init();
END ExcHandlers.





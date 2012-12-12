(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 04-Apr-96  David Dion (ddion) at the University of Washington
 *	Updated for spin-20
 *
 * 15-Jan-96  David Dion (ddion) at the University of Washington
 *	Added empty FPUState to thread_set_state syscall.
 *
 * 28-Dec-95  Emin Gun Sirer (egs) at the University of Washington
 *	Changed to conform to the new syscall interface.
 *
 * 15-Dec-95  David Dion (ddion) at the University of Washington
 *	Updated to spin-8 for CVS.
 *
 * 04-Aug-95  David Dion (ddion) at the University of Washington
 *      Created to separate cthread support calls from other system calls.
 *)
MODULE ThreadHandlers;

IMPORT UserSpaceThread, Space, CPU, Word, Strand;
IMPORT Fmt, Textify;  (* mainly for debugging *)
IMPORT Syscall;
IMPORT HandlerUtils;
FROM HandlerUtils IMPORT Debug;
FROM HandlerUtils IMPORT Print;

CONST
  WordSize = BYTESIZE(Word.T);


PROCEDURE thread_switch(<*UNUSED*>strand: Strand.T; 
                        VAR ms: CPU.SavedState) = (* -61 *)
  BEGIN
    (*
     * Implement this about as simply as possible.  We don't care about
     * handoffs or scheduling hints for now.  Just yield.
     *)
    (* IF Debug THEN Print("thread_switch: calling Strand.Yield.\n"); END; *)
    Strand.Yield();
    ms.v0 := Syscall.KERN_SUCCESS;
  END thread_switch;

PROCEDURE thread_create(strand: Strand.T; 
                        VAR ms: CPU.SavedState) = (* -96 *)
  VAR
    callerUthread: UserSpaceThread.T;
    callspace: Space.T;
    space: Space.T;
    newUthread: UserSpaceThread.T;
    extern: Word.T;
  BEGIN
    (* Get calling thread and space. *)
    callerUthread := NARROW(strand, UserSpaceThread.T);
    callspace := UserSpaceThread.GetSpace(callerUthread);

    (* Figure out which space UST should be allocated in. *)
    space := NARROW(HandlerUtils.Internalize(callerUthread, ms.a0), 
                    Space.T);
    (* Get the UST *)
    newUthread := UserSpaceThread.Create(space);

    (* Write back the externalized UST REF *)
    extern := HandlerUtils.Externalize(callerUthread, newUthread);

    WITH copyArray = VIEW(extern, ARRAY[0..WordSize-1] OF CHAR) DO
      IF NOT HandlerUtils.CopyOut(callspace, copyArray, ms.a1, WordSize) THEN
        HandlerUtils.PrintError("thread_create: CopyOut exception.\n");
        ms.v0 := Syscall.Failure;
        RETURN;
      END;
    END;
    IF Debug THEN Print("\twrote back " & Fmt.Unsigned(extern) & "\n"); END;
    IF Debug THEN Print("\tcreated thread " &
      Textify.Ref(newUthread) & " in space 0x" &
      Textify.Ref(space) & "\n"); 
    END;
    ms.v0 := Syscall.KERN_SUCCESS;
  END thread_create;

PROCEDURE thread_set_state(strand: Strand.T; 
                           VAR ms: CPU.SavedState) = (* -97 *)
  VAR
    state: UserSpaceThread.State;
    targetUthread: UserSpaceThread.T;
    newState: ARRAY[0..31] OF Word.T;
  BEGIN
    (* Make sure the state is what we expect, 32 registers in 
       alpha_thread_state. *) 
    IF ms.a3 # 32 THEN
      HandlerUtils.PrintError("thread_set_state: don't understand newState. ");
      HandlerUtils.PrintError("size = " & Fmt.Unsigned(ms.a3) & "\n");
      ms.v0 := Syscall.KERN_INVALID_ARGUMENT;
      RETURN;
    END;

    WITH callerUthread = NARROW(strand, UserSpaceThread.T),
         space = UserSpaceThread.GetSpace(callerUthread),
         stateArray = VIEW(newState, ARRAY [0..255] OF CHAR) DO
      IF NOT HandlerUtils.CopyIn(space, ms.a2, stateArray, 256) THEN
        HandlerUtils.PrintError("thread_set_state: CopyIn exception.\n");
        ms.v0 := Syscall.Failure;
        RETURN;
      END;

      (* Internalize the target thread. *)
      targetUthread := 
          NARROW(HandlerUtils.Internalize(callerUthread, ms.a0),
                 UserSpaceThread.T);
      (* make sure target_thread isn't self *)
      IF targetUthread = callerUthread THEN
        ms.v0 := Syscall.KERN_INVALID_ARGUMENT;
        RETURN;
      END;
    END;

    (* Allocate the UST state.  Unfortunately, registers are ordered
       differently between the UST state and the AlphaThreadState.  This
       means values will be copied over one at a time. *)
    state := NEW(UserSpaceThread.State);
    (* Allocate CPUState and FPUState.  However, only set CPUState. *)
    state.cpustate := NEW(REF CPU.SavedState);
    state.fpustate := NEW(REF CPU.FloatRegs);

    (* Copy data over to state.cpustate.  Register numbers taken from 
       Alpha Architecture Reference Manual and matched to "Software Name"
       in CPU.SavedState and thread_status.h. *)
    state.cpustate^.v0 := newState[0];
    state.cpustate^.t0 := newState[1];
    state.cpustate^.t1 := newState[2];
    state.cpustate^.t2 := newState[3];
    state.cpustate^.t3 := newState[4];
    state.cpustate^.t4 := newState[5];
    state.cpustate^.t5 := newState[6];
    state.cpustate^.t6 := newState[7];
    state.cpustate^.t7 := newState[8];
    state.cpustate^.s0 := newState[9];
    state.cpustate^.s1 := newState[10];
    state.cpustate^.s2 := newState[11];
    state.cpustate^.s3 := newState[12];
    state.cpustate^.s4 := newState[13];
    state.cpustate^.s5 := newState[14];
    state.cpustate^.s6 := newState[15];
    state.cpustate^.a0 := newState[16];
    state.cpustate^.a1 := newState[17];
    state.cpustate^.a2 := newState[18];
    state.cpustate^.a3 := newState[19];
    state.cpustate^.a4 := newState[20];
    state.cpustate^.a5 := newState[21];
    state.cpustate^.t8 := newState[22];
    state.cpustate^.t9 := newState[23];
    state.cpustate^.t10 := newState[24];
    state.cpustate^.t11 := newState[25];
    state.cpustate^.ra := newState[26];
    state.cpustate^.pv := newState[27];
    state.cpustate^.at := newState[28];
    state.cpustate^.gp := newState[29];
    state.cpustate^.usp := newState[30];
    state.cpustate^.pc := newState[31]; (* pc is 31st, not zero *)
    IF Debug THEN Print("\tthread_set_state: usp = 0x" & 
      Fmt.Unsigned(state.cpustate^.usp) & " pc = 0x" &
      Fmt.Unsigned(state.cpustate^.pc) & " for strand 0x" &
      Textify.Ref(targetUthread) & "\n"); END;

    (* Finally call UST.SetState *)
    UserSpaceThread.SetState(targetUthread, state);
    ms.v0 := Syscall.KERN_SUCCESS;
  END thread_set_state;

PROCEDURE thread_resume(strand: Strand.T; 
                        VAR ms: CPU.SavedState) = (* -98 *)
  VAR
    state: UserSpaceThread.State;
  BEGIN
    IF Debug THEN Print("\tthread ms.a0 = " & 
      Fmt.Unsigned(ms.a0) & "\n"); END;
    WITH targetUthread = 
        NARROW(HandlerUtils.Internalize(NARROW(strand, 
                                                    UserSpaceThread.T), 
                                           ms.a0),
               UserSpaceThread.T) DO

      IF Debug THEN 
        Print("\tresuming thread " &
          Textify.Ref(targetUthread) & "\n");
        state := NEW(UserSpaceThread.State);
        state.cpustate := NEW(REF CPU.SavedState);
        UserSpaceThread.GetState(targetUthread, state);
        Print("\tthread_resume: pc = " & 
          Fmt.Unsigned(state.cpustate^.pc) & "\n");
        Print("\tthread_resume: a0 = " & 
          Fmt.Unsigned(state.cpustate^.a0) & "\n");
        Print("\tthread_resume: a3 = " & 
          Fmt.Unsigned(state.cpustate^.a3) & "\n");
        Print("\tthread_resume: a4 = " & 
          Fmt.Unsigned(state.cpustate^.a4) & "\n");
      END;

      UserSpaceThread.Resume(targetUthread);
    END;
    ms.v0 := Syscall.KERN_SUCCESS;
  END thread_resume;

PROCEDURE thread_rendezvous(strand: Strand.T; 
                            VAR ms: CPU.SavedState) = (* -57 *)
  VAR
    stopme: BOOLEAN;
    other: UserSpaceThread.T;
    self: UserSpaceThread.T;
  BEGIN
    (* Convert Word.T argument into BOOLEAN. *)
    IF ms.a0 # 0 THEN 
      stopme := TRUE; 
    ELSE 
      stopme := FALSE;
    END;
    IF stopme THEN
      IF Debug THEN Print("\tstopme = TRUE\n"); END;
    ELSE
      IF Debug THEN Print("\tstopme = FALSE\n"); END;
    END;
    IF Debug THEN Print("\textern otherthread = " & 
      Fmt.Unsigned(ms.a1) & "\n"); END;

    (* Get threadself argument. *)
    self := NARROW(strand, UserSpaceThread.T);

    (* Set up other thread argument.  Important: if the passed argument
       is 0, don't try to Internalize it because the ensuing NARROW
       will fail! *)
    IF ms.a1 # 0 THEN
      other := NARROW(HandlerUtils.Internalize(self, ms.a1), 
                             UserSpaceThread.T);
    ELSE
      other := NIL;
    END;

    (* Call the extension rendezvous instead of UST.Rendezvous.  I don't
       really think Rendezvous belongs in UST.[im]3. *)
    Rendezvous(stopme, other, self);

    ms.v0 := Syscall.KERN_SUCCESS;
  END thread_rendezvous;

PROCEDURE Rendezvous(stopSelf: BOOLEAN; 
                     otherUthread: UserSpaceThread.T; 
                     Uthreadself: UserSpaceThread.T) =
  BEGIN
    IF otherUthread # NIL THEN
      UserSpaceThread.Resume(otherUthread);
    END;

    IF stopSelf THEN
      UserSpaceThread.Suspend(Uthreadself);
    END;
  END Rendezvous;

BEGIN
END ThreadHandlers.


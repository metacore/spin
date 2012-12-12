(*
 * Copyright 1997, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 17-Feb-97   (negrin) at the University of Washington
 *	Changed  states that cause a fin to be sent (Fin_Wait1 and Closing)
 *      to call Output, to force sending a fin if there is not data to be sent.
 *
 * 11-Feb-97   (negrin) at the University of Washington
 *	fixed bug in Fin_wait1.  Shouldn't do anything if user tries
 *	to go to the Fin_wait1 state while in the fin_wait1 state.
 *
 * 06-Feb-97  Marc Fiuczynski (mef) at the University of Washington
 *	In the Time_Wait proc we switch to the "expectedState", which
 *	should always be "Closed"; however, I don't actually check for
 *	that.  It is unclear how much checking we want to do, as it may
 *	be to restrictive.
 *
 *	Where possible the state procs use the tcp flag constants
 *	directly as an arg to Output, rather than assigning them to local
 *	variables.  The compiler may take care of this automatically when
 *	optimizations are turned on, but lets not take any changes.
 *	
 * 05-Feb-97  Marc Fiuczynski (mef) at the University of Washington
 *	The state procs are now exposed as methods in the TcpCtrlBlock.T
 *	object.  The changestate procedure now invokes the tcb's methods
 *	rather than indirecting through a stateproc array.  I hope that
 *	the compiler wont produce braindead code for the case statement.
 *	
 *	Since TCBs are object there was no need to pass them by VAR
 *	(which wouldn't work with objects anyways).  
 *	
 * 16-Jan-97  Richard Negrin (negrin) at the University of Washington
 *	created.
 *)

MODULE LocalSM EXPORTS LocalSM, LocalSMPrivate;
IMPORT IO;
IMPORT TcpSM;
IMPORT M3Tcp;
IMPORT TcpPktFormat;
IMPORT TcpCtrlBlock;
IMPORT Clock; 
IMPORT TcpDebug;
IMPORT Thread;

(*Declarations*)
VAR gotoNext : BOOLEAN := FALSE; (* Tells whether to go to the *)

CONST
  syn    = TcpPktFormat.Flags{TcpPktFormat.Flag.syn};
  ack    = TcpPktFormat.Flags{TcpPktFormat.Flag.ack};
  fin    = TcpPktFormat.Flags{TcpPktFormat.Flag.fin};

<*UNUSED*>
CONST
  finack = fin + ack;
  synack = syn + ack; 
  rst    = TcpPktFormat.Flags{TcpPktFormat.Flag.rst};


(****************************************************************************************)
(*Public Function Implementations*)
(****************************************************************************************)
PROCEDURE ChangeState(tcb: TcpCtrlBlock.T; expectedState : TcpSM.States) = 
  BEGIN


(*    Make sure if you call output that you release the inOutMutex.  That is
      we use Thread.Acquire and not Lock, so we can release the mutex in the functions
      below.  The locking protocol needs to be more fine grained.*)

      TRY 
        Thread.Acquire(tcb.inOutMutex);

      
      REPEAT
        (* XXX ugh.  I don't like doing it this way.
           We'll see how the compiler deals with this
           and then figure out how to speed it up . *)

        (* invoke the appropriate method based on the current state of
           the tcb *)

        CASE tcb.t_state OF
        | TcpSM.States.Closed     =>
          tcb.localClosed(expectedState);
        | TcpSM.States.Listen     =>
          tcb.localListen(expectedState);
        | TcpSM.States.SYN_Sent   =>
          tcb.localSYN_Sent(expectedState);
        | TcpSM.States.SYN_Rcvd   =>
          tcb.localSYN_Rcvd(expectedState);
        | TcpSM.States.Estab      =>
          tcb.localEstab(expectedState);
        | TcpSM.States.FIN_Wait1   =>
          tcb.localFIN_Wait1(expectedState);
        | TcpSM.States.FIN_Wait2  =>
          tcb.localFIN_Wait2(expectedState);
        | TcpSM.States.Close_Wait =>
          tcb.localClose_Wait(expectedState);
        | TcpSM.States.Closing    =>
          tcb.localClosing(expectedState);
        | TcpSM.States.Last_Ack   =>
          tcb.localLast_Ack(expectedState);
        | TcpSM.States.Time_Wait  =>
          tcb.localTime_Wait(expectedState);
        END;
        (* gotoNext should really be set as the return value from calling 
           one of the above state procedures. *)

        gotoNext := FALSE; (*always initialize this to false every time we start*)
      UNTIL gotoNext = FALSE;

      IF (tcb.t_state # expectedState) THEN  (*Should raise some error or exception*)
        IF TcpDebug.debugLevel > 0 THEN IO.Put("State Machine Error\n");END;
      END;
    
   FINALLY
   Thread.Release(tcb.inOutMutex);
   END;

  END ChangeState;

(****************************************************************************************)
(*Private Function Implementations*)
(****************************************************************************************)

PROCEDURE Closed(tcb: TcpCtrlBlock.T; expectedState : TcpSM.States) =
  BEGIN

    IF (expectedState = TcpSM.States.SYN_Sent) THEN (*Active Open*)
      Clock.SetAlarm(tcb.t_srtt, TcpSM.SynTimeout, tcb);
      M3Tcp.OutputControl(tcb, syn); 
      tcb.t_state := TcpSM.States.SYN_Sent;

    ELSIF (expectedState = TcpSM.States.Listen) THEN (*Passive Open*)
      tcb.t_state := TcpSM.States.Listen;
      
    END;
      
  END Closed;


PROCEDURE Listen(tcb: TcpCtrlBlock.T; expectedState : TcpSM.States) =
  BEGIN

    IF (expectedState = TcpSM.States.Closed) THEN
      M3Tcp.CleanUp(tcb);
    ELSE
      IF TcpDebug.debugLevel > 0 THEN 
        IO.Put("User ");
        IO.Put(tcb.name);
        IO.Put(" tried to change state when in Listen State\n");
      END;
    END;

  END Listen;


PROCEDURE SYN_Sent(tcb: TcpCtrlBlock.T; expectedState : TcpSM.States) =
  BEGIN

    IF (expectedState = TcpSM.States.Closed) THEN
      M3Tcp.CleanUp(tcb);
    END;
    
  END SYN_Sent;


PROCEDURE SYN_Rcvd(tcb: TcpCtrlBlock.T; expectedState : TcpSM.States) =
  BEGIN

    (*expected should be FIN_Wait1 not closed, otherwise we will
      get a state error.*)
    IF (expectedState = TcpSM.States.Closed) THEN (*Active close*)

      tcb.t_state := TcpSM.States.FIN_Wait1;

      TRY 
        Thread.Release(tcb.inOutMutex);
        M3Tcp.Output(tcb,NIL);
      FINALLY
        Thread.Acquire(tcb.inOutMutex);
      END;

    END;

  END SYN_Rcvd;


PROCEDURE Estab(tcb: TcpCtrlBlock.T; expectedState : TcpSM.States) =

  BEGIN
    IF (expectedState = TcpSM.States.FIN_Wait1) THEN (*Active close*)
      tcb.t_state := TcpSM.States.FIN_Wait1;

      TRY 
        Thread.Release(tcb.inOutMutex);
        M3Tcp.Output(tcb,NIL);
      FINALLY
        Thread.Acquire(tcb.inOutMutex);
      END;

    ELSE
      IF TcpDebug.debugLevel > 0 THEN 
        IO.Put("User ");
        IO.Put(tcb.name);
        IO.Put(" tried to change state when in Estab State\n"); 
      END;
    END;

  END Estab;
  


PROCEDURE FIN_Wait1(<*UNUSED*>tcb: TcpCtrlBlock.T;<*UNUSED*>  expectedState : TcpSM.States) =

  BEGIN
    IF TcpDebug.debugLevel > 0 THEN 
      IO.Put("User tried to change state when in FIN_Wait1 state \n");
    END;
  END FIN_Wait1;


PROCEDURE FIN_Wait2(<*UNUSED*>tcb: TcpCtrlBlock.T; <*UNUSED*>expectedState : TcpSM.States) =
  BEGIN

    IF TcpDebug.debugLevel > 0 THEN
      IO.Put("User tried to change state when in FIN_Wait2 state \n");
    END;

  END FIN_Wait2;

  
PROCEDURE Close_Wait(tcb: TcpCtrlBlock.T; expectedState : TcpSM.States) =
  BEGIN
    IF (expectedState = TcpSM.States.Last_Ack) THEN (*Passive close*)

      tcb.t_state := TcpSM.States.Last_Ack;

      TRY 
        Thread.Release(tcb.inOutMutex);
        M3Tcp.Output(tcb,NIL);
      FINALLY
        Thread.Acquire(tcb.inOutMutex);
      END;

    ELSE
      IF TcpDebug.debugLevel > 0 THEN 
        IO.Put("User tried to change state when in Close_Wait state \n");
      END;

    END;
  
  END Close_Wait;


PROCEDURE Closing(<*UNUSED*>tcb: TcpCtrlBlock.T; <*UNUSED*>expectedState : TcpSM.States) =
  BEGIN

    IF TcpDebug.debugLevel > 0 THEN 
      IO.Put("User tried to change state when in Closing state \n");
    END;

  END Closing;


PROCEDURE Last_Ack(<*UNUSED*>tcb: TcpCtrlBlock.T; <*UNUSED*>expectedState : TcpSM.States) =
  BEGIN

    IF TcpDebug.debugLevel > 0 THEN 
      IO.Put("User tried to change state when in Last_Ack state \n");
    END;

  END Last_Ack;


PROCEDURE Time_Wait(tcb: TcpCtrlBlock.T; expectedState : TcpSM.States) =
  BEGIN
    M3Tcp.CleanUp(tcb);

    (* XXX we should really only transition into the Closed state.
       Instead we'll just transition to the state provided as an
       arg. *)
    tcb.t_state := expectedState;
  END Time_Wait;

BEGIN
END LocalSM. 










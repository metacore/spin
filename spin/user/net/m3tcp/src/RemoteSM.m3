(*
 * Copyright 1997, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 11-Feb-97   (negrin) at the University of Washington
 *	fixed bug in estab.  It was checking for a fin when it should
 *	have been checking for a syn.
 *
 * 06-Feb-97  Marc Fiuczynski (mef) at the University of Washington
 *	Where possible the state procs use the tcp flag constants
 *	directly as an arg to Output, rather than assigning them to local
 *	variables.  The compiler may take care of this automatically when
 *	optimizations are turned on, but lets not take any changes.
 *	
 *	The state procs are now exposed as methods in the TcpCtrlBlock.T
 *	object.  The changestate procedure now invokes the tcb's methods
 *	rather than indirecting through a stateproc array.  I hope that
 *	the compiler wont produce braindead code for the case statement.
 *	
 *	Since TCBs are object there was no need to pass them by VAR
 *	(which wouldn't work with objects anyways).  
 *	
 *	Added a special case to the FIN_Wait2 state proc to handle
 *	braindead Windows TCP stacks that send us a RST packet instead of
 *	a FIN packet during the four-way shutdown sequence.  Wish I knew
 *	why Microsoft decided to handle their passiveclose this way.
 *	
 *	The MSL values are now coming from the tcb rather than a
 *	constant.  This allows the application to set the MSL rather than
 *	sticking to a particular value.
 *
 * 16-Jan-97  Richard Negrin (negrin) at the University of Washington
 *	created.
 *
 *)

(* This module is only for responding to incoming packets. Hence 
 * the name RemoteSM.  Every time a packet comes in its flags are sent to 
 * this module along with the tcb, and a possible state change occurs.
 *) 

MODULE RemoteSM EXPORTS RemoteSM, RemoteSMPrivate;
IMPORT IO;
IMPORT M3Tcp;
IMPORT TcpPktFormat;
IMPORT Mbuf;
IMPORT TcpSM;
IMPORT TcpCtrlBlock;

(*SPIN SPECIFIC *)
IMPORT Clock, LocalSM, TcpDebug;

(*Declarations*)
VAR 
  gotoNext : BOOLEAN := FALSE; (* Tells whether to go to the
                                  next state or or just return*) 

CONST
  syn    = TcpPktFormat.Flags{TcpPktFormat.Flag.syn};
  ack    = TcpPktFormat.Flags{TcpPktFormat.Flag.ack};
  synack = syn + ack; 
  fin    = TcpPktFormat.Flags{TcpPktFormat.Flag.fin};
  rst    = TcpPktFormat.Flags{TcpPktFormat.Flag.rst};
  rstack = rst + ack;
<*UNUSED*>
CONST
  finack = fin + ack;

(****************************************************************************************)
(*Public Function Implementations*)
(****************************************************************************************)
PROCEDURE ChangeState(
    tcb: TcpCtrlBlock.T;
    READONLY tcpHdr: TcpPktFormat.NewT)=
  BEGIN

    (*We don't do any locking here because the only time this is called is inside
      the Input (or Handler) function and he locks the inOutMutex.  If the Input function
      ever gets changed to not lock the inOut mutex when this function is called then
      we need to locking in here.*)

      REPEAT
        (* XXX ugh.  I don't like doing it this way.
           We'll see how the compiler deals with this
           and then figure out how to speed it up . *)

        (* invoke the appropriate method based on the current state of
           the tcb *)

        CASE tcb.t_state OF
        | TcpSM.States.Closed     =>
          tcb.remoteClosed(tcpHdr);
        | TcpSM.States.Listen     =>
          tcb.remoteListen(tcpHdr);
        | TcpSM.States.SYN_Sent   =>
          tcb.remoteSYN_Sent(tcpHdr);
        | TcpSM.States.SYN_Rcvd   =>
          tcb.remoteSYN_Rcvd(tcpHdr);
        | TcpSM.States.Estab      =>
          tcb.remoteEstab(tcpHdr);
        | TcpSM.States.FIN_Wait1   =>
          tcb.remoteFIN_Wait1(tcpHdr);
        | TcpSM.States.FIN_Wait2  =>
          tcb.remoteFIN_Wait2(tcpHdr);
        | TcpSM.States.Close_Wait =>
          tcb.remoteClose_Wait(tcpHdr);
        | TcpSM.States.Closing    =>
          tcb.remoteClosing(tcpHdr);
        | TcpSM.States.Last_Ack   =>
          tcb.remoteLast_Ack(tcpHdr);
        | TcpSM.States.Time_Wait  =>
          tcb.remoteTime_Wait(tcpHdr);
        END;
        (* gotoNext should really be set as the return value from calling 
           one of the above state procedures. *)

        gotoNext := FALSE; (*always initialize this to false every time we start*)
      UNTIL gotoNext = FALSE;

  END ChangeState;


(****************************************************************************************
 *Private Function Implementations
 *
 *Note: All the cases where a reset occurs have not yet been handled. 
 ****************************************************************************************)

PROCEDURE Closed(
    tcb: TcpCtrlBlock.T;
    READONLY tcpHdr: TcpPktFormat.NewT) =
  VAR
    sndflags : TcpPktFormat.Flags;

  BEGIN

    (*from RFC 793*)
    IF NOT(TcpPktFormat.Flag.rst IN tcpHdr.flags) THEN
      
      IF (TcpPktFormat.Flag.ack IN tcpHdr.flags) THEN
        tcb.snd_nxt := tcpHdr.ack_seq;
        sndflags := rst;
      ELSE
        tcb.snd_nxt := 0;
        tcb.rcv_nxt := tcpHdr.seq + 1; (*supposed to be data len but we don't know it here*)
        sndflags := rstack;
      END;

      M3Tcp.OutputControl(tcb, sndflags); (*Should check return value of output*)

    ELSE
      IF TcpDebug.debugLevel > 0 THEN 
        IO.Put("Received a reset packet while in Closed state, ignoring it. \n");
      END;
    END;

  END Closed;
  

PROCEDURE Listen(
    tcb: TcpCtrlBlock.T;
    READONLY tcpHdr: TcpPktFormat.NewT) =
  BEGIN

    (*Ignore resets*)
    IF (TcpPktFormat.Flag.rst IN tcpHdr.flags) THEN
      RETURN;

    ELSIF (TcpPktFormat.Flag.ack IN tcpHdr.flags) THEN
      tcb.snd_nxt := tcpHdr.ack_seq;
      M3Tcp.OutputControl(tcb, rst); 
      tcb.t_state := TcpSM.States.Closed;
    
    ELSIF (TcpPktFormat.Flag.syn IN tcpHdr.flags) THEN 
      tcb.irs     := tcpHdr.seq;      
      tcb.rcvdSyn := TRUE;
      M3Tcp.OutputControl(tcb, synack); 
      Clock.SetAlarm(tcb.t_srtt, TcpSM.SynTimeout, tcb);
      tcb.t_state := TcpSM.States.SYN_Rcvd;

    END;

  END Listen;


PROCEDURE SYN_Sent(
    tcb: TcpCtrlBlock.T;
    READONLY tcpHdr: TcpPktFormat.NewT) =
   BEGIN

    IF (TcpPktFormat.Flag.ack IN tcpHdr.flags) AND
       (TcpPktFormat.Flag.syn IN tcpHdr.flags) THEN 
      EVAL Clock.CancelAlarm(TcpSM.SynTimeout,tcb); 
      tcb.irs  := tcpHdr.seq;
      tcb.rcvdSyn := TRUE;
      tcb.snd_una := tcb.snd_una + 1;
      M3Tcp.OutputControl(tcb, ack); 
      tcb.t_state := TcpSM.States.Estab;

      (* If we get an syn without an ack then we get simultaneous open *)
    ELSIF (TcpPktFormat.Flag.syn IN tcpHdr.flags) THEN 
      EVAL Clock.CancelAlarm(TcpSM.SynTimeout, tcb);
      tcb.irs  := tcpHdr.seq;      
      tcb.rcvdSyn := TRUE;
      Clock.SetAlarm(tcb.t_srtt, TcpSM.SynTimeout, tcb);
      M3Tcp.OutputControl(tcb, synack); 
      tcb.t_state := TcpSM.States.SYN_Rcvd;
      
    END;

   END SYN_Sent;


PROCEDURE SYN_Rcvd(
    tcb: TcpCtrlBlock.T;
    READONLY tcpHdr: TcpPktFormat.NewT) =
  BEGIN

    IF (TcpPktFormat.Flag.rst IN tcpHdr.flags) THEN
      tcb.t_state := TcpSM.States.Listen;
    END;

    IF (TcpPktFormat.Flag.ack IN tcpHdr.flags) THEN 
      EVAL Clock.CancelAlarm(TcpSM.SynTimeout, tcb);
      tcb.snd_una := tcb.snd_una + 1;
      tcb.t_state := TcpSM.States.Estab;
    END;

  END SYN_Rcvd;


PROCEDURE Estab(
    tcb: TcpCtrlBlock.T;
    READONLY tcpHdr: TcpPktFormat.NewT) =
  BEGIN

    (*If it is a syn I should send a reset*)
    IF (TcpPktFormat.Flag.syn IN tcpHdr.flags) THEN
      M3Tcp.OutputControl(tcb, rst); (* XXX do we ignore syn or try to shutdown??? *)
    END;

    IF (TcpPktFormat.Flag.rst IN tcpHdr.flags) THEN
      M3Tcp.CleanUp(tcb);
    END;

    IF (TcpPktFormat.Flag.fin IN tcpHdr.flags) THEN 
      M3Tcp.OutputControl(tcb, ack); 
      tcb.t_state := TcpSM.States.Close_Wait;
    END;
    
  END Estab;

PROCEDURE FIN_Wait1(
    tcb: TcpCtrlBlock.T;
    READONLY tcpHdr: TcpPktFormat.NewT) =
  BEGIN
    
    IF (TcpPktFormat.Flag.fin IN tcpHdr.flags) AND
       (TcpPktFormat.Flag.ack IN tcpHdr.flags) AND 
       (tcpHdr.ack_seq = tcb.fin_seq + 1) THEN

      (*XXX (negrin) Is it possible that dataTimeout could be called after this cancel
            alarm? If so that would be bad.  Must make sure that locks 
            are set up correctly.*)

      EVAL Clock.CancelAlarm(M3Tcp.DataTimeout, tcb);
      tcb.t_rxtcur := 0;
      M3Tcp.OutputControl(tcb, ack); 

      (* move to time wait state and turn on MSL timeout *)
      tcb.t_state := TcpSM.States.Time_Wait;  
      Clock.SetAlarm(tcb.msl, MSLTimeout , tcb);


    ELSIF (TcpPktFormat.Flag.ack IN tcpHdr.flags) AND 
          (tcpHdr.ack_seq = tcb.fin_seq + 1) THEN 

      (*XXXShould also set a timer for 10 minutes, in case the other side doesn't
        send a fin*)
      EVAL Clock.CancelAlarm(M3Tcp.DataTimeout, tcb); 
      tcb.t_rxtcur := 0;
      tcb.t_state := TcpSM.States.FIN_Wait2;

    ELSIF (TcpPktFormat.Flag.fin IN tcpHdr.flags) AND (*simultaneous close*)
          (tcpHdr.ack_seq < tcb.fin_seq + 1) THEN 
      M3Tcp.OutputControl(tcb,ack); 
      tcb.t_state := TcpSM.States.Closing;  
    END;

  END FIN_Wait1;


PROCEDURE FIN_Wait2(
    tcb: TcpCtrlBlock.T; 
    READONLY tcpHdr: TcpPktFormat.NewT) =
  BEGIN

    IF (TcpPktFormat.Flag.fin IN tcpHdr.flags) THEN
      M3Tcp.OutputControl(tcb, ack); 
      (* move to time wait state and turn on MSL timeout *)
      tcb.t_state := TcpSM.States.Time_Wait;
      Clock.SetAlarm(tcb.msl, MSLTimeout , tcb);
    ELSIF      
      (* Windows boxes send us a RST instead of a FIN *)
      TcpPktFormat.Flag.rst IN tcpHdr.flags THEN
      (* move to time wait state and turn on MSL timeout *)
      tcb.t_state := TcpSM.States.Time_Wait;
      Clock.SetAlarm(tcb.msl, MSLTimeout , tcb);
    END;
  END FIN_Wait2;


PROCEDURE Close_Wait(
    <*UNUSED*>tcb: TcpCtrlBlock.T; 
    <*UNUSED*>READONLY tcpHdr: TcpPktFormat.NewT) =
  BEGIN
    (*Raise some error*)
    (*XXXmaybe we should send an ack in case our fin got lost*)
    IF TcpDebug.debugLevel > 0 THEN IO.Put("Received a packet while in Close Wait state\n"); END;
  END Close_Wait;


PROCEDURE Closing(
    tcb: TcpCtrlBlock.T;
    READONLY tcpHdr: TcpPktFormat.NewT) =
  BEGIN

    IF (TcpPktFormat.Flag.ack IN tcpHdr.flags) AND
       (tcpHdr.ack_seq = tcb.fin_seq + 1) THEN 

      EVAL Clock.CancelAlarm(M3Tcp.DataTimeout, tcb); 
      tcb.t_rxtcur := 0;

      (* move to time wait state and turn on MSL timeout *)
      tcb.t_state := TcpSM.States.Time_Wait;
      Clock.SetAlarm(tcb.msl, MSLTimeout , tcb);
    END;

  END Closing;


PROCEDURE Last_Ack(
    tcb: TcpCtrlBlock.T; 
    READONLY tcpHdr: TcpPktFormat.NewT) =
  BEGIN

    IF (TcpPktFormat.Flag.ack IN tcpHdr.flags) THEN 
      EVAL Clock.CancelAlarm(M3Tcp.DataTimeout, tcb); 
      tcb.t_rxtcur := 0;
      tcb.t_state := TcpSM.States.Closed;
    END;

  END Last_Ack;


PROCEDURE Time_Wait(
    <*UNUSED*>tcb: TcpCtrlBlock.T;
    <*UNUSED*>READONLY tcpHdr: TcpPktFormat.NewT) =
  BEGIN
    (*should probably send a reset here or something*)
    IF TcpDebug.debugLevel > 0 THEN
      IO.Put("Received a packet in TimeWait State, ignored \n");
    END;
  END Time_Wait;


PROCEDURE MSLTimeout (arg:REFANY) =
  VAR
   tcb :  TcpCtrlBlock.T;
  BEGIN
    tcb := NARROW(arg, TcpCtrlBlock.T);
    LocalSM.ChangeState(tcb, TcpSM.States.Closed);

(* XXX This isn't necessary and it is annoying.

    IF TcpDebug.debugLevel > 0 THEN 
      IO.Put("MSL Timeout - ");
      IO.Put(tcb.name);
      IO.Put("\n");
    END;
*)
  END MSLTimeout;


(* XXX temporary input data processing function *)
PROCEDURE ProcessPacket(
    <*UNUSED*>tcb: TcpCtrlBlock.T;
    <*UNUSED*>m: Mbuf.T;
    <*UNUSED*>offset: CARDINAL) = 
  BEGIN
    (* do nothing *)
  END ProcessPacket;


BEGIN
END RemoteSM.

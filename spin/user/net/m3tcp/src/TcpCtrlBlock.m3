(*
 * Copyright 1997, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 02-Mar-97   (negrin) at the University of Washington
 *	added CalcRTT and ProcessAck as functions on a TcpCtrlBlock.
 *	ProcessAck contians code for responding to duplicate
 *	Acks. (although that shouldn't be the default code).
 *	 
 *
 * 14-Feb-97   (negrin) at the University of Washington
 *	moved GetWindow to be a funciton on a TcpCtrlBlock.
 *
 * 06-Feb-97  Marc Fiuczynski (mef) at the University of Washington
 *	Made the TcpCtrlBlock.T an opaque object.  Overrideing the
 *	statemachine methods with the procedures defined in the
 *	Local/Remote statemachine modules.  
 *	
 *	Added a msl (max seg life) field that can be changed by the
 *	application.
 *	
 *	Added a processPacket method that allows the application to
 *	receive incoming packets.
 *
 * 16-Jan-97  Richard Negrin (negrin) at the University of Washington
 *	created.
 *
 *)

MODULE TcpCtrlBlock;
IMPORT TcpSM, Ifq, Clock, Fmt, IO, TcpDebug, TcpPktFormat, M3Tcp;
IMPORT RemoteSM, RemoteSMPrivate, LocalSM, LocalSMPrivate;

VAR 
  Iss := 49;

CONST 
  MSL = 60 * 1024;
  DefaultWinSize  = 4096; (* 4KB window size *)
  MaxSegSize      = 512;
  InitFrequency   = 500;  (*msecs*)
  DefaultRxtValue = 3000;

REVEAL T = TPublic BRANDED OBJECT
  (* stick field that we want to hide from people here *)
OVERRIDES
  init              := Init;
  getWindow         := GetWindow;
  calcRtt           := CalcRtt;
  processAck        := ProcessAck;
  timeout           := Timeout;

  (* XXX temporary *)
  processPacket     := RemoteSMPrivate.ProcessPacket;

  (* remote state machine functions *)
  changeRemoteState := RemoteSM.ChangeState;
  remoteClosed      := RemoteSMPrivate.Closed;
  remoteListen      := RemoteSMPrivate.Listen;
  remoteSYN_Sent    := RemoteSMPrivate.SYN_Sent;
  remoteSYN_Rcvd    := RemoteSMPrivate.SYN_Rcvd;
  remoteEstab       := RemoteSMPrivate.Estab;
  remoteFIN_Wait1   := RemoteSMPrivate.FIN_Wait1;
  remoteFIN_Wait2   := RemoteSMPrivate.FIN_Wait2;
  remoteClose_Wait  := RemoteSMPrivate.Close_Wait;
  remoteClosing     := RemoteSMPrivate.Closing;
  remoteLast_Ack    := RemoteSMPrivate.Last_Ack;
  remoteTime_Wait   := RemoteSMPrivate.Time_Wait;

  (* local state machine functions *)
  changeLocalState  := LocalSM.ChangeState;
  localClosed       := LocalSMPrivate.Closed;
  localListen       := LocalSMPrivate.Listen;
  localSYN_Sent     := LocalSMPrivate.SYN_Sent;
  localSYN_Rcvd     := LocalSMPrivate.SYN_Rcvd;
  localEstab        := LocalSMPrivate.Estab;
  localFIN_Wait1    := LocalSMPrivate.FIN_Wait1;
  localFIN_Wait2    := LocalSMPrivate.FIN_Wait2;
  localClose_Wait   := LocalSMPrivate.Close_Wait;
  localClosing      := LocalSMPrivate.Closing;
  localLast_Ack     := LocalSMPrivate.Last_Ack;
  localTime_Wait    := LocalSMPrivate.Time_Wait;
END;

PROCEDURE GetWindow(self : T): CARDINAL = 
  BEGIN

    (*XXX For slow start and congestion control should be.
      MIN(tcb.snd_wnd, tcb.snd_cwnd);*)

    RETURN self.snd_wnd;

  END GetWindow;  

PROCEDURE CalcRtt(self : T; numTicks : INTEGER ) =

  BEGIN

    IF TcpDebug.debugLevel > 0 THEN
      IO.Put("numTicks for seq " & Fmt.Int(self.t_timed_seq)  & " is: " & Fmt.Int(numTicks) & "\n")
    END;

  END CalcRtt;



(* Process Ack*)
(*Return a boolean specifying whether Output should be called at the end of Input*)
(*Should not do duplicate ack checking by default. Should do nothing by default.*)

PROCEDURE ProcessAck(tcb : T; tcpHdr : TcpPktFormat.NewT) : BOOLEAN = 

  VAR
    ret_val : BOOLEAN := FALSE;

  BEGIN

(*XXX Slow Start Code.  
  this does not belong in base Tcp. It
  belongs in an extension. I am just putting it here for now.*)



(*XXX Duplicate Ack Code, this does not belong in base Tcp. It
   belongs in an extension. I am just putting it here for now.*)

    (*If we start getting duplicate acks then rexmt*)

    IF (tcpHdr.ack_seq = tcb.last_ack) AND
      NOT (TcpPktFormat.Flag.syn IN tcpHdr.flags) THEN
      
      INC(tcb.t_dupacks);
      
      (*Make this comparison equals and not greather than, that
        way if we get a whole bunch of duplicate acks we won't resend the
               packet every time. *)

      IF (tcb.t_dupacks = tcb.tcprexmtthresh) THEN

        IF TcpDebug.debugLevel > 0 THEN 
          IO.Put("Got three duplicate acks so retransmitting\n");
        END;

        M3Tcp.CauseResend(tcb);
        ret_val :=  TRUE;
      END;
      
    ELSE
      
      tcb.last_ack := tcpHdr.ack_seq;
      tcb.t_dupacks := 0;
      ret_val := FALSE;

    END;
    
    RETURN ret_val;

  END ProcessAck;

PROCEDURE Timeout(  tcb : T) = 

  BEGIN

    tcb.snd_cwnd := tcb.t_maxseg;

  END Timeout; 

PROCEDURE Init(self : T; name : TEXT): T = 
  BEGIN
    (* XXX should we take this back out? Take this out once
       wrap code has been put in places

       This is only in here for debugging purposes, but
       it doesn't affect anything.  
     *)
    Iss := Iss + 50;

    (* all tcb's start at this state *)
    self.t_state := TcpSM.States.Closed;

    self.inOutMutex  := NEW(MUTEX);

    self.sendQ := NEW (REF Ifq.ifqueue);
    Ifq.Init(self.sendQ^);

    self.iss      := Iss;  (*This should be a function that calculates an iss*)
    self.snd_una  := self.iss;
    self.snd_nxt  := self.iss;
    self.snd_up   := self.iss;
    self.snd_max  := self.iss;
    self.fin_seq  := self.iss; (*initialize this to iss so we know a fin hasn't been sent*)
    self.rcv_wnd  := DefaultWinSize;    
    self.snd_wnd  := 0;

    self.sent_fin := FALSE;

    self.t_timed_seq  := self.iss;
    self.timing_rtt   := FALSE;

    self.t_maxseg       := MaxSegSize;
    self.max_syns       := 12;
    self.num_syns       := 0;
    self.last_ack       := 0;
    self.tcprexmtthresh := 3;
    self.maxNumSends    := 10;



    self.queueingStates := TcpSM.StateSet{TcpSM.States.SYN_Sent, 
                                          TcpSM.States.Estab, 
                                          TcpSM.States.Close_Wait};

    self.sendingStates  := TcpSM.StateSet{TcpSM.States.Estab,
                                          TcpSM.States.FIN_Wait1,
                                          TcpSM.States.Closing,
                                          TcpSM.States.Close_Wait,
                                          TcpSM.States.Last_Ack};

    self.recveingStates := TcpSM.StateSet{TcpSM.States.Estab,
                                          TcpSM.States.FIN_Wait1};

    self.finSendStates  := TcpSM.StateSet{TcpSM.States.FIN_Wait1,
                                          TcpSM.States.Last_Ack};

    (* These get initialized when we get a SYN *)
    self.rcv_nxt := 0;
    self.rcv_adv := 0;
    self.irs     := 0;
    self.rcv_up  := 0;

    self.snd_wl1 := 0;
    self.snd_wl2 := 0;

    self.snd_cwnd     := self.t_maxseg; (* this needs to be reset if we get a new
                                           max seg size with our syn.*)
    self.snd_ssthresh := 1; 

    self.name     := name;
    self.t_rxtcur := 0;                (* holds current retransmit value    *)
    self.t_srtt   := DefaultRxtValue;  (* calculated smooth round trip time *)
    self.t_rtseq  := self.iss;         (* seq num of time segment           *)

    self.last_rxmt_seq := self.iss;
    self.num_rxmts     := 0;
    self.max_rxmts     := 12;

    (* I made these up so I need to come up with real ones. *)
    self.t_rttmin := 1000;  (* min retransmit time *)
    self.t_rttmax := 64000; (* max retransmit time *)

    self.msl := 2* MSL;
    RETURN self;
  END Init;

PROCEDURE InitialSeqTimer (<*UNUSED*>arg:REFANY) =
  BEGIN
    Iss := Iss + 64000;
    Clock.SetAlarm(InitFrequency,InitialSeqTimer,NIL); 
  END InitialSeqTimer;

(*Uncomment this later. For now I don't want to deal with wrap code
  and I want the numbers to stay small.*)
BEGIN
(*  Clock.SetAlarm(InitFrequency,InitialSeqTimer,NIL); *)
END TcpCtrlBlock. 

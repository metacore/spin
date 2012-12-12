(*
 * Copyright 1997, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 06-Feb-97  Marc Fiuczynski (mef) at the University of Washington
 *	Added debugging output.
 *
 * 16-Jan-97  Richard Negrin (negrin) at the University of Washington
 *	created.
 *
 *)


MODULE TcpSM;
IMPORT Clock, TcpPktFormat, IO, TcpCtrlBlock, M3Tcp, TcpDebug;

CONST StateText = ARRAY States OF TEXT 
  { "Closed","Listen","SYN_Sent", "SYN_Rcvd", "Estab", "FIN_Wait1",
    "FIN_Wait2", "Close_Wait", "Closing", "Last_Ack", "Time_Wait" };

PROCEDURE StateToText(s : States) : TEXT = 
  BEGIN
    RETURN StateText[s];
  END StateToText; 

PROCEDURE SynTimeout(arg:REFANY) =

  VAR  
    flag : TcpPktFormat.Flags;
       tcb :  TcpCtrlBlock.T;
  BEGIN

    
    tcb := NARROW(arg, TcpCtrlBlock.T);

    IF (tcb.t_state = States.SYN_Rcvd) THEN

      flag := TcpPktFormat.Flags{TcpPktFormat.Flag.syn} + 
              TcpPktFormat.Flags{TcpPktFormat.Flag.ack};

    ELSIF (tcb.t_state = States.SYN_Sent) THEN    
    
      flag := TcpPktFormat.Flags{TcpPktFormat.Flag.syn}

    ELSE
       IF TcpDebug.debugLevel > 0 THEN 
         IO.Put(tcb.name);
         IO.Put(" shouldn't be sending a syn when in ");
         IO.Put(StateToText(tcb.t_state));
         IO.Put("state \n"); END;
      RETURN;
    END;

    IF ( tcb.num_syns < tcb.max_syns ) THEN

      tcb.snd_nxt := tcb.snd_nxt -1; (*Reset Fin sequence number*)
      M3Tcp.OutputControl(tcb ,flag); 
      Clock.SetAlarm(tcb.t_srtt, SynTimeout, tcb);
      tcb.num_syns := tcb.num_syns + 1;

    ELSE
      IF TcpDebug.debugLevel > 0 THEN IO.Put("Giving up, Syn not being acked\n"); END;
      M3Tcp.CleanUp(tcb);

    END;
    IF TcpDebug.debugLevel > 0 THEN IO.Put("Syn timeout happened.\n"); END;

  END SynTimeout;


 
BEGIN
END TcpSM.

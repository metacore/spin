(*
 * Copyright 1997, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 03-Mar-97   (negrin) at the University of Washington
 *	added a CauseResend function that one calls to resend data.
 *	changed maxNumSends to be a member of the TcpCtrlBlock.
 *
 * 28-Feb-97   (negrin) at the University of Washington
 *	added three variables, queueingStates, sendingStates,
 *	receivingStates. These are sets of state machine
 *	states. rather than doing 3 to 5 checks against
 *	the state variable, just see if it is in a set.
 *	 
 *
 * 23-Feb-97  Richard Negrin (negrin) at the University of Washington
 *	added code to do round trip time calculation and timeout calculation.
 *
 * 13-Feb-97   (negrin) at the University of Washington
 *	
 *	Changed timing of fin packet to use DataTimeout instead of
 *	FinTimeout. 
 *
 * 12-Feb-97   (negrin) at the University of Washington
 *	Changed code so fin is sent once queue is emptied
 *	only if user has called close. Before it would
 *	just send the fin as soon as the user called
 *	close.
 *
 * 11-Feb-97   (negrin) at the University of Washington
 *	added code to cleanup function to free mbufs
 *	on the queue.
 *
 * 11-Feb-97   (negrin) at the University of Washington
 *	fixed bug in activeclose.  Don't need to check to see
 *	what state we are in.
 *
 * 06-Feb-97  Marc Fiuczynski (mef) at the University of Washington
 *	Added support to export interfaces via a spin domain called
 *	M3Tcp.
 *	
 *	Integrated the Input procedure into the Handler procedure.  This
 *	way we can look at the tcp/ip header in the context of that
 *	procedure and not recompute it twice.  The Input procedure has
 *	been reworked somewhat for performance, but there is still lots
 *	of work to do.  There are tons of comments in there with "XXX"
 *	that we need to look at and clean up. Finally, we may have to
 *	expose the Input procedure again sometime in the future (after
 *	SOSP).
 *	
 *	Added support for multiple tcbs via an "incarnation" table.  As a
 *	result needed to rework all of the shell commands in terms of
 *	this table.
 *	
 *	We never flip the tcp sport,dport frmo network to host format.
 *	
 *	XXX Take this comment back out in the future
 *	The PrintTcpHdr function may have become a little ugly.  Need to
 *	clean it up sometime soon, but most likely not until after SOSP.
 *	
 *	Now computing the total tcp bytes and data length by looking at
 *	the ip header totlen value and subtracting out the tcp/ip header.
 *
 * 22-Jan-97  Richard Negrin (negrin) at the University of Washington
 *	changed buffer scheme to use shallow copy
 *
 * 16-Jan-97  Richard Negrin (negrin) at the University of Washington
 *	created.
 *
 *)

MODULE M3Tcp;

(* Standard M3 *)
    IMPORT IO, Fmt, Ctypes;

(* SPIN SPECIFIC*)
    IMPORT NameServer, ParseParams, Dispatcher, Clock;

(* PLEXUS SPECIFIC *)
    IMPORT NetDb, Tcp, TcpPktFormat, IpPktFormat, IpGen;

(* URT SPECIFIC *)
    IMPORT Net, Mbuf;

(* Local *)
    IMPORT Ifq, TcpDebug, Incarnation, TcpCtrlBlock, TcpSM, RemoteSM,
           LocalSM;

(* GENERICS *)
    IMPORT M3TcpCmd, M3TcpInterface, IncarnationTbl;

CONST flagnames = ARRAY [TcpPktFormat.Flag.fin..TcpPktFormat.Flag.urg] OF TEXT 
  { "F","S","R","P","A","U" };
      HostOrder    = 0;
      NetworkOrder = 1;

      tcp_hdr_len = BYTESIZE(TcpPktFormat.Header);
      protocol    = IpPktFormat.IPPROTO_TCP;

CONST
  syn    = TcpPktFormat.Flags{TcpPktFormat.Flag.syn};
  ack    = TcpPktFormat.Flags{TcpPktFormat.Flag.ack};
  fin    = TcpPktFormat.Flags{TcpPktFormat.Flag.fin};
  push   = TcpPktFormat.Flags{TcpPktFormat.Flag.push};
  finack = fin + ack;

<*UNUSED*>
CONST
  synack = syn + ack; 
  rst    = TcpPktFormat.Flags{TcpPktFormat.Flag.rst};

VAR
  m3tcp                    : Dispatcher.Binding;
  lossRate                 : INTEGER  := 0;
  TotalNumPacketsSent      : CARDINAL := 0;
  TotalNumBytesSent        : CARDINAL := 0;
  TotalNumPacketsRecv      : CARDINAL := 0;
  TotalNumBytesRecv        : CARDINAL := 0;
  TotalNumPacketsAttempted : CARDINAL := 0;
  TotalNumTimeOuts         : CARDINAL := 0;
  TotalNumDataBytesSent    : CARDINAL := 0;

PROCEDURE CauseResend(tcb :  TcpCtrlBlock.T) = 

  BEGIN

    EVAL Clock.CancelAlarm(DataTimeout,tcb);
    tcb.snd_nxt := tcb.snd_una;
    tcb.t_rxtcur  := 0;           (*reset the rxtmt timer*)
    tcb.timing_rtt    := FALSE; (*turn off round trip timing*)


  END CauseResend;
  

PROCEDURE DataTimeout(arg:REFANY) =

  VAR
    tcb :  TcpCtrlBlock.T;       

  BEGIN

    tcb := NARROW(arg, TcpCtrlBlock.T);

    LOCK tcb.inOutMutex DO    

      IF tcb.last_rxmt_seq = tcb.t_rtseq THEN
        INC(tcb.num_rxmts);
      ELSE
        tcb.num_rxmts := 0;
        tcb.last_rxmt_seq := tcb.t_rtseq;
      END;

      IF (tcb.num_rxmts = tcb.max_rxmts) THEN
        CleanUp(tcb);
        RETURN;
      END;

      CauseResend(tcb);

      IF TcpDebug.debugLevel > 0 THEN IO.Put("\nDATA TIMER WENT OFF\n"); END;
      INC(TotalNumTimeOuts); 

      tcb.timeout();  (*code the user wants to stick in*)

    END;

    Output(tcb,NIL,push);
  END DataTimeout;


PROCEDURE SendItem(tcb     : TcpCtrlBlock.T; 
  data    : Mbuf.T; 
  tcp     : TcpPktFormat.NewT)=
  VAR
    packet : Mbuf.T;

  BEGIN

    (*Allocate space for a header*)
    IF data # NIL THEN
      packet := data; 
      packet := Mbuf.m_prepend(packet,tcp_hdr_len,Mbuf.M_WAIT);
    ELSE
      packet := Mbuf.m_get(Mbuf.M_WAIT, Mbuf.MT_DATA);
      packet.mh_hdr.mh_len := tcp_hdr_len;
      Mbuf.M_ALIGN(packet,tcp_hdr_len);      
    END;

    WITH buf = Mbuf.Array(packet)^,
         tcpHdr = VIEW(buf,TcpPktFormat.NewT)
     DO

      tcpHdr.flags   := tcp.flags;
      tcpHdr.seq     := Net.htonl(tcp.seq);
      tcpHdr.ack_seq := Net.htonl(tcp.ack_seq);
      tcpHdr.window  := Net.htons(tcp.window);
      tcpHdr.x2      := tcp.x2;
      tcpHdr.xoff    := tcp.xoff;
      tcpHdr.urg_ptr := Net.htons(tcp.urg_ptr);

      (* tcb lport/rport already in net order *)
      tcpHdr.sport := tcb.incarnation.lport; 
      tcpHdr.dport := tcb.incarnation.rport;
      tcpHdr.check := 0; (*Done in PacketSend*)

    END;
    
    PacketSend(tcb.incarnation.raddr, tcb.incarnation.laddr, packet);
  END SendItem; 

PROCEDURE Output(tcb       : TcpCtrlBlock.T; 
                 dataBuf   : Mbuf.T := NIL;
                 userflags : TcpPktFormat.Flags := TcpPktFormat.Flags{}) =
  VAR 
    tcpHdr       : TcpPktFormat.NewT; 
    dataToSend   : Mbuf.T;
    sendQHead    : Mbuf.T;
    len          : CARDINAL := 0;
    qlen         : CARDINAL := 0;
    win          : CARDINAL := 0;
    unAckedBytes : CARDINAL := 0;
    flags        : TcpPktFormat.Flags;
    numSends     : CARDINAL := 0;

  BEGIN

    LOCK tcb.inOutMutex DO    
      
      IF TcpDebug.debugLevel > 0 THEN IO.Put("\nOutput was called\n"); END;

      (*Put this on the queue of things to send.
        Conditions:
        1. There is data.
        2. We are in state that allows data to be queued.*)
      IF  (dataBuf # NIL AND 
           (tcb.t_state IN tcb.queueingStates)) THEN
        Ifq.Enqueue(tcb.sendQ^, dataBuf); 
      END;
    END; (*LOCK*)

    (* Conditions for staying in the loop. (that is why they are anded and I have a NOT)
      Make sure there is something on the queue.
      make sure we haven't filled the window.
      Make sure we have data to send. 
      Make sure we are in data-sending states (different than data enqueuing states).
      Make sure we aren't stuck in a tight loop sending a million packets without
          receiving an ack.

      Default data sending states are: Estab, FIN_Wait1, Closing, Close_Wait, Last_Ack  
    *)
    LOOP 
      LOCK tcb.inOutMutex DO  

      win := tcb.getWindow();                    (*sending window*) 
      qlen := Ifq.NumBytes(tcb.sendQ^);           (*total bytes in queue*)
      unAckedBytes := tcb.snd_nxt - tcb.snd_una; (*bytes sent but not acknowledged*)

      IF NOT (Ifq.NotEmpty(tcb.sendQ^)           AND 
             (unAckedBytes < win)                AND
             (unAckedBytes < qlen)                AND
             (tcb.t_state  IN tcb.sendingStates) AND
             (numSends < tcb.maxNumSends)) THEN
        EXIT;
      END;

      IF TcpDebug.debugLevel > 0 THEN
        IO.Put("total num bytes  : " & Fmt.Int(tcb.sendQ^.ifq_bytes)  & "\n"); 
        IO.Put("Unacked Bytes    : " & Fmt.Int( unAckedBytes) & "\n");
      END;

      len := MIN (win, qlen) - unAckedBytes;
      len := MIN (tcb.t_maxseg, len);       

      IF TcpDebug.debugLevel > 0 THEN
        IO.Put("len equals : " & Fmt.Int(len) & "\n");
      END;

      (*sanity check*)
      IF (tcb.sendQ^.ifq_bytes # Mbuf.m_length(tcb.sendQ^.ifq_head)) THEN
        IF TcpDebug.debugLevel > 0 THEN IO.Put("Failed Sanity check bailing out\n");END;
        RETURN;
      END;
        
      sendQHead  := Ifq.GetHead(tcb.sendQ^);
      dataToSend := Mbuf.m_copym( sendQHead, 
                                  unAckedBytes + tcb.sendQ^.ifq_sendUnaOffset, 
                                  len, Mbuf.M_WAIT);

      IF (tcb.t_rxtcur = 0) THEN  (*no timer set*)
        IF TcpDebug.debugLevel > 0 THEN 
          IO.Put("Set alarm for seq " & Fmt.Int(tcb.snd_nxt) & "\n"); 
        END;
        tcb.t_rxtcur  := tcb.t_srtt;
        tcb.t_rtseq := tcb.snd_nxt;
        Clock.SetAlarm(tcb.t_rxtcur,DataTimeout, tcb);
      END;

      (* use the user provided flags, regardless what they may be 
         and ensure that the ack bit is set.
       *)
      flags := userflags + ack;
      
      tcpHdr.flags   := flags;
      tcpHdr.seq     := tcb.snd_nxt; 
      tcpHdr.ack_seq := tcb.rcv_nxt; (*this is garbage for a SYN*)
      tcpHdr.window  := tcb.rcv_adv - tcb.rcv_nxt; 
      tcpHdr.x2      := 0;
      tcpHdr.xoff    := tcp_hdr_len DIV 4;  (* header length in 32 bit words *) 
      tcpHdr.urg_ptr := 0;

      (*Increment send next variable *)
      tcb.snd_nxt := tcb.snd_nxt + len; (*XXX wrap add code*)

      (*XXXWrap Code*)
      (*Time the packet to see what our round trip time is*)
      IF (tcb.snd_max < tcb.snd_nxt) THEN (*not a retransmit*)
        tcb.snd_max := tcb.snd_nxt;

        IF NOT (tcb.timing_rtt) THEN
          tcb.t_timed_seq := tcpHdr.seq; 
          tcb.t_clockVal := Clock.ReadTicks();
          tcb.timing_rtt := TRUE;

          IF TcpDebug.debugLevel > 0 THEN
            IO.Put("Started timing seq " & Fmt.Int(tcb.t_timed_seq) & "\n");
          END;
        END;

        INC(TotalNumDataBytesSent, len);
      END;



      IF TcpDebug.debugLevel > 0 THEN
        IO.Put(tcb.name & " sending a data packet: \n");
        IO.Put("Sending ");
        PrintPkt(dataToSend,0);    
        (* XXX these two lines are done in Send Item *)
        tcpHdr.sport := tcb.incarnation.lport;
        tcpHdr.dport := tcb.incarnation.rport; 
        PrintTcpHdr(tcpHdr,HostOrder);
      END;
      END; (*LOCK*)

      SendItem(tcb, dataToSend, tcpHdr);
      IF TcpDebug.debugLevel > 0 THEN IO.Put("\n"); END;
      
      INC(numSends);

    END;  (*LOOP*)

    LOCK tcb.inOutMutex DO  

      (* Conditions for sending fin.: 
         Make sure user has called close.
         Make sure we have sent all the data in the queue and not sent a fin.
            If we had sent a fin then snd_nxt would be one bigger than snd_una. 
      *)
      IF   ((tcb.t_state = TcpSM.States.FIN_Wait1 OR 
             tcb.t_state = TcpSM.States.Last_Ack)  AND 
            tcb.snd_nxt - tcb.snd_una = Ifq.NumBytes(tcb.sendQ^)) THEN

        IF TcpDebug.debugLevel > 0 THEN 
          IO.Put("Fin seq is: " & Fmt.Int(tcb.snd_nxt) & "\n");  
        END;

        (*set timer*)
        IF (tcb.t_rxtcur = 0) THEN  (*no timer set*)
          IF TcpDebug.debugLevel > 0 THEN 
            IO.Put("Set alarm for fin seq " & Fmt.Int(tcb.snd_nxt) & "\n"); 
          END;
          tcb.t_rxtcur := tcb.t_srtt;
          tcb.t_rtseq  := tcb.snd_nxt;
          Clock.SetAlarm(tcb.t_rxtcur, DataTimeout, tcb);
        END;

        tcb.sent_fin := TRUE;
        tcb.fin_seq := tcb.snd_nxt; 
        OutputControl(tcb, finack); 

      END; (*IF*)
    END; (*LOCK*)

  END Output;

PROCEDURE OutputControl(tcb   : TcpCtrlBlock.T; 
                        flags : TcpPktFormat.Flags := TcpPktFormat.Flags{})=
  VAR 
    tcpHdr : TcpPktFormat.NewT; 
  BEGIN
    tcpHdr.flags   := flags;
    tcpHdr.seq     := tcb.snd_nxt; 
    tcpHdr.ack_seq := tcb.rcv_nxt; (*this is garbage for a SYN*)
    tcpHdr.window  := tcb.rcv_adv - tcb.rcv_nxt; 

    tcpHdr.x2      := 0;
    tcpHdr.xoff    := tcp_hdr_len DIV 4;  (* header length in 32 bit words *) 
    tcpHdr.urg_ptr := 0;
    
    (* If this is a syn, a fin, then we just add one to sequence 
       number, if it is a pure ack we don't add one.*)
       
    IF (TcpPktFormat.Flag.syn IN tcpHdr.flags) OR
       (TcpPktFormat.Flag.fin IN tcpHdr.flags) THEN
      tcb.snd_nxt := tcb.snd_nxt + 1;
    END;
    
    (*XXXWRAP Code*)
    IF (tcb.snd_max < tcb.snd_nxt) THEN (*not a retransmit*)
      tcb.snd_max := tcb.snd_nxt;
    END;

    IF TcpDebug.debugLevel > 0 THEN 
      IO.Put("\n" & tcb.name & " sending a ctrl packet: \n"); 
    END;

    (* XXX these two lines are done in Send Item *)
    tcpHdr.sport := tcb.incarnation.lport;
    tcpHdr.dport := tcb.incarnation.rport; 

    PrintTcpHdr(tcpHdr,HostOrder);
    SendItem(tcb, NIL, tcpHdr);
    
    IF TcpDebug.debugLevel > 0 THEN
      IO.Put("snd_nxt now equals: ");
      IO.Put(Fmt.Int(tcb.snd_nxt));
      IO.Put(" for " & tcb.name  & "\n"); 
    END;

  END OutputControl;



PROCEDURE CleanUp(tcb : TcpCtrlBlock.T) = 

  VAR  
    temp : Mbuf.T;

  BEGIN

  (*clean up queues.*)
    WHILE (Ifq.NotEmpty(tcb.sendQ^)) DO

        temp := Ifq.Dequeue(tcb.sendQ^); 
        Mbuf.m_freem(temp);  
      END;

  (*cancel all timers.*)
    EVAL Clock.CancelAlarm(TcpSM.SynTimeout, tcb); 
    EVAL Clock.CancelAlarm(DataTimeout, tcb);

    (*Should do a state transistion and not set state
      explicitly*)
    tcb.t_state := TcpSM.States.Closed;

  END CleanUp;

(*XXXShould use some wrap code here*)
PROCEDURE Between(LeftSide : TcpCtrlBlock.tcp_seq;
                  LeftSideInclusive : BOOLEAN;
                  RightSide : TcpCtrlBlock.tcp_seq;
                  RightSideInclusive : BOOLEAN;
                  Number : TcpCtrlBlock.tcp_seq) : BOOLEAN =

  BEGIN

    IF (LeftSideInclusive) THEN 

      IF (RightSideInclusive) THEN

        RETURN LeftSide <= Number AND Number <= RightSide;
        

      ELSE

        RETURN LeftSide <= Number AND Number  < RightSide;

      END;
    
    ELSE

      IF (RightSideInclusive) THEN

        RETURN LeftSide < Number AND Number <= RightSide ;

      ELSE

        RETURN LeftSide < Number AND Number <  RightSide;

      END;

    END;

  END Between; 

(*I can safely assume sequence numbers are always 32 bits.*)
(*That is why I hardcode the value 16_ffffffff in here *)
(*Note 2147483647 is 16_ffffffff divided by two. That is my delta
  with which I check for wrap.*)

PROCEDURE Less (LeftSide : TcpCtrlBlock.tcp_seq; 
                RightSide : TcpCtrlBlock.tcp_seq):BOOLEAN = 

  BEGIN

    IF RightSide - LeftSide > 0 THEN

      RETURN 16_ffffffff - RightSide + LeftSide > 2147483647;

    ELSE
      
      RETURN 16_ffffffff - LeftSide + RightSide < 2147483647;
  
    END;

  END Less;

PROCEDURE LessOrE (LeftSide : TcpCtrlBlock.tcp_seq; 
                    RightSide : TcpCtrlBlock.tcp_seq):BOOLEAN = 

  BEGIN

    IF RightSide - LeftSide >= 0 THEN

      RETURN 16_ffffffff - RightSide + LeftSide > 2147483647;

    ELSE
      
      RETURN 16_ffffffff - LeftSide +  RightSide < 2147483647;
  
    END;

  END LessOrE;

(*I am being lazy here, a > b --> b <= a*)
<*UNUSED*>
PROCEDURE Greater (LeftSide : TcpCtrlBlock.tcp_seq; 
                      RightSide : TcpCtrlBlock.tcp_seq):BOOLEAN = 

  BEGIN

    RETURN Less(RightSide, LeftSide);

  END Greater;

<*UNUSED*>
PROCEDURE GreaterOrE (LeftSide : TcpCtrlBlock.tcp_seq; 
                      RightSide : TcpCtrlBlock.tcp_seq):BOOLEAN = 

  BEGIN
    
    RETURN LessOrE(RightSide, LeftSide);
    
  END GreaterOrE;

(* event handles for tcp packets *)

PROCEDURE Handler (packet, curr: Mbuf.T; offset: CARDINAL):BOOLEAN =
  VAR
    tcb : TcpCtrlBlock.T;

  BEGIN
    (* get the ip address to send the packet back to *)
    WITH ipHdr     = VIEW(Mbuf.Array(packet)^,IpPktFormat.T),
         tcpHdrBuf = SUBARRAY(Mbuf.Array(curr)^,offset,tcp_hdr_len),
         tcpHdr    = VIEW(tcpHdrBuf,TcpPktFormat.NewT)
     DO
      (* lookup of TCB is in two stages:
         1) check if there exists an a fully qualified incarnation.
         2) check if there is an incarnation in listen mode. 
      *)

      VAR
        inc : Incarnation.T;
      BEGIN
        (* use tcpHdr.dport as lport, and ipHdr.sport as rport *)
        inc.lport := tcpHdr.dport;
        inc.rport := tcpHdr.sport;
        (* use ipHdr.daddr as laddr, and ipHdr.saddr as raddr *)
        inc.laddr := ipHdr.daddr;
        inc.raddr := ipHdr.saddr;

        (* XXX need to protect incarnation.get with a lock *)
        LOCK incMutex DO
          IF incarnation.get(inc,tcb) = FALSE THEN
            (* use tcpHdr.dport only -- so zero out the others *)
            inc.rport := 0;
            inc.laddr := 0;
            inc.raddr := 0;

            (* XXX need to protect incarnation.get with a lock *)
            IF incarnation.get(inc,tcb) THEN
              (* XXX this is a temporary hack. 
                 really need some sort of conn request backlog.
              *)
              tcb.incarnation.lport := tcpHdr.dport;
              tcb.incarnation.rport := tcpHdr.sport;
              tcb.incarnation.laddr := ipHdr.daddr;
              tcb.incarnation.raddr := ipHdr.saddr;
            ELSE
              (* XXX send reset packet --- need a tcprespond function. *)
              RETURN FALSE;
            END;
          END;
        END;
      END;
      (* PROCEDURE Input *)
      VAR 
        len        : INTEGER;
        packetLen  : CARDINAL;
        bytesAcked : CARDINAL;
        temp       : Mbuf.T;
        tryOutput  : BOOLEAN := FALSE;
      BEGIN
        LOCK tcb.inOutMutex DO

          IF TcpDebug.debugLevel > 0 THEN IO.Put("\n"); END;
          INC(TotalNumPacketsRecv);
          (* XXX the net2host conversion should be done in the Handler (mef) *)
          (*Convert back to host format*)
          tcpHdr.seq     := Net.nltoh(tcpHdr.seq);
          tcpHdr.ack_seq := Net.nltoh(tcpHdr.ack_seq); (*this is garbage for a SYN*)
          tcpHdr.window  := Net.nstoh(tcpHdr.window ); 
          tcpHdr.urg_ptr := Net.nstoh(tcpHdr.urg_ptr);
          (* XXX no need to flip these internally  (mef)
             tcpHdr.sport   := Net.nstoh(tcpHdr.sport);
             tcpHdr.dport   := Net.nstoh(tcpHdr.dport);
          *)
          tcpHdr.check   := Net.nstoh(tcpHdr.check);

          IF TcpDebug.debugLevel > 0 THEN IO.Put(tcb.name & " receiving a packet: \n"); END;
          PrintTcpHdr(tcpHdr, HostOrder);

          (* compute tcp data length based on ip totlen and subtracting out ip header *)
          len := Net.nstoh(ipHdr.tot_len) - (ipHdr.hlen * 4);
          
          (* add up the total amount of TCP header+data we've received *)
          TotalNumBytesRecv := TotalNumBytesRecv + len;

          (* compute the amount of data, total - header length *)
          len := len - (tcpHdr.xoff * 4);

          (* XXX If this isn't within our window drop it. (negrin) *)
          (* XXX this is an initial implementation to avoid having 
                 reassembly queue. (mef,negrin)*)
          (* XXX Trim data here, (negrin) *)
          (* XXX use wrap code. (negrin) *)
          IF (TcpPktFormat.Flag.syn IN tcpHdr.flags AND tcb.rcvdSyn) 
            AND (len > 0) 
            AND NOT (Between(tcb.rcv_nxt,TRUE,
                             tcb.rcv_nxt+tcb.rcv_wnd,FALSE,tcpHdr.seq))
            AND NOT (Between(tcb.rcv_nxt,TRUE,
                             tcb.rcv_nxt+tcb.rcv_wnd,FALSE,tcpHdr.seq+len-1)) 
           THEN
            IF TcpDebug.debugLevel > 0 THEN
              IO.Put("Packet Dropped, Sequence Number Out of Range \n"); 
              IO.Put("rcv_nxt  : " & Fmt.Int(tcb.rcv_nxt) & "\n");
              IO.Put("rcv_wnd  : " & Fmt.Int(tcb.rcv_wnd) & "\n");
              IO.Put("seq num  : " & Fmt.Int(tcpHdr.seq) & "\n");
              IO.Put("data len : " & Fmt.Int(len) & "\n");
            END;
            RETURN FALSE;
          END;

          (*If this is an ack AND is invalid, drop the packet.
             The following conditions have to be true for the
             packet to be dropped.
             1. only check this if it is an ack. 
             2. snd_una < ack_seq <= snd_max)
             3. special case of when there is no unacknowledged
             data ie snd_una = ack_seq 
          *)
          (* XXX use wrap code. (negrin) *)
          IF     (TcpPktFormat.Flag.ack IN tcpHdr.flags)  AND   
            NOT  Between(tcb.snd_una, FALSE, tcb.snd_max, TRUE, tcpHdr.ack_seq) AND 
            NOT (tcb.snd_una = tcpHdr.ack_seq)  THEN 

            IF TcpDebug.debugLevel > 0 THEN
              IO.Put("Ack Number out of Range \n");
              IO.Put("snd_una: " & Fmt.Int(tcb.snd_una) & "\n");
              IO.Put("snd_max: " & Fmt.Int(tcb.snd_max) & "\n");
              IO.Put("ack num: " & Fmt.Int(tcpHdr.ack_seq) & "\n");
            END;
            RETURN FALSE;
          END;

          IF (TcpPktFormat.Flag.syn IN tcpHdr.flags) OR
            (TcpPktFormat.Flag.fin IN tcpHdr.flags) THEN
            tcb.rcv_nxt := tcpHdr.seq + 1;
            IF TcpDebug.debugLevel > 0 THEN
              IO.Put("Bytes of data with Syn or Fin: " & Fmt.Int(len) & "\n"); 
            END;
          END;

          (* Need to process data and send ack for it first, then process fin.
             Need to do opposite order for Syn, process Syn first then data *)
          IF (len > 0)  THEN

            (* packet contains data --- pass it up somehow. *)
            IF TcpDebug.debugLevel > 0 THEN 
              IO.Put("Received {"); 
              IO.PutInt(len);
              IO.Put("} ");
            END;
            PrintPkt(curr, offset + tcpHdr.xoff * 4);

            (* XXX this is just plain wrong... but I don't know what to do
               with it now.  It seems more and more that we should decide
               in the context of the remotestatemachine what to do, and
               allow the application to extend the statemachine.  What
               bothers me is that there are lots of actions that need to
               be taken in the established mode.  Some of these actions
               have ordering constraings, others are required (e.g., ack
               processing), etc. (mef) *)
            (* Default states for recveingStates are Estab and FIN_Wait1 (negrin)*)

            IF tcb.t_state IN tcb.recveingStates THEN

              (* XXX need to update rcv_next before we call processPacket,
                 because it may try to send data while we are receiving.
                 Actually, my guess is that this will create a deadlock.
                 Maybe rcv_nxt should be updated as part of processPacket?
                 rcv_nxt should be updated by whoever actually looks/queues
                 the mbuf chain.
              *)
              tcb.rcv_nxt := tcb.rcv_nxt + len;
              tcb.processPacket(curr,offset+tcpHdr.xoff*4);
            END;

            IF NOT((TcpPktFormat.Flag.syn IN tcpHdr.flags) OR
                   (TcpPktFormat.Flag.fin IN tcpHdr.flags)) THEN
              (* send an ack packet *)
              OutputControl(tcb, ack); 
            END;
          END;

          (*XXX need wrap code here (negrin) *)
          (*Conditions:
            It is an ack AND any one of the follwing three.
            1. segment has new data.
            2. segment does not have new data and segment acknowledges new data
            3. segment does not contain acknowledgement of new data, does not
            contain data, but new window is bigger than old window.
          *)

          IF ((TcpPktFormat.Flag.ack IN tcpHdr.flags) AND
            ((tcb.snd_wl1   < tcpHdr.seq)                                        OR
            (tcb.snd_wl1   = tcpHdr.seq     AND tcb.snd_wl2   < tcpHdr.ack_seq) OR
            (tcb.snd_wl2   = tcpHdr.ack_seq AND tcpHdr.window > tcb.snd_wnd)))   THEN

            tcb.snd_wnd := tcpHdr.window;
            tcb.snd_wl1 := tcpHdr.seq;
            tcb.snd_wl2 := tcpHdr.ack_seq;

            IF (tcb.snd_wnd > tcb.max_sndwnd) THEN
              tcb.max_sndwnd := tcb.snd_wnd
            END;

            tryOutput := TRUE;
          END;

          tcb.rcv_adv := tcb.rcv_nxt + tcb.rcv_wnd;
          RemoteSM.ChangeState(tcb, tcpHdr);

          IF TcpDebug.debugLevel > 0 THEN
            IO.Put("state is: " & TcpSM.StateToText(tcb.t_state) & "\n");
          END;

          (*If we filled our window and we just got an ack for some of what 
            we sent, then try to send more. XXX Wrap Code*)
          IF ((tcb.snd_nxt - tcb.snd_una = tcb.snd_wnd) AND
            (tcb.snd_una < tcpHdr.ack_seq)) THEN

            tryOutput := TRUE; 
          END;
          
          (*XXX Wrap Code*)
          WHILE (Ifq.NotEmpty(tcb.sendQ^) AND (tcb.snd_una < tcpHdr.ack_seq)) DO 
            
            temp := Ifq.GetHead(tcb.sendQ^);
            packetLen :=  temp.mh_hdr.mh_len; (*this gets size of 1st mbuf only*)

            (*XXXNeed to use Wrap Code here. Not sure how that will work*)
            bytesAcked := MIN (tcpHdr.ack_seq - tcb.snd_una, 
                               packetLen - tcb.sendQ^.ifq_sendUnaOffset);

            (*Fine grained checking, it is possible only some
              of a packet of data will be acknowledged, in which case I need to keep
              some of the buffer but not all of it. Set the sendUnaOffset
              and put it back on the front of the queue.*)
            
            IF (bytesAcked < packetLen - tcb.sendQ^.ifq_sendUnaOffset ) THEN
              
              tcb.sendQ^.ifq_sendUnaOffset := tcb.sendQ^.ifq_sendUnaOffset + bytesAcked;
              IF TcpDebug.debugLevel > 0 THEN
                IO.Put("Updating sndUnaOffset to: ");
                IO.PutInt(tcb.sendQ^.ifq_sendUnaOffset);
                IO.Put("\n"); 
              END;
            ELSE

              tcb.sendQ^.ifq_sendUnaOffset := 0;
              (* XXX this only gets one mbuf from the chain *)
              temp := Ifq.Dequeue(tcb.sendQ^); 
              Mbuf.m_freem(temp);
              IF TcpDebug.debugLevel > 0 THEN
                IO.Put("Dequeueing and freeing a packet\n"); 
              END;

            END;

            tcb.snd_una := tcb.snd_una + bytesAcked;
            IF TcpDebug.debugLevel > 0 THEN 
              IO.Put("Acking :" & Fmt.Int(bytesAcked) & "\n"); 
              IO.Put("new snd_una: " & Fmt.Int(tcb.snd_una) & "\n"); 
            END;
          END;

          
          (*When the other side acks our fin we need to update snd_una (negrin)
             Conditions for acking a fin.:  
             If it is an ack.
             If it acking the fin sequence number.
             Make sure we sent a fin. (i.e. in a fin sent state)
             XXX This check isn't completely valid because we might not have
             sent the fin yet, but if the seq numbers wrapped then ack_seq might
             equal fin_seq.  
           *)
            
          IF (TcpPktFormat.Flag.ack IN tcpHdr.flags) AND 
             (tcpHdr.ack_seq = tcb.fin_seq + 1) AND
             (tcb.sent_fin) THEN
            tcb.snd_una := tcpHdr.ack_seq;
          END;

          (*XXXwrap code*)

          IF (tcpHdr.ack_seq > tcb.t_rtseq) AND 
            (tcb.t_rxtcur # 0) AND 
            ((tcb.t_state = TcpSM.States.Estab) OR (tcb.t_state = TcpSM.States.FIN_Wait1)) 
           THEN 

            EVAL Clock.CancelAlarm(DataTimeout,tcb);
            IF TcpDebug.debugLevel > 0 THEN 
              IO.Put("Canceling alarm for seq " & Fmt.Int(tcb.t_rtseq) & "\n"); 
            END;

            IF (tcb.snd_nxt > tcb.snd_una) THEN 
              IF TcpDebug.debugLevel > 0 THEN 
                IO.Put("Resetting alarm for seq " & Fmt.Int(tcb.snd_una) & "\n"); 
              END;
              tcb.t_rxtcur  := tcb.t_srtt;
              tcb.t_rtseq := tcb.snd_una;
              Clock.SetAlarm(tcb.t_rxtcur,DataTimeout, tcb);
            ELSE
              tcb.t_rxtcur := 0;
            END;

          END;

          (*If we got an ack for packet we are timing then calculate time and
            get a new timeout value.*)

          IF ((tcb.timing_rtt) AND
              (tcb.t_timed_seq < tcpHdr.ack_seq)) THEN
            tcb.timing_rtt := FALSE;
            tcb.calcRtt(Clock.ReadTicks() - tcb.t_clockVal);
          END;


          (*If the timer gets called and sets snd_nxt equal to 
            snd_una and during the call to Output an ack comes in that moves
            up snd_una, then snd_una would be bigger than snd_nxt. So do a check
            here that if snd_una is bigger to set them equal again. There must
            be a better way to do this. (negrin) *)

          IF tcb.snd_una > tcb.snd_nxt THEN
            tcb.snd_nxt := tcb.snd_una;
          END;
        END; (*Lock*)

        IF (tryOutput) THEN
          Output(tcb,NIL,push);
        END;
      END; (* PROCEDURE INPUT *)
      RETURN FALSE; (* return true if consuming packet *)
    END;
  END Handler;

VAR
  portStart := 20;
  portEnd   := 20000;

FUNCTIONAL
PROCEDURE Guard (<*UNUSED*>packet: Mbuf.T; curr: Mbuf.T; offset: CARDINAL):BOOLEAN = 
  BEGIN
    WITH tcpHeaderBuf = SUBARRAY(Mbuf.Array(curr)^,offset,tcp_hdr_len),
         tcpHeader = VIEW(tcpHeaderBuf,TcpPktFormat.NewT),
         port = Net.nstoh(tcpHeader.dport)
     DO

      RETURN port <= portEnd AND port >= portStart;
    END;
  END Guard;

PROCEDURE Run(pp: ParseParams.T): BOOLEAN =
  TYPE Buffer = REF ARRAY OF CHAR;

  VAR
    m          : Mbuf.T;    
    numBytes   : INTEGER := 0;
    numPackets : INTEGER := 0;
    data       : REF ARRAY OF CHAR;
    resetPort  : Ctypes.unsigned_short;
    tcb        : TcpCtrlBlock.T;
    inc        : Incarnation.T;
    closeAfterSend : INTEGER := 0;

  PROCEDURE Walker(pfunc : PROCEDURE(tcb: TcpCtrlBlock.T)) = 
    VAR
      tcb : TcpCtrlBlock.T;
    BEGIN
      WITH iterate = incarnation.iterate() DO
        WHILE iterate.next(inc,tcb) = TRUE DO
          pfunc(tcb);
        END;
      END;
    END Walker;

  PROCEDURE PrintInfo(tcb: TcpCtrlBlock.T) = 
    BEGIN
      IO.Put(tcb.name);
      IO.Put(" - [");
      IO.PutInt(Net.nstoh(tcb.incarnation.lport));
      IO.Put(",");
      IO.PutInt(Net.nstoh(tcb.incarnation.rport));
      IO.Put(",");
      IO.PutInt(tcb.incarnation.laddr);
      IO.Put(",");
      IO.PutInt(tcb.incarnation.raddr);
      IO.Put("] ");
      IO.Put(" state is: ");
      IO.Put(TcpSM.StateToText(tcb.t_state));
      IO.Put("\n"); 
    END PrintInfo;

  PROCEDURE Reset(tcb: TcpCtrlBlock.T) = 
    BEGIN
      (* XXX aweful hack, but I'm tired right now *)
      IF tcb.incarnation.lport = resetPort THEN
        EVAL tcb.init(tcb.name & "-reset");
        CleanUp(tcb);
      END;
    END Reset;

  BEGIN
    pp.reset();

    TRY
      pp.skipNext(); (* m3tcp *)

      IF pp.testNext("-zap") THEN
        Walker(CleanUp);
        Uninit();

      ELSIF pp.testNext("-test") THEN

        (*purely for testing random things, does not appear in command usage.*)
        WITH name = "po" DO
          tcb := NEW(TcpCtrlBlock.T).init(name);
        END;

        IF TcpSM.States.Estab IN tcb.recveingStates THEN
          IO.Put("this worked, estab is a receiving state\n");
        END;

        IF TcpSM.States.Close_Wait IN tcb.sendingStates THEN
          IO.Put("close_wait is in sendStates\n");
        END;

      ELSIF pp.testNext("-reset") THEN

        (* reset a particular connection *)
        resetPort := Net.nstoh(pp.getNextInt());
        Walker(Reset);

      ELSIF pp.testNext("-pktlossrate") THEN
        
        lossRate := pp.getNextInt();

      ELSIF pp.testNext("-state") THEN

        Walker(PrintInfo);
        IO.Put("Total Number of Sends attempted: " & Fmt.Int(TotalNumPacketsAttempted) & "\n");
        IO.Put("Total Number of Packets Sent: " & Fmt.Int(TotalNumPacketsSent) & "\n");
        IO.Put("Total Number of Packets Received: " & Fmt.Int(TotalNumPacketsRecv) & "\n");
        IO.Put("Total Number of Bytes Sent: " & Fmt.Int(TotalNumBytesSent) & "\n");
        IO.Put("Total Number of Bytes Received: " & Fmt.Int(TotalNumBytesRecv) & "\n");
        IO.Put("Total Number of Timeouts: " & Fmt.Int(TotalNumTimeOuts) & "\n");
        IO.Put("Total Bytes  of Data Sent: " & Fmt.Int(TotalNumDataBytesSent) & "\n");
        IO.Put("Packet Loss Rate set to one loss every: " & Fmt.Int(lossRate) & "\n"); 

      ELSIF pp.testNext("-passiveopen") THEN
        inc.lport := Net.nstoh(pp.getNextInt());
        inc.rport := 0;
        inc.laddr := 0;
        inc.raddr := 0;
          
        WITH name = "po" & Fmt.Int(Net.nstoh(inc.lport)) DO
          tcb := NEW(TcpCtrlBlock.T).init(name);
        END;
        tcb.incarnation := inc;
        tcb.t_maxseg := pp.getNextInt();
        LOCK incMutex DO
          EVAL incarnation.put(inc,tcb);
        END;
        tcb.changeLocalState(TcpSM.States.Listen);
      
      ELSIF pp.testNext("-activeopen") THEN

          (* XXX extra work required
             1) allocate a new tcb

             2) insert it into port management table after we've figured
             out what the incarnation values are. 
          *)

          inc.lport := Net.nstoh(pp.getNextInt());
          (*        IO.Put("src port: "); IO.PutInt(Net.htonl(tcb.sport)); IO.Put("\n");*)
          inc.rport := Net.nstoh(pp.getNextInt());
          (*        IO.Put("dest port: "); IO.PutInt(Net.htonl(tcb.dport)); IO.Put("\n");*)
          TRY
            inc.laddr := NetDb.GetHostByName(pp.getNext());
            (*        IO.Put("source addr: "); IO.PutInt(Net.htonl(tcb.saddr)); IO.Put("\n");*)
            inc.raddr := NetDb.GetHostByName(pp.getNext());
            (*        IO.Put("dest addr: "); IO.PutInt(Net.htonl(tcb.daddr)); IO.Put("\n");*)
          EXCEPT
          | NetDb.HostNotFound =>
            IO.Put("Host not found.\n");
          END;

          (* insert into incarnation table *)

          (* XXX need locking on the incarnation table *)
          LOCK incMutex DO
          IF incarnation.get(inc, tcb) = FALSE THEN
            WITH name = "ao" & Fmt.Int(Net.nstoh(inc.lport)) DO
              tcb := NEW(TcpCtrlBlock.T).init(name);
            END;
            EVAL incarnation.put(inc,tcb);
            tcb.changeLocalState(TcpSM.States.SYN_Sent); (*Send a SYN*)
          ELSE
            IO.Put("A tcb with [");
            IO.PutInt(Net.nstoh(inc.lport));
            IO.Put(",");
            IO.PutInt(Net.nstoh(inc.rport));
            IO.Put(",");
            IO.PutInt(inc.laddr);
            IO.Put(",");
            IO.PutInt(inc.raddr);
            IO.Put("]");
            IO.Put(" already exists.\n");
          END;
          END;
        
      ELSIF pp.testNext("-passiveclose") THEN
     
          inc.lport := Net.nstoh(pp.getNextInt());
          inc.rport := 0; (* Net.nstoh(pp.getNextInt()); *)
          inc.laddr := 0; (* NetDb.GetHostByName(pp.getNext()); *)
          inc.raddr := 0; (* NetDb.GetHostByName(pp.getNext()); *)

          (* This should only get called if the user is a server in the
             close_wait state, and is responding to a FIN *)

          IF incarnation.get(inc, tcb) = TRUE THEN
            IF (tcb.t_state = TcpSM.States.Close_Wait) THEN
              LocalSM.ChangeState(tcb, TcpSM.States.Last_Ack);
            END;
          ELSE
            IO.Put("A tcb with [");
            IO.PutInt(Net.nstoh(inc.lport));
            IO.Put(",");
            IO.PutInt(Net.nstoh(inc.rport));
            IO.Put(",");
            IO.PutInt(inc.laddr);
            IO.Put(",");
            IO.PutInt(inc.raddr);
            IO.Put("]");
            IO.Put(" doesn't exists.\n");
          END;

      ELSIF pp.testNext("-activeclose") THEN
          inc.lport := Net.nstoh(pp.getNextInt());
          inc.rport := 0; (* Net.nstoh(pp.getNextInt()); *)
          inc.laddr := 0; (*NetDb.GetHostByName(pp.getNext()); *)
          inc.raddr := 0; (* NetDb.GetHostByName(pp.getNext()); *)

          IF incarnation.get(inc, tcb) = TRUE THEN
              LocalSM.ChangeState(tcb, TcpSM.States.FIN_Wait1);
          ELSE
            IO.Put("A tcb with [");
            IO.PutInt(Net.nstoh(inc.lport));
            IO.Put(",");
            IO.PutInt(Net.nstoh(inc.rport));
            IO.Put(",");
            IO.PutInt(inc.laddr);
            IO.Put(",");
            IO.PutInt(inc.raddr);
            IO.Put("]");
            IO.Put(" doesn't exists.\n");
          END;
      ELSIF pp.testNext("-senddata") THEN

          inc.lport := Net.nstoh(pp.getNextInt());
          inc.rport := 0; (* Net.nstoh(pp.getNextInt());        *)
          inc.laddr := 0; (* NetDb.GetHostByName(pp.getNext()); *)
          inc.raddr := 0; (* NetDb.GetHostByName(pp.getNext()); *)

          numBytes := pp.getNextInt();
          numPackets := pp.getNextInt();
          closeAfterSend := pp.getNextInt();

        IF numBytes = 0 OR numPackets = 0 THEN 
          IO.Put("Must be greater than zero\n");
          RETURN TRUE;
        END;

        LOCK incMutex DO
          IF incarnation.get(inc,tcb) = FALSE THEN
            RETURN TRUE;
          END;
        END;
        FOR j := 0 TO (numPackets - 1) (**100*)
         DO
(*          data := NEW (Buffer, 20*numBytes);*)
          data := NEW (Buffer, numBytes);

          FOR i := 0 TO numBytes - 1
           DO

            data[i] := 'a';
(*            data[i*20 + 0] := 'a';
            data[i*20 + 1] := 'b';
            data[i*20 + 2] := 'c';
            data[i*20 + 3] := 'd';
            data[i*20 + 4] := 'e';

            data[i*20 + 5] := 'f';
            data[i*20 + 6] := 'g';
            data[i*20 + 7] := 'h';
            data[i*20 + 8] := 'i';
            data[i*20 + 9] := 'j';

            data[i*20 + 10] := 'k';
            data[i*20 + 11] := 'l';
            data[i*20 + 12] := 'm';
            data[i*20 + 13] := 'n';
            data[i*20 + 14] := 'o';

            data[i*20 + 15]   := 'p';
            data[i*20 + 16]   := 'q';
            data[i*20 + 17]   := 'r';
            data[i*20 + 18]   := 's';
            data[i*20 + 19]   := '\n';
*)
          END;

          TRY
            m := Mbuf.MclGetOa(data, (*20**)numBytes);
          EXCEPT
          | Mbuf.LengthMismatch =>
            IO.Put("PANIC: mbuf length mismatch exception.\n");
          END;
          Output(tcb, m, push); (*send data*)
        END;

        IF closeAfterSend > 0 THEN
          tcb.changeLocalState(TcpSM.States.FIN_Wait1);
        END;

      ELSIF pp.testNext("-debug") THEN
        
          TcpDebug.debugLevel := pp.getNextInt();

      ELSE
        Usage();

      END;

    EXCEPT
      ParseParams.Error => Usage();
    END;
    RETURN TRUE;

  END Run;

PROCEDURE Usage() = 
  BEGIN
    IO.Put("m3tcp -zap");
    IO.Put("\n");
    IO.Put("m3tcp -reset localPort");
    IO.Put("\n");
    IO.Put("m3tcp -pktlossrate rate");
    IO.Put("\n");    
    IO.Put("m3tcp -state");
    IO.Put("\n");
    IO.Put("m3tcp -passiveopen localPort max_seg");
    IO.Put("\n");
    IO.Put("m3tcp -activeopen localPort");
    IO.Put("\n");
    IO.Put("m3tcp -passiveclose localPort");
    IO.Put("\n");
    IO.Put("m3tcp -activeclose localPort");
    IO.Put("\n");
    IO.Put("m3tcp -senddata localPort packet_size(units of 20) numPackets CloseAfterSend");
    IO.Put("\n");
    IO.Put("m3tcp -debug level (0 for off, 1 for on) ");
    IO.Put("\n");    

  END Usage;

PROCEDURE PacketSend(
       daddr : IpPktFormat.Address;
       saddr : IpPktFormat.Address;
       data  : Mbuf.T) =
  CONST 

  VAR 
    size   : CARDINAL;
    ip : IpPktFormat.Header;     
    ipovl_buf : ARRAY [1 .. BYTESIZE(IpPktFormat.OverlayHeader)] OF Net.BYTE;
    ipovl_csum: Ctypes.unsigned_short;
  BEGIN

    (* compute the length of the ip packet *)
      size :=  Mbuf.m_length (data);

    ip.protocol := protocol;
    ip.daddr    := daddr;
    ip.saddr    := saddr;
    ip.tot_len  := 0;
    ip.ttl      := 30;

    (* ip.tot_len  := size*)


    WITH header_buf = Mbuf.Array(data) DO
      WITH header = VIEW(header_buf^,TcpPktFormat.NewT) 
       DO

        (* cons up ip overlay header for checksum *)
        WITH ipovl = VIEW(ipovl_buf,IpPktFormat.OverlayHeader) DO

          ipovl.fill[0]:= 0;
          ipovl.fill[1]:= 0;
          ipovl.ih_x1  := 0;
          ipovl.ih_pr  := protocol;
          ipovl.ih_len := Net.htons(size); (* XXX this is odd. Should be size + ip_hdr_len *)
          ipovl.ih_src := ip.saddr; (*already in network order because we never swapped*)
          ipovl.ih_dst := ip.daddr; (*already in network order because we never swapped*)
        END;
        ipovl_csum := Net.checksum(ipovl_buf);

        (* compute new checksum for tcp packet with ip overlay header *)
        header.check := 0;
        header.check := Mbuf.Checksum(data,ipovl_csum,size);
      END;
    END;        

    IF ((lossRate = 0) OR  (TotalNumPacketsAttempted = 0) OR 
        (TotalNumPacketsAttempted MOD lossRate) # 0) THEN

      TotalNumPacketsSent := TotalNumPacketsSent + 1;
      TotalNumBytesSent := TotalNumBytesSent + size;
      (* send as ip packet - no options *)
      IpGen.PacketSend(ip, data);
    ELSE 
      IF TcpDebug.debugLevel > 0 THEN IO.Put("\nFAKING PACKET LOSS \n");  END;
    END;
    TotalNumPacketsAttempted := TotalNumPacketsAttempted + 1;
  END PacketSend;

PROCEDURE PrintPkt(mbuf : Mbuf.T ; <*UNUSED*> hdrLen : CARDINAL ) = 
  VAR
    m: Mbuf.T;
  BEGIN
    IF mbuf = NIL THEN RETURN; END;

    m := mbuf;
    IF TcpDebug.debugLevel > 0 THEN

      IO.Put(Fmt.Int(Mbuf.m_length(m)) & " bytes {");
      (*    WITH buf = SUBARRAY(Mbuf.Array(m)^, hdrLen, Mbuf.m_length(m) - hdrLen)
            DO
            FOR i := FIRST(buf) TO LAST(buf)
            DO
            IO.Put(Text.FromChar(buf[i]));
            END;
            m := m.mh_hdr.mh_next;
            END;
            
            WHILE (MOD # NIL) DO
            IO.Put("looped through\n");
            
            WITH buf = Mbuf.Array(m)^
            DO
            
            FOR i := FIRST(buf) TO LAST(buf)
            DO
            IO.Put(Text.FromChar(buf[i]));
            END;
            END;
            m := m.mh_hdr.mh_next;
            END;
      *)
      IO.Put("}\n");
    END;

  END PrintPkt;

PROCEDURE PrintTcpHdr(READONLY hdr: TcpPktFormat.NewT; MemLayout : INTEGER)=
  VAR first := TRUE;
  BEGIN

    IF TcpDebug.debugLevel > 0 THEN

      IO.Put("flags {");
      FOR flag := TcpPktFormat.Flag.fin TO TcpPktFormat.Flag.urg DO
        IF flag IN hdr.flags THEN
          IF NOT first THEN 
            IO.Put(" "); 
          ELSE
            first := FALSE;
          END;
          IO.Put(flagnames[flag]);
        END;
      END;
      IO.Put("} ");
      
      IO.Put("srcport ");
(* XXX
      IF (MemLayout = NetworkOrder) THEN
      IO.PutInt(Net.nstoh(hdr.sport));
      ELSE
        IO.PutInt(hdr.sport);
      END;
 *)
      IO.PutInt(Net.nstoh(hdr.sport));
      IO.Put(" ");
      
      IO.Put("dstport ");
(* XXX
      IF (MemLayout = NetworkOrder) THEN
        IO.PutInt(Net.nstoh(hdr.dport));
      ELSE
        IO.PutInt(hdr.dport);
      END;
 *)
      IO.PutInt(Net.nstoh(hdr.dport));
      IO.Put(" ");

      IO.Put("seq ");
      IF (MemLayout = NetworkOrder) THEN
        IO.Putx(Net.nltoh(hdr.seq));
      ELSE
        IO.Putx(hdr.seq);
      END;
      IO.Put(" ");
      
      IO.Put("ackseq ");
      IF (MemLayout = NetworkOrder) THEN
        IO.Putx(Net.nltoh(hdr.ack_seq));
      ELSE
        IO.Putx(hdr.ack_seq);
      END;
      IO.Put(" ");
      
      IO.Put("window ");
      IF (MemLayout = NetworkOrder) THEN
        IO.PutInt(Net.nstoh(hdr.window));
      ELSE
        IO.PutInt(hdr.window);
      END;
      IO.Put(" ");
      
      IO.Put("urg_ptr ");
      IF (MemLayout = NetworkOrder) THEN
        IO.PutInt(Net.nstoh(hdr.urg_ptr));
      ELSE
        IO.PutInt(hdr.urg_ptr);
      END;
      IO.Put(" ");
      
      IO.Put("xoff ");
      IO.PutInt(hdr.xoff);
      IO.Put("\n");
    END; (*IF Debug*)
      
  END PrintTcpHdr;
    
PROCEDURE Uninit() = 
  BEGIN

    (*XXXNeed to kill InitialSeqTimer.  Will the TcpCtrlBlock module 
      go away if m3tcp goes away ?*)

    (* uninstall tcp event handler *)
    Tcp.Uninstall(m3tcp);

    (* uninstall shell event handler *)
    M3TcpCmd.Uninstall();

    (* nil out global refs for GC *)
    incarnation := NIL;
    incMutex := NIL;
    m3tcp := NIL;
  END Uninit;

PROCEDURE Init() = 
  BEGIN
    (* setup the incarnation table and install on the tcp event *)
    incMutex := NEW(MUTEX);
    incarnation := NEW(IncarnationTbl.Default).init();
    m3tcp := Tcp.Install(Tcp.PacketArrived, Guard, Handler);

    (* support for domain/dynlink -- export interfaces *)
    BEGIN
      TRY
        EVAL M3TcpInterface.Export(NIL);
      EXCEPT
      | NameServer.Error (* (ec) *)  => 
        IO.PutError("M3TCP Link init FAILED\n");
      END;
    END
  END Init;

BEGIN
  Init();
END M3Tcp.

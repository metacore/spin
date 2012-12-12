MODULE TcpStuff;
IMPORT Httpd;
IMPORT Incarnation;
IMPORT TcpCtrlBlock;
IMPORT M3Tcp, TcpSM;
IMPORT Mbuf, Net;
IMPORT IO, Fmt;
IMPORT TcpPktFormat;
IMPORT Ifq;
(* IMPORT BlockList; *)
IMPORT Text;
IMPORT Error;
IMPORT Errno;

IMPORT Sema, Thread, Clock, ThreadExtra;

TYPE T = TcpCtrlBlock.T OBJECT
  queue : REF Ifq.ifqueue;
  sema  : Sema.T;
  thread : Thread.T;
OVERRIDES
  processPacket   := ProcessPacket;

  remoteListen    := Listen;

  localTime_Wait := Time_Wait;
END;

VAR num: CARDINAL := 0;
CONST
  syn    = TcpPktFormat.Flags{TcpPktFormat.Flag.syn};
  ack    = TcpPktFormat.Flags{TcpPktFormat.Flag.ack};
  synack = syn + ack; 
  rst    = TcpPktFormat.Flags{TcpPktFormat.Flag.rst};

PROCEDURE ProcessPacket(
    tcb    : T; 
  m      : Mbuf.T;
  offset : CARDINAL) = 
  VAR
    top: Mbuf.T;
  BEGIN
    (* actually received some data on this tcb. *)
    top := m;
    top := Mbuf.m_copym(top,offset,Mbuf.M_COPYALL,Mbuf.M_WAIT);
    IF tcb.queue = NIL THEN
      tcb.queue := NEW(REF Ifq.ifqueue);
      tcb.sema := Sema.Alloc();
      Ifq.Init(tcb.queue^);
      tcb.thread := ThreadExtra.PFork(Receive, tcb);
    END;
    Ifq.Enqueue(tcb.queue^,top);
    Sema.V(tcb.sema);
  END ProcessPacket;

PROCEDURE Listen(tcb: T; READONLY tcpHdr : TcpPktFormat.NewT) =
  BEGIN
    (*Ignore resets*)
    IF (TcpPktFormat.Flag.rst IN tcpHdr.flags) THEN
      RETURN;
    END;
      
    IF (TcpPktFormat.Flag.ack IN tcpHdr.flags) THEN
      tcb.snd_nxt := tcpHdr.ack_seq;
      M3Tcp.OutputControl(tcb, rst); 
      tcb.t_state := TcpSM.States.Closed;
    
    ELSIF (TcpPktFormat.Flag.syn IN tcpHdr.flags) THEN 
      VAR
        newtcb: TcpCtrlBlock.T;
      BEGIN
        (* place a new tcb that listens for the next
           connection into the incarnation table with
           lport=port and the other fields=0
        *)

        LOCK M3Tcp.incMutex DO
          newtcb := NEW(T).init("httpd-"&Fmt.Int(Net.nstoh(tcb.incarnation.lport))&":"&Fmt.Int(num));
          INC(num);
          newtcb.incarnation.lport := tcb.incarnation.lport;
          newtcb.incarnation.rport := 0;
          newtcb.incarnation.laddr := 0;
          newtcb.incarnation.raddr := 0;
          newtcb.changeLocalState(TcpSM.States.Listen);
          newtcb.t_maxseg          := tcb.t_maxseg;

          (* The following put should be cheap because the key already
             exists and we are just putting in a new tcp that is in the
             listen state *)
          EVAL M3Tcp.incarnation.put(newtcb.incarnation,newtcb);

          (* As for the tcb that just got a syn, we update its
             incarnation so that future packets are directed to it.  *)
          tcb.irs := tcpHdr.seq;      
          
          EVAL M3Tcp.incarnation.put(tcb.incarnation,tcb);
        END;

        M3Tcp.OutputControl(tcb, synack); 
        Clock.SetAlarm(tcb.t_srtt, TcpSM.SynTimeout, tcb);
        tcb.t_state := TcpSM.States.SYN_Rcvd;
      END;
    END;

  END Listen;

PROCEDURE Time_Wait (self: T; expectedState: TcpSM.States) = 
  VAR 
    me: TcpCtrlBlock.T;
  BEGIN
    (* clean up after myself *)
    LOCK M3Tcp.incMutex DO
      EVAL M3Tcp.incarnation.delete(self.incarnation, me);
    END;
    
    (* do what the super type usually does when closing. *)
    TcpCtrlBlock.T.localTime_Wait(self,expectedState);
  END Time_Wait;

PROCEDURE Receive(arg: REFANY) : REFANY =
  VAR 
    req      : Httpd.Request;
    recvdata : Mbuf.T;
    databuf  : Net.Payload;

  BEGIN
    WITH tcb = NARROW(arg,T) DO 

      (* XXX need to make sure to wakeup this thread in case the
         connection dies too early. *)
      REPEAT
        Sema.P(tcb.sema);
      UNTIL Ifq.NumBytes(tcb.queue^) > 8;
      (* process packet *)

      tcb.sema := NIL;
      tcb.thread := NIL;

      recvdata := Ifq.GetHead(tcb.queue^);
      (* XXX this is a horrible hack
         NIL out the rest of the queue *)
      tcb.queue.ifq_head          := NIL;
      tcb.queue.ifq_tail          := NIL;
      tcb.queue.ifq_len           := 0;
      tcb.queue.ifq_bytes         := 0; 
      tcb.queue.ifq_maxlen        := 0; 
      tcb.queue.ifq_drops         := 0;
      tcb.queue.ifq_sendUnaOffset := 0;

      (* XXX checking sanity. *)
      IF recvdata = NIL THEN 
        IO.Put(tcb.name);  
        IO.Put(" got nothing.\n");
        (* close the connection *)
        tcb.changeLocalState(TcpSM.States.FIN_Wait1);
        RETURN NIL;
      END;

      databuf := Mbuf.Array(recvdata);
      TRY
        Httpd.ParseRequest(databuf^, req);
        IF FALSE THEN 
          IO.Put("[");
          IO.PutInt(Text.Length(req.url));
          IO.Put("]");
          IO.Put("GET ");
          IO.Put(req.url);
          IO.Put("\n");
        END;
        Httpd.ServeFromDisk(tcb, req, FALSE);
      EXCEPT
      | Httpd.FileNotFound(file) => IO.Put(file);
      | Httpd.BadRequest(msg) => IO.Put(msg);
      | Httpd.NotDoneYet(msg) => IO.Put(msg);
      | Error.E(e) => IO.Put(e.message());
      | Errno.E => (* ignore - remote is gone. *)
      ELSE
        (* ignore, unknown exception *)
      END;
      Mbuf.m_freem(recvdata);

      (* close the connection *)
      tcb.changeLocalState(TcpSM.States.FIN_Wait1);
      IF FALSE THEN 
        IO.Put(tcb.name);  IO.Put(" done.\n");
      END;
    END;
    RETURN NIL;
  END Receive;

PROCEDURE CreateListen(lport: CARDINAL) = 
  VAR
    tcb        : TcpCtrlBlock.T;
    inc        : Incarnation.T;
  BEGIN
    inc.lport := Net.nstoh(lport);
    inc.rport := 0; (* Net.nstoh(pp.getNextInt()); *)
    inc.laddr := 0; (* NetDb.GetHostByName(pp.getNext()); *)
    inc.raddr := 0; (* NetDb.GetHostByName(pp.getNext()); *)

    WITH name = "httpd-" & Fmt.Int(Net.nstoh(inc.lport)) DO
      tcb := NEW(T).init(name);
    END;
    tcb.incarnation := inc;
    LOCK M3Tcp.incMutex DO 
      EVAL M3Tcp.incarnation.put(inc,tcb);
    END;
    tcb.changeLocalState(TcpSM.States.Listen);
  END CreateListen;

BEGIN
END TcpStuff.

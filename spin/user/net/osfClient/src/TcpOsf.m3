(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 31-May-97  David Becker at the University of Washington
 *      Replaced If interface with NetDev/EtherDev
 *
 * 06-Oct-96  Robert Grimm (rgrimm) at the University of Washington
 *      added call to DispatcherPrivate.KeepStub in Init proc
 *      to ensure that the PacketSend event can be handled.
 *
 * 19-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	make guards FUNCTIONAL
 *
 * 13-Jun-96  Marc Fiuczynski (mef) at the University of Washington
 *	Optimize for small packets on the receive path.
 *
 * 20-May-96  Przemek Pardyak (pardy) at the University of Washington
 *	Changed the dispatcher interface.
 *
 * 20-Apr-96  Wilson Hsieh (whsieh) at the University of Washington
 *	PROCANY change
 *
 * 29-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Now automatically (un)installing event handler with closure on
 *	tcp ports.
 *
 * 07-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Converted to new spin shell commands.
 *
 * 03-Aug-95  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *)

MODULE TcpOsf;
IMPORT Tcp, Net, Mbuf, MbufPublic, IpPktFormat, OsfNet,
       OsfNetEmulation, Dispatcher, IO, Ctypes, Fmt, Port, Key,
       TcpOsfTbl, Spy, IpRoute, SocketRep, SocketAddr, Word,
       Thread, Clock, ParseParams;
IMPORT DispatcherPrivate; (* XXX get rid of this *)

PROCEDURE ExceptionPrint(ec:Dispatcher.ErrorCode) = 
  BEGIN
    CASE ec OF
    | Dispatcher.ErrorCode.InvalidProcedure =>
      IO.Put("udprpc invalid procedure installed.\n");
    ELSE
      IO.Put("udprpc dispatcher install error.\n");
    END;
  END ExceptionPrint;

PROCEDURE Install (
    event: Tcp.PacketArrivedEvent;
    whenClause: Tcp.PacketArrivedEvent; 
    handler: Tcp.PacketArrivedEvent): Dispatcher.Binding =
  VAR
    binding : Dispatcher.Binding;
  BEGIN
    TRY
      TRY
        binding := Dispatcher.InstallHandler(event,
                                             whenClause, 
                                             handler,
                                             options := Dispatcher.Options{Dispatcher.Opt.First}
                                             );
      EXCEPT
      | Dispatcher.Error(ec) => ExceptionPrint(ec);
      END;
    FINALLY
      RETURN binding;
    END;
  END Install;

PROCEDURE all_or_none_guard(
    <*UNUSED*> packet : Mbuf.T;
    curr              : Mbuf.T; 
    offset            : CARDINAL):BOOLEAN =
  BEGIN
    WITH tcpHeaderBuf = SUBARRAY(Mbuf.Array(curr)^,offset,tcp_hdr_len),
         tcpHeader = VIEW(tcpHeaderBuf,T)
     DO
      RETURN tcpHeader.dport = port;
    END;
  END all_or_none_guard;

PROCEDURE empty(
    <*UNUSED*> packet : Mbuf.T;
    <*UNUSED*> curr   : Mbuf.T; 
    <*UNUSED*> offset : CARDINAL):BOOLEAN =
  BEGIN
    RETURN firefirefire;
  END empty; 

PROCEDURE pa(
    <*UNUSED*> packet : Mbuf.T;
    <*UNUSED*> curr   : Mbuf.T; 
    <*UNUSED*> offset : CARDINAL):BOOLEAN =
  BEGIN
    RETURN FALSE;
  END pa;


VAR
  spindles     : ARRAY [1..100] OF REFANY;
  currspindle  : CARDINAL := 0;
  firefirefire : BOOLEAN := FALSE;
  port         := Net.htons(0);

PROCEDURE Run(pp: ParseParams.T): BOOLEAN =
  VAR
    j : CARDINAL;
  BEGIN
    pp.reset();
    TRY
      pp.skipNext(); (* ipdefault *)
      IF pp.testNext("-debug") THEN
        debug_level := Net.MapDebug(pp.getNextInt());
      ELSIF pp.testNext("-installguard") THEN
        IF pp.testNext("empty") THEN
          j := pp.getNextInt();
          FOR i:= 1 TO j DO
            INC(currspindle);
            spindles[currspindle] := Install(Tcp.PacketArrived,
                                             empty,
                                             pa);
          END;
        ELSIF pp.testNext("full") THEN
          j := pp.getNextInt();
          FOR i:= 1 TO j DO
            INC(currspindle);
            spindles[currspindle] := Install(Tcp.PacketArrived,
                                             all_or_none_guard,
                                             pa);
          END;
        END;
      ELSIF pp.testNext("-uninstallguard") THEN
        WHILE currspindle > 0 DO
          Tcp.Uninstall(spindles[currspindle]);
          spindles[currspindle] := NIL;
          DEC(currspindle);
        END;
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
    IO.Put("Usage: " & CommandName & ":" & CommandHelp & "\n");
  END Usage;

CONST
  debug  = FALSE;
  timing = OsfNetEmulation.timing;

VAR
  debug_level : Net.Level := Net.oLevel.NODEBUG;

TYPE 
  portTableT = RECORD
    table: TcpOsfTbl.Default;
    mutex : MUTEX;
  END;

VAR
  dead, portTable : portTableT;


VAR
  recvtimer : Spy.T;
  tcptimer  : Spy.T;
  dataflow  : Spy.T;
  ipsendtimer     : Spy.T;




PROCEDURE AddTcpPortHandler(port:Ctypes.unsigned_short) =
  VAR 
    portEntry  : Port.T;
    key   : Key.T;
  BEGIN
    LOCK portTable.mutex DO
      key := port;

      IF debug AND debug_level # Net.oLevel.NODEBUG THEN
        Net.Debug(debug_level, Net.oLevel.WARNING,"TcpOsf AddTcpPortHandler on " &
          Fmt.Int(Net.nstoh(port)));
      END;

      IF portTable.table.get(key,portEntry) = FALSE THEN
        portEntry := NEW(Port.T);
        portEntry.count := 0;
        portEntry.port := port;
        portEntry.binding := 
            Tcp.InstallWithClosure(Tcp.PacketArrived,
                                   TcpInputWC,TcpInputPA,
                                   portEntry,
                                   portEntry);
        EVAL portTable.table.put(key,portEntry);

        IF debug AND debug_level # Net.oLevel.NODEBUG THEN
          Net.Debug(debug_level, Net.oLevel.WARNING," and event handler installed.");
        END;
      END;
      INC(portEntry.count);

      IF debug AND debug_level # Net.oLevel.NODEBUG THEN
        Net.Debug(debug_level, Net.oLevel.WARNING,"\n");
      END;
    END;
  END AddTcpPortHandler;

CONST DefaultTimeout = 1024 * 60;
VAR timeoutPending : BOOLEAN := FALSE;
PROCEDURE Timeout(<* UNUSED *> arg: REFANY) =
  VAR
    key: Key.T;
    val: Port.T;
  BEGIN
    Thread.Acquire(dead.mutex);
    WITH iterate = dead.table.iterate() DO
      WHILE iterate.next(key,val) = TRUE DO
        IF val.count = 1 THEN
          IF Port.States.Reap IN val.state THEN
            EVAL dead.table.delete(key,val);
            Thread.Release(dead.mutex);
            LOCK portTable.mutex DO
              IF debug AND debug_level # Net.oLevel.NODEBUG THEN
                Net.Debug(debug_level, Net.oLevel.WARNING," and event handler uninstalled.");
              END;
              EVAL portTable.table.delete(key,val);
              Tcp.Uninstall(val.binding);
            END;
            Thread.Acquire(dead.mutex);
          ELSIF Port.States.Dead IN val.state THEN
            val.state := Port.State{Port.States.Reap};
          END;
        END;
      END
    END;
    IF dead.table.size() = 0 THEN
      timeoutPending := FALSE;
    ELSE
      timeoutPending := TRUE;
      Clock.SetAlarm(DefaultTimeout,Timeout,NIL);
    END;
    Thread.Release(dead.mutex);
  END Timeout;

PROCEDURE DelTcpPortHandler(port:Ctypes.unsigned_short) =
  VAR
    val : Port.T;
    key : Key.T;
  BEGIN
    LOCK portTable.mutex DO
      key := port;

      IF debug AND debug_level # Net.oLevel.NODEBUG THEN
        Net.Debug(debug_level, Net.oLevel.WARNING,"TcpOsf DelTcpPortHandler for " & 
          Fmt.Int(Net.nstoh(port)));
      END;

      IF portTable.table.get(key,val) = TRUE THEN
        IF val.count = 1 THEN 
          LOCK dead.mutex DO
            EVAL dead.table.put(key,val);
            IF NOT timeoutPending THEN
              timeoutPending := TRUE;
              Clock.SetAlarm(DefaultTimeout,Timeout,NIL);
            END;
          END;
        ELSE
          DEC(val.count);
        END
      END;

      IF debug AND debug_level # Net.oLevel.NODEBUG THEN
        Net.Debug(debug_level, Net.oLevel.WARNING,"\n");
      END;

    END;
  END DelTcpPortHandler;

CONST
  tcp_hdr_len = BYTESIZE(T);

FUNCTIONAL
PROCEDURE TcpInputWC(
    closure: REFANY;
    <* UNUSED *> packet: Mbuf.T; 
    curr: Mbuf.T; 
    offset: CARDINAL):BOOLEAN = 
  BEGIN
    (* #ifdef debug_level != NODEBUG *)
(*
    IF debug AND debug_level # Net.oLevel.NODEBUG THEN
      Net.Debug(debug_level, Net.oLevel.INFO,"TcpOsf WC ")
    END;
*)
    WITH tcpHeaderBuf = SUBARRAY(Mbuf.Array(curr)^,offset,tcp_hdr_len),
         tcpHeader = VIEW(tcpHeaderBuf,T),
         port = NARROW(closure,Port.T).port
       DO
      RETURN tcpHeader.dport = port;
    END;
  END TcpInputWC;

PROCEDURE TcpInputPA(
    <* UNUSED *> closure: REFANY; 
    packet: Mbuf.T;
    <* UNUSED *> curr : Mbuf.T; 
    <* UNUSED *> offset: CARDINAL):BOOLEAN = 
  VAR
    data, copy_packet : Mbuf.T;
  BEGIN
    (* #ifdef debug_level != NODEBUG *)
    IF debug AND debug_level # Net.oLevel.NODEBUG THEN
      Net.Debug(debug_level, Net.oLevel.INFO,"TcpOsf PA ")
    END;

    IF timing THEN Spy.Enter(recvtimer); END;

    WITH origpkt = Mbuf.Array(packet),
         ip_header = VIEW(origpkt^,IpPktFormat.T),
         ip_hdr_len = ip_header.hlen*4
         DO

          (* copy packet so that tcp_input can futz with the ip header and tcp header.
             give it a new mbuf, since tcp_input will use m_pullup() to readjust data
             areas.
          *)
          
          IF packet.mh_hdr.mh_next = NIL AND packet.mh_hdr.mh_len <= Mbuf.MHLEN THEN 
            (* just copy small packets wholesale *)
            copy_packet := Mbuf.m_copym(packet,0,Mbuf.M_COPYALL,Mbuf.M_WAIT);
          ELSE
            data := Mbuf.m_copym(packet,ip_hdr_len + tcp_hdr_len, Mbuf.M_COPYALL,Mbuf.M_WAIT);
            copy_packet := Mbuf.m_gethdr(Mbuf.M_WAIT, Mbuf.MT_DATA);
            copy_packet.mh_hdr.mh_len := ip_hdr_len + tcp_hdr_len;
            copy_packet.mh_hdr.mh_next := data;
            Mbuf.m_copydata(packet,0,ip_hdr_len + tcp_hdr_len,copy_packet);
          END;

          WITH copypkt = Mbuf.Array(copy_packet),
               copy_ip_header = VIEW(copypkt^,IpPktFormat.T),
               copy_ip_hdr_len = copy_ip_header.hlen*4
           DO

            (* now we have to mimic what osf/1 ip_input does to the ip header. *)

            (* Convert fields to host representation. 
               netinet/ip.c: line 302. 
            *)
            copy_ip_header.tot_len := Net.htons(ip_header.tot_len);
            copy_ip_header.id      := Net.htons(ip_header.id);
            copy_ip_header.frag_off:= Net.htons(ip_header.frag_off);

            (* Adjust ip.tot_len to not reflect header.
               netinet/ip.c: line 480 or 513. 
            *)
            DEC(copy_ip_header.tot_len,copy_ip_hdr_len);


            IF debug AND debug_level # Net.oLevel.NODEBUG THEN
              Net.Debug(debug_level, Net.oLevel.DEBUG,"TcpOsf = " & 
                Fmt.Int(copy_ip_header.tot_len) &  " " & 
                Fmt.Int(copy_ip_header.id) &  " " & 
              Fmt.Unsigned(copy_ip_header.frag_off) & "\n" );
            END;
            IF timing THEN 
              Spy.Exit(recvtimer); 
              (* NOT IN IX86_SPIN
              WITH timer = VIEW(packet.mh_hdr.mh_union[0],INTEGER),
                   copy_timer = VIEW(copy_packet.mh_hdr.mh_union[0],INTEGER),
                   copy_mark  = VIEW(copy_packet.mh_hdr.mh_union[1],INTEGER),
                   stop = SAL.Timestamp()
               DO
                Spy.Hit(dataflow,timer,stop);
                timer := stop;
                copy_timer := stop;
                copy_mark  := 16_deadc0dedeadc0de;
              END;
              *)
              Spy.Enter(tcptimer);
            END;

            OsfNet.TcpInput(copy_packet,copy_ip_hdr_len);

            IF timing THEN Spy.Exit(tcptimer); END;
            IF debug AND debug_level # Net.oLevel.NODEBUG THEN
              Net.Debug(debug_level, Net.oLevel.DEBUG,"TcpOsf returned from tcp_input\n");
            END;

          END;
    END;
    RETURN FALSE;
  END TcpInputPA;

PROCEDURE PacketSend(
	READONLY packet	: Mbuf.T;
                 rt     : REFANY) = 
  CONST
    header_len = BYTESIZE(IpPktFormat.Header);
  VAR 
    size   : CARDINAL;
    s      : SocketAddr.T;
    ro     : IpRoute.T;
  BEGIN
    (* XXX in the future we are going to interpret the route informatin
       passed to us from the osf tcp implementation.
    *)
    IF timing THEN Spy.Enter(ipsendtimer);  END;
    <* ASSERT rt = NIL *>

    ro := rt;

    (* check if fragmentation is required *)
    (* XXX: device dependent MTU.  which device we send over depends 
       on which route we are taking.
    *)
    
    (* XXX emergency fix. VIEW ALIGNMENT CHECKED FAILED.
    WITH flags = VIEW(packet.mh_hdr.mh_flags, Mbuf.MbufFlagSet)
     DO
      IF Mbuf.MbufFlags.PKTHDR IN flags THEN
        size := MbufPublic.GetPktHdrLen(packet);
      ELSE
        size := Mbuf.total_length(packet) + header_len;
      END;
    END;
    *)
    IF Word.And(packet.mh_hdr.mh_flags,Mbuf.M_PKTHDR) = Mbuf.M_PKTHDR THEN
      size := MbufPublic.GetPktHdrLen(packet);
    ELSE
      size := Mbuf.m_length(packet) + header_len;
    END;

    WITH header_buf = Mbuf.Array(packet),
         header = VIEW(header_buf^, IpPktFormat.T)
      DO
       (* go grab a route for the ip address *)
       IF ro = NIL THEN ro := IpRoute.Lookup(header.daddr); END;
       IF size <= ro.dev.mtu() THEN
         s.sa_family := SocketRep.AF_INET;
         s.sa_len    := BYTESIZE(SocketAddr.T); (* required by the networking code to be set *)

         WITH ipdaddr = VIEW(s.sa_data,IpPktFormat.AddressArray) DO
           IF IpRoute.Status.Gateway IN ro.status THEN 
             ipdaddr := VIEW(ro.dst,IpPktFormat.AddressArray);
           ELSE
             ipdaddr := VIEW(header.daddr,IpPktFormat.AddressArray);
           END;
         END;

         (* set the ip header length to the header size / 4*)
         header.hlen := header_len DIV 4; (* we are not setting ip options *)
         (* set the version number *)
         header.vers := 4; (* version compatibility *)
         (* set total packet length and checksum *)
         header.tot_len := Net.htons(size);
         (* compute checksum for the ip header packet *)
         header.check := 0;
         header.check := Net.checksum(header_buf^,header_len,0);
         (* header.check := Net.htons(header.check); *)
         IF timing THEN Spy.Exit(ipsendtimer);  END;
         EVAL ro.PacketSend(ro.dev, packet, s, NIL);
       ELSE
         IO.Put("TcpOsf.PacketSend PANIC.\n");
         <* ASSERT FALSE *>
       END;
     END;
  END PacketSend;

PROCEDURE Mss(VAR tp: OsfNet.tcpcbT; <* UNUSED *> offer: Ctypes.unsigned_short):Ctypes.unsigned_short = 
  CONST (* XXX myMss = 1440; *)
    myMss = 1024;
  BEGIN
    (* don't shrink max seg below 32 bytes *)
    tp.t_maxseg := myMss; (* MAX(32,MIN(offer,myMss)); *)
    RETURN (tp.t_maxseg);
  END Mss;

PROCEDURE Init() = 

  PROCEDURE Uninstall(event : OsfNet.PortHandlerT) = 
    BEGIN
    TRY 
      Dispatcher.Uninstall(Dispatcher.GetOriginalHandler(event));
    EXCEPT
    | Dispatcher.Error(ec) => 
      CASE ec OF
      | Dispatcher.ErrorCode.InvalidProcedure =>
        IO.Put("TcpOsf invalid procedure uninstalled.\n");
      ELSE
        IO.Put("TcpOsf dispatcher install error.\n");
      END;
    END;
    END Uninstall; 


  PROCEDURE Install(event: OsfNet.PortHandlerT; handler : OsfNet.PortHandlerT) =
    BEGIN
    TRY 
      EVAL Dispatcher.InstallHandler(event, NIL, handler);
    EXCEPT
    | Dispatcher.Error(ec) => 
      CASE ec OF
      | Dispatcher.ErrorCode.InvalidProcedure =>
        IO.Put("TcpOsf invalid procedure uninstalled.\n");
      ELSE
        IO.Put("TcpOsf dispatcher install error.\n");
      END;
    END;
    END Install;

  BEGIN
    (* Hack to make the PacketSend event accessible in the dispatcher *)
    TRY
      DispatcherPrivate.KeepStub(PacketSend);
    EXCEPT
    | Dispatcher.Error =>
      IO.PutError("TcpOsf dispatcher error: can't keep stub\n");
    END;

    dead.table := NEW(TcpOsfTbl.Default).init();
    dead.mutex := NEW(MUTEX);
    portTable.table := NEW(TcpOsfTbl.Default).init();
    portTable.mutex := NEW(MUTEX);

    (* uninstall default handler *)
    Uninstall(OsfNet.AddTcpPortHandler);
    Install(OsfNet.AddTcpPortHandler, AddTcpPortHandler);

    (* uninstall default handler *)
    Uninstall(OsfNet.DelTcpPortHandler);
    Install(OsfNet.DelTcpPortHandler, DelTcpPortHandler);    

    (* set the ip_output variable so that the tcp C code can make calls out *)
    OsfNet.SetTcpIpOutputUpcall(PacketSend);
    
    (* set the function that computes the mss value *)
    OsfNet.SetTcpMss(Mss);

    IF timing THEN
      recvtimer := Spy.Create("TcpOsfInput");
      tcptimer  := Spy.Create("tcp_input");
      dataflow  := Spy.Create("IP.PA -> TCPOSF.PA");
      ipsendtimer := Spy.Create("TcpOsf.PacketSend (ip)");
    END;

    (* IO.Put("TcpOsf module initialized.\n"); *)
  END Init;

BEGIN
END TcpOsf.

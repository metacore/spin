(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 09-Dec-97  David Becker at the University of Washington
 *	Fixed PacketSend to set correct pkt header length (for vx0)
 * 06-Oct-96  Robert Grimm (rgrimm) at the University of Washington
 *      added call to DispatcherPrivate.KeepStub in Init proc
 *      to ensure that the PacketSend event can be handled.
 *
 * 19-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	make guards FUNCTIONAL
 *
 * 13-Jun-96  Marc Fiuczynski (mef) at the University of Washington
 *	Cleanup.  Remove bogus handlers.
 *
 * 20-May-96  Przemek Pardyak (pardy) at the University of Washington
 *	Changed the dispatcher interface.
 *
 * 20-Apr-96  Wilson Hsieh (whsieh) at the University of Washington
 *	PROCANY change
 *
 *)

MODULE UdpOsf;

IMPORT Net, Mbuf, MbufPublic, OsfNet, ParseParams;
IMPORT IO, Ctypes, Fmt;
IMPORT Dispatcher, DispatcherPrivate;
IMPORT IpGen, Udp, IpPktFormat, OsfNetEmulation, Port, Key, UdpOsfTbl;
IMPORT Word;

PROCEDURE Run(pp: ParseParams.T): BOOLEAN =
  BEGIN
    pp.reset();
    TRY
      pp.skipNext(); (* udposf *)
      IF pp.testNext("-debug") THEN
        debug_level := Net.MapDebug(pp.getNextInt());
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
  debug = FALSE;
<*UNUSED*>
CONST  
  timing = OsfNetEmulation.timing;
  
VAR
  debug_level : Net.Level := Net.oLevel.WARNING;

TYPE 
  portTableT = RECORD
    table: UdpOsfTbl.Default;
    mutex : MUTEX;
  END;

VAR
  portTable : portTableT;

PROCEDURE AddUdpPortHandler(port:Ctypes.unsigned_short) =
  VAR 
    portEntry  : Port.T;
    key   : Key.T;
  BEGIN
    LOCK portTable.mutex DO
      key := port;

      IF debug AND debug_level # Net.oLevel.NODEBUG THEN
        Net.Debug(debug_level, Net.oLevel.WARNING,"UdpOsf AddUdpPortHandler on " &
          Fmt.Int(Net.nstoh(port)));
      END;

      IF portTable.table.get(key,portEntry) = FALSE THEN
        portEntry := NEW(Port.T);
        portEntry.count := 0;
        portEntry.port := port;
        portEntry.binding := 
            Udp.InstallWithClosure(Udp.PacketArrived,
                                          UdpInputWC,UdpInputPA,
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
  END AddUdpPortHandler;

PROCEDURE DelUdpPortHandler(port:Ctypes.unsigned_short) =
  VAR
    portEntry : Port.T;
    key : Key.T;
  BEGIN
    LOCK portTable.mutex DO
      key := port;

      IF debug AND debug_level # Net.oLevel.NODEBUG THEN
        Net.Debug(debug_level, Net.oLevel.WARNING,"UdpOsf DelUdpPortHandler for " & 
          Fmt.Int(Net.nstoh(port)));
      END;

      IF portTable.table.get(key,portEntry) = TRUE THEN
        IF portEntry.count = 1 THEN 
          EVAL portTable.table.delete(key,portEntry);
          Udp.Uninstall(portEntry.binding);
          IF debug AND debug_level # Net.oLevel.NODEBUG THEN
            Net.Debug(debug_level, Net.oLevel.WARNING," and event handler uninstalled.");
          END;
        ELSE
          DEC(portEntry.count);
        END
      END;

      IF debug AND debug_level # Net.oLevel.NODEBUG THEN
        Net.Debug(debug_level, Net.oLevel.WARNING,"\n");
      END;

    END;
  END DelUdpPortHandler;

CONST
  udp_hdr_len    = BYTESIZE(T);

FUNCTIONAL
PROCEDURE UdpInputWC(
    closure: REFANY;
    <* UNUSED *> packet: Mbuf.T; 
    curr: Mbuf.T; 
    offset: CARDINAL):BOOLEAN = 
  BEGIN
    WITH UdpHeaderBuf = SUBARRAY(Mbuf.Array(curr)^,offset,udp_hdr_len),
         UdpHeader = VIEW(UdpHeaderBuf,T),
         port = NARROW(closure,Port.T).port
       DO
      RETURN UdpHeader.dport = port;
    END;
  END UdpInputWC;

PROCEDURE UdpInputPA(
    <* UNUSED *> closure: REFANY; 
    packet: Mbuf.T;
    <* UNUSED *> curr : Mbuf.T; 
    <* UNUSED *> offset: CARDINAL):BOOLEAN = 
  VAR
    data, copy_packet : Mbuf.T;
  BEGIN

    (* #ifdef debug_level != NODEBUG *)
    IF debug AND debug_level # Net.oLevel.NODEBUG THEN
      Net.Debug(debug_level, Net.oLevel.INFO,"UdpOsf PA ")
    END;

    WITH origpkt = Mbuf.Array(packet),
         ip_header = VIEW(origpkt^,IpPktFormat.T),
         ip_hdr_len = ip_header.hlen*4
         DO

          (* copy packet so that Udp_input can futz with the ip header and Udp header.
             give it a new mbuf, since Udp_input will use m_pullup() to readjust data
             areas.
          *)
          
          data := Mbuf.m_copym(packet,ip_hdr_len + udp_hdr_len, Mbuf.M_COPYALL,Mbuf.M_WAIT);
          copy_packet := Mbuf.m_gethdr(Mbuf.M_WAIT, Mbuf.MT_DATA);
          copy_packet.mh_hdr.mh_len := ip_hdr_len + udp_hdr_len;
          copy_packet.mh_hdr.mh_next := data;
          MbufPublic.SetPktHdrLen(copy_packet, Mbuf.m_length(copy_packet));

          (* #ifdef debug_level != NODEBUG *)
          IF debug AND debug_level # Net.oLevel.NODEBUG THEN
            Net.Debug(debug_level, Net.oLevel.DEBUG,"UdpOsf m->m_len = " & 
              Fmt.Int(copy_packet.mh_hdr.mh_len) & "+" & 
              Fmt.Int(data.mh_hdr.mh_len) & "\n" );
          END;

          Mbuf.m_copydata(packet,0,ip_hdr_len + udp_hdr_len,copy_packet);

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
              Net.Debug(debug_level, Net.oLevel.DEBUG,"UdpOsf m->m_len = " & 
                Fmt.Int(copy_packet.mh_hdr.mh_len) &  " " & 
              Fmt.Unsigned(copy_packet.mh_hdr.mh_flags) & "\n" );
            END;

            OsfNet.UdpInput(copy_packet,copy_ip_hdr_len);
            IF debug AND debug_level # Net.oLevel.NODEBUG THEN
              Net.Debug(debug_level, Net.oLevel.DEBUG,"UdpOsf returned from Udp_input");
            END;

          END;
    END;
    RETURN FALSE;
  END UdpInputPA;

PROCEDURE PacketSend(
	READONLY packet	: Mbuf.T;
                 rt     : REFANY) = 
  CONST
    header_len = BYTESIZE(IpPktFormat.Header);
  VAR
    ip: IpPktFormat.T;
  BEGIN
    <* ASSERT rt = NIL *>
    WITH headerBuf = Mbuf.Array(packet)
     DO
      ip := VIEW(headerBuf^,IpPktFormat.T)
    END;
    IF Word.And(packet.mh_hdr.mh_flags,Mbuf.M_PKTHDR) # 0 THEN
      MbufPublic.SetPktHdrLen(packet,MbufPublic.GetPktHdrLen(packet)-header_len );
    END;
    INC(packet.mh_hdr.mh_data,header_len);
    DEC(packet.mh_hdr.mh_len,header_len);
    IpGen.PacketSend(ip,packet,rt);
  END PacketSend;

PROCEDURE Init() = 

  PROCEDURE Uninstall(event : OsfNet.PortHandlerT) = 
    BEGIN
    TRY 
      Dispatcher.Uninstall(Dispatcher.GetOriginalHandler(event));
    EXCEPT
    | Dispatcher.Error(ec) => 
      CASE ec OF
      | Dispatcher.ErrorCode.InvalidProcedure =>
        IO.Put("UdpOsf invalid procedure uninstalled.\n");
      ELSE
        IO.Put("UdpOsf dispatcher install error.\n");
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
        IO.Put("UdpOsf invalid procedure uninstalled.\n");
      ELSE
        IO.Put("UdpOsf dispatcher install error.\n");
      END;
    END;
    END Install;

  BEGIN
    (* Hack to make the PacketSend event accessible in the dispatcher *)
    TRY
      DispatcherPrivate.KeepStub(PacketSend);
    EXCEPT
    | Dispatcher.Error =>
      IO.PutError("UdpOsf dispatcher error: can't keep stub\n");
    END;

    portTable.table := NEW(UdpOsfTbl.Default).init();
    portTable.mutex := NEW(MUTEX);

    (* uninstall default handler *)
    Uninstall(OsfNet.AddUdpPortHandler);
    Install(OsfNet.AddUdpPortHandler, AddUdpPortHandler);

    (* uninstall default handler *)
    Uninstall(OsfNet.DelUdpPortHandler);
    Install(OsfNet.DelUdpPortHandler, DelUdpPortHandler);    

    (* set the ip_output variable so that the Udp C code can make calls out *)
    OsfNet.SetUdpIpOutputUpcall(PacketSend);
    (* IO.Put("UdpOsf module initialized.\n"); *)
  END Init;

BEGIN
END UdpOsf.

(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 07-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Converted to new spin shell commands.
 *
 *
 *)

MODULE TcpGen;

IMPORT IO;
IMPORT Ctypes;
(* IMPORT Fmt; *)
IMPORT IpGen;
IMPORT IpPktFormat;
IMPORT Mbuf;
IMPORT Net;
(* IMPORT TcpDefault; *)
IMPORT TcpPktFormat;
(* IMPORT Word; *)
IMPORT Spy;
(* IMPORT SAL; *)

IMPORT ParseParams;

PROCEDURE Run(pp: ParseParams.T): BOOLEAN =
  BEGIN
    pp.reset();
    TRY
      pp.skipNext(); (* ipdefault *)
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
  timing = FALSE;

VAR
  send_timer: Spy.T;
  debug_level : Net.Level := Net.oLevel.NODEBUG;

PROCEDURE PacketSend(
	READONLY ip	: IpPktFormat.Header;
	READONLY tcp	: TcpPktFormat.Header;
	READONLY data	: Mbuf.T) =
  CONST 
    header_len = BYTESIZE(TcpPktFormat.Header);
    (* ipovl_len  = BYTESIZE(IpPktFormat.OverlayHeader); *)
    ip_hdr_len = BYTESIZE(IpPktFormat.Header);
    protocol   = IpPktFormat.IPPROTO_TCP;
  VAR 
    size   : CARDINAL;
    packet : Mbuf.T;
    ipovl_buf : ARRAY [1 .. BYTESIZE(IpPktFormat.OverlayHeader)] OF Net.BYTE;
    ipovl_csum: Ctypes.unsigned_short;
  BEGIN

    IF timing THEN
      Spy.Enter(send_timer);
    END;

    IF debug AND debug_level # Net.oLevel.NODEBUG THEN
      Net.Debug(debug_level, Net.oLevel.INFO,"TcpGen.PacketSend [");
    END;

    (* compute the length of the ip packet *)
    IF ip.tot_len # 0 THEN 
      size := ip.tot_len - ip_hdr_len;
    ELSE
      size :=  Mbuf.m_length (data) + header_len;
    END;


    (* #ifdef debug_level != NODEBUG *)
    IF debug AND debug_level # Net.oLevel.NODEBUG THEN
      Net.Debug(debug_level, Net.oLevel.INFO,"a ");
    END;


    (* grab an mbuf for the TCP  header.  Since we will have to
       prepend link and network layer data as well, we will want to
       leave space at the beginning of the buffer and then use the
       sys/mbuf.h M_PREPEND macro to prepend data.  It is smart enough
       to allocate another mbuf if there is not enough leading space.

       XXX: for now just prepend header by allocating the mbuf.  
    *)
    IF data # NIL THEN
      packet := data; (* XXX don't lose the data pointer in m_prepend *)
      packet := Mbuf.m_prepend(packet,header_len,Mbuf.M_WAIT);
    ELSE
      packet := Mbuf.m_get(Mbuf.M_WAIT, Mbuf.MT_DATA);
      packet.mh_hdr.mh_len := header_len;
      Mbuf.M_ALIGN(packet,header_len);      
    END;

    IF debug AND debug_level # Net.oLevel.NODEBUG THEN
      Net.Debug(debug_level, Net.oLevel.INFO,"b ");
    END;


    (* set the tcp header fields *)
    WITH header_buf = Mbuf.Array(packet) DO
      WITH header = VIEW(header_buf^,TcpPktFormat.NewT) 
       DO
        
        IF debug AND debug_level # Net.oLevel.NODEBUG THEN
          Net.Debug(debug_level, Net.oLevel.INFO,"c ");
        END;

        header.sport := tcp.sport;
        header.dport := tcp.dport;
        header.seq   := tcp.seq;
        header.ack_seq := tcp.ack_seq;

        header.x2 := tcp.x2;
        header.xoff := tcp.xoff;
        header.flags := tcp.flags;

        header.window := tcp.window;
        header.urg_ptr := tcp.urg_ptr;

        IF debug AND debug_level # Net.oLevel.NODEBUG THEN
          Net.Debug(debug_level, Net.oLevel.INFO,"d ");
        END;

        (* cons up ip overlay header for checksum *)
        WITH ipovl = VIEW(ipovl_buf,IpPktFormat.OverlayHeader) DO

          IF debug AND debug_level # Net.oLevel.NODEBUG THEN
            Net.Debug(debug_level, Net.oLevel.INFO,"e ");
          END;

          ipovl.fill[0]:= 0;
          ipovl.fill[1]:= 0;
          ipovl.ih_x1  := 0;
          ipovl.ih_pr  := protocol;
          ipovl.ih_len := Net.htons(size); (* XXX this is odd.  It should be size + ip_hdr_len *)
          ipovl.ih_src := ip.saddr;
          ipovl.ih_dst := ip.daddr;
        END;
        ipovl_csum := Net.checksum(ipovl_buf);

        IF debug AND debug_level # Net.oLevel.NODEBUG THEN
          Net.Debug(debug_level, Net.oLevel.INFO,"f ");
        END;

        (* compute new checksum for tcp packet with ip overlay header *)
        header.check := 0;
        header.check := Mbuf.Checksum(packet,ipovl_csum,size);
      END;
    END;        

    IF timing THEN
      Spy.Exit(send_timer);
    END;

    IF debug AND debug_level # Net.oLevel.NODEBUG THEN
      Net.Debug(debug_level, Net.oLevel.INFO,"[tcp->ip send] ");
    END;

    (* send as ip packet - no options *)
    IpGen.PacketSend(ip, packet);
  END PacketSend;


BEGIN
  IF timing THEN
    send_timer:= Spy.Create("tcp_output");
  END;
END TcpGen. 


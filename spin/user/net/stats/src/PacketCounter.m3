(* 
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 31-May-97  David Becker at the University of Washington
 *      Replaced If interface with NetDev/EtherDev
 *
 * 14-Oct-96 Robert Grimm (rgrimm) at the University of Washington
 *      Added support for loopback counting.
 *
 * 06-Oct-96 Robert Grimm (rgrimm) at the University of Washington
 *      Added support for counting sent packets, the /proc file
 *      system and cleaned up some of the code in the course of it.
 *
 * 02-Oct-96 Robert Grimm (rgrimm) at the University of Washington
 *      Initial packet counter written in one day...
 *      (as my first Modula-3 and as my first SPIN program)
 *)

MODULE PacketCounter;
IMPORT Ctypes;
IMPORT Dispatcher;
IMPORT Error;
IMPORT Ether; (* event we are handling *)
IMPORT EtherGen; (* event we are handling *)
IMPORT Fmt;
IMPORT Icmp; (* event we are handling *)
IMPORT IcmpGen; (* event we are handling *)
IMPORT IcmpPktFormat;
IMPORT InfoFile;
IMPORT Ip; (* event we are handling *)
IMPORT IpGen; (* event we are handling *)
IMPORT IpPktFormat;
IMPORT IpRoute;
IMPORT IO;
IMPORT Loopback; (* event we are handling *)
IMPORT LoopbackTunnel; (* event we are handling *)
IMPORT Mbuf;
IMPORT ParseParams;
IMPORT SocketAddr;
IMPORT Tcp; (* event we are handling *)
IMPORT TcpGen; (* event we are handling *)
IMPORT TcpOsf; (* event we are handling *)
IMPORT TcpPktFormat;
IMPORT Udp; (* event we are handling *)
IMPORT UdpGen; (* event we are handling *)
IMPORT UdpOsf; (* event we are handling *)
IMPORT UdpPktFormat;
IMPORT Wr;
IMPORT NetDev;

(* ----------------------------------------------------------------------
 *                                  Types
 *)

TYPE
  TcpIpCounter =
    REF RECORD tcp, ip: INTEGER END;

(* ----------------------------------------------------------------------
 *                 Counter variables and initialization flag
 *)

VAR
  etherInCounter  := NEW( REF INTEGER );
  loopInCounter   := NEW( REF INTEGER );
  ipInCounter     := NEW( REF INTEGER );
  icmpInCounter   := NEW( REF INTEGER );
  udpInCounter    := NEW( REF INTEGER );
  tcpInCounter    := NEW( REF INTEGER );
  etherOutCounter := NEW( REF INTEGER );
  loopOutCounter  := NEW( REF INTEGER );
  icmpOutCounter  := NEW( REF INTEGER );
  udpOutCounter   := NEW( REF INTEGER );

  (* ip and tcp packets sent counters are kept in one record
   * so they can be both incremented for the TcpOsf.PacketSend
   * event handler
   *)

  tcpIpOutCounter := NEW( TcpIpCounter );

  initialized  := FALSE;


(* ----------------------------------------------------------------------
 *                          Shell Command Support
 *)

(* Run Shell Command *)

PROCEDURE Run(pp: ParseParams.T): BOOLEAN =
  BEGIN
    pp.reset();
    TRY
      pp.skipNext(); (* netstats *)
      IF pp.testNext("init") THEN
        Init(FALSE);
      ELSIF pp.testNext("reset") THEN
        Reset();
      ELSIF pp.testNext("dump") THEN
        Dump();
      ELSE
        Usage();
      END;
    EXCEPT
      ParseParams.Error => Usage();
    END;
    RETURN TRUE;
  END Run;

(* Print usage for shell command *)

PROCEDURE Usage() = 
  BEGIN
    IO.Put("Usage: " & CommandName & ":" & CommandHelp & "\n");
  END Usage;


(* ----------------------------------------------------------------------
 *                               Event Handling 
 *)

(* "Generic" event handler for PacketArrived events *)

PROCEDURE InCount(closure : REFANY; <* UNUSED *> packet, curr : Mbuf.T;
                  <* UNUSED *> offset : CARDINAL) : BOOLEAN =
  BEGIN
    WITH count = NARROW( closure, REF INTEGER ) DO
      INC(count^);
    END;

    RETURN FALSE;
  END InCount;

(* Ether and Loopback Send Counter *)

PROCEDURE EtherLoopOutCount( closure : REFANY; <* UNUSED *> dev: NetDev.T;
                         <* UNUSED *> mbuf : Mbuf.T;
                         <* UNUSED *> VAR s : SocketAddr.T;
                         <* UNUSED *> rte : ADDRESS ) : 
                       Ctypes.int =
  BEGIN
    WITH count = NARROW( closure, REF INTEGER ) DO
      INC(count^);
    END;

    RETURN 0;
  END EtherLoopOutCount;

(* Ip Send Counter *)

PROCEDURE IpOutCount( closure : REFANY;
                      <* UNUSED *> READONLY ip : IpPktFormat.Header;
                      <* UNUSED *> READONLY data : Mbuf.T;
                      <* UNUSED *> rt : IpRoute.T ) =
  BEGIN
    WITH count = NARROW( closure, TcpIpCounter ) DO
      INC(count.ip);
    END;
  END IpOutCount;

(* Icmp Send Counter *)

PROCEDURE IcmpOutCount( closure : REFANY;
                        <* UNUSED *> READONLY ip : IpPktFormat.Header;
                        <* UNUSED *> READONLY icmp : IcmpPktFormat.Header;
                        <* UNUSED *> READONLY data : Mbuf.T ) =
  BEGIN
    WITH count = NARROW( closure, REF INTEGER ) DO
      INC(count^);
    END;
  END IcmpOutCount;

(* Udp Send Counter *)

PROCEDURE UdpOutCount( closure : REFANY;
                       <* UNUSED *> READONLY ip : IpPktFormat.Header;
                       <* UNUSED *> READONLY udp : UdpPktFormat.Header;
                       <* UNUSED *> READONLY data : Mbuf.T;
                       <* UNUSED *> dochecksum : BOOLEAN ) =
  BEGIN
    WITH count = NARROW(closure, REF INTEGER ) DO
      INC(count^);
    END;
  END UdpOutCount;

(* OSF Udp Send Counter *)

PROCEDURE UdpOsfOutCount( closure : REFANY;
                          <* UNUSED *> READONLY packet : Mbuf.T;
                          <* UNUSED *> rt : REFANY ) =
  BEGIN
    WITH count = NARROW(closure, REF INTEGER ) DO
      INC(count^);
    END;
  END UdpOsfOutCount;

(* Tcp Send Counter *)

PROCEDURE TcpOutCount( closure : REFANY;
                       <* UNUSED *> READONLY ip : IpPktFormat.Header;
                       <* UNUSED *> READONLY tcp : TcpPktFormat.Header;
                       <* UNUSED *> READONLY data : Mbuf.T ) =
  BEGIN
    WITH count = NARROW(closure, TcpIpCounter ) DO
      INC(count.tcp);
      END;
  END TcpOutCount;

(* OSF Tcp Send Counter *)

PROCEDURE TcpOsfOutCount( closure : REFANY;
                          <* UNUSED *> READONLY packet : Mbuf.T;
                          <* UNUSED *> rt : REFANY ) =

  BEGIN
    WITH count = NARROW(closure, TcpIpCounter ) DO
      INC(count.ip);
      INC(count.tcp);
      END;
  END TcpOsfOutCount;

(* Install the event handlers *)

PROCEDURE InstallCounter( event : PROCANY; handler : PROCANY;
                          closure : REFANY ) =
  VAR
    binding : Dispatcher.Binding;
  BEGIN
    TRY
      binding := Dispatcher.InstallHandler( event,NIL,handler,NIL,closure );
    EXCEPT
    | Dispatcher.Error(ec) => 
      CASE ec OF
      | Dispatcher.ErrorCode.InvalidProcedure =>
        IO.PutError("PacketCounter invalid procedure installed.\n");
      ELSE
        IO.PutError("PacketCounter dispatcher install error.\n");
      END;
    END;
  END InstallCounter;

(* Initialize the counters *)

PROCEDURE Init(verbose:BOOLEAN) = 
  BEGIN
    IF initialized THEN
      Reset();
      RETURN;
    END;

    (* Install the counter handlers on all packet arrived and sent events *)

    InstallCounter( Ether.PacketArrived,    InCount,        etherInCounter  );
    InstallCounter( Loopback.PacketArrived, InCount,        loopInCounter   );
    InstallCounter( Ip.PacketArrived,       InCount,        ipInCounter     );
    InstallCounter( Icmp.PacketArrived,     InCount,        icmpInCounter   );
    InstallCounter( Udp.PacketArrived,      InCount,        udpInCounter    );
    InstallCounter( Tcp.PacketArrived,      InCount,        tcpInCounter    );
    InstallCounter( EtherGen.PacketSend,   EtherLoopOutCount,etherOutCounter);
    InstallCounter( LoopbackTunnel.PacketSend, EtherLoopOutCount,
                    loopOutCounter);
    InstallCounter( IpGen.PacketSend,       IpOutCount,     tcpIpOutCounter );
    InstallCounter( IcmpGen.PacketSend,     IcmpOutCount,   icmpOutCounter  );
    InstallCounter( UdpGen.PacketSend,      UdpOutCount,    udpOutCounter   );
    InstallCounter( UdpOsf.PacketSend,      UdpOsfOutCount, udpOutCounter   );
    InstallCounter( TcpGen.PacketSend,      TcpOutCount,    tcpIpOutCounter );
    InstallCounter( TcpOsf.PacketSend,      TcpOsfOutCount, tcpIpOutCounter );

    (* Done *)

    initialized := TRUE;
    IF verbose THEN IO.Put("PacketCounter module initialized.\n"); END;
  END Init;


(* ----------------------------------------------------------------------
 *                         Reset and Print Counters
 *)

(* Reset all counters *)

PROCEDURE Reset() =
  BEGIN
    etherInCounter^     := 0;
    loopInCounter^      := 0;
    ipInCounter^        := 0;
    icmpInCounter^      := 0;
    udpInCounter^       := 0;
    tcpInCounter^       := 0;
    etherOutCounter^    := 0;
    loopOutCounter^     := 0;
    tcpIpOutCounter.ip  := 0;
    icmpOutCounter^     := 0;
    udpOutCounter^      := 0;
    tcpIpOutCounter.tcp := 0;
  END Reset;

(* Dump counters to shell console or to proc file *)

PROCEDURE Dump( wr : Wr.T := NIL ) =
  BEGIN
    IO.Put("\n", wr);
    IO.Put("                 Arrived            Sent   \n", wr);
    IO.Put("               ----------        ----------\n", wr);
    IO.Put(" Ether    :     " & Fmt.Pad(Fmt.Int(etherInCounter^    ), 8), wr);
    IO.Put("          "       & Fmt.Pad(Fmt.Int(etherOutCounter^   ), 8), wr);
    IO.Put("\n", wr);
    IO.Put(" Loopback :     " & Fmt.Pad(Fmt.Int(loopInCounter^     ), 8), wr);
    IO.Put("          "       & Fmt.Pad(Fmt.Int(loopOutCounter^    ), 8), wr);
    IO.Put("\n", wr);
    IO.Put(" IP       :     " & Fmt.Pad(Fmt.Int(ipInCounter^       ), 8), wr);
    IO.Put("          "       & Fmt.Pad(Fmt.Int(tcpIpOutCounter.ip ), 8), wr);
    IO.Put("\n", wr);
    IO.Put(" ICMP     :     " & Fmt.Pad(Fmt.Int(icmpInCounter^     ), 8), wr);
    IO.Put("          "       & Fmt.Pad(Fmt.Int(icmpOutCounter^    ), 8), wr);
    IO.Put("\n", wr);
    IO.Put(" UDP      :     " & Fmt.Pad(Fmt.Int(udpInCounter^      ), 8), wr);
    IO.Put("          "       & Fmt.Pad(Fmt.Int(udpOutCounter^     ), 8), wr);
    IO.Put("\n", wr);
    IO.Put(" TCP      :     " & Fmt.Pad(Fmt.Int(tcpInCounter^      ), 8), wr);
    IO.Put("          "       & Fmt.Pad(Fmt.Int(tcpIpOutCounter.tcp), 8), wr);
    IO.Put("\n\n", wr);
  END Dump;

(* ----------------------------------------------------------------------
 *                       Module Initialization
 *)

BEGIN
  Reset(); (* needed to correctly initialize counters *)
  
  (* Create /proc info file *)
  TRY
    InfoFile.Create("/proc/netstats", Dump);
  EXCEPT
  | Error.E(e) =>
    BEGIN
      IO.PutError("PacketCounter can't create /proc/netstats file:\n");
      IO.PutError("   " & e.message() & "\n");
    END;
  END;

  (* Init right away, for now *)
  Init(TRUE);
END PacketCounter.

(* ---------------------------------------------------------------------- *)

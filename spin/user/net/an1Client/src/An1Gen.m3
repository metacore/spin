(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 *)

UNSAFE MODULE An1Gen EXPORTS An1Gen, An1GenPrivate;

IMPORT Net;
IMPORT An1Arp;
IMPORT IO;
IMPORT Mbuf;
(* IMPORT Fmt; *)
IMPORT An1PktFormat;
IMPORT IpPktFormat;
IMPORT If;
IMPORT SocketRep;
IMPORT Spy;
IMPORT Ctypes;
IMPORT Talc;
(* DynShell support *)
IMPORT Text,SafeConvert;
IMPORT Dispatcher;
IMPORT SpinShell;
<* UNUSED *> VAR shell := Dispatcher.Install(SpinShell.RunCommand,EC_Guard,RunCommand);

PROCEDURE EC_Guard(argc: INTEGER; READONLY argv: ARRAY OF TEXT): BOOLEAN =
  BEGIN
    RETURN argc >= 1 AND Text.Equal(argv[0], "An1Gen");
  END EC_Guard;

PROCEDURE RunCommand(argc: INTEGER; READONLY argv: ARRAY OF TEXT): BOOLEAN =
  VAR
  BEGIN
    IF argc > 1 AND Text.Equal(argv[1], "debug") THEN
      debug_level := Net.MapDebug(SafeConvert.Atoi(argv[2]));
    ELSE
      IO.Put("An1Gen.RunCommand: no such command ");
      FOR i := 0 TO argc DO
        IO.Put(argv[i]);
      END;
      IO.Put("\n");
    END;
    RETURN TRUE;
  END RunCommand;

VAR
  send_timer : Spy.T;
  debug_level: Net.Level := Net.oLevel.NODEBUG;

EXCEPTION NoValidAn1Address;
PROCEDURE PacketSend( 
  VAR ifp : If.ifnet; 
  mbuf    : Mbuf.T; 
  VAR s   : SocketRep.M3sockaddr;
  <* UNUSED *> rte     : ADDRESS := NIL) : Ctypes.int =
  VAR error : Ctypes.int := 0;

  PROCEDURE Ip() : Ctypes.int RAISES {NoValidAn1Address} = 
    VAR dst, src: An1Arp.T;
        e: An1PktFormat.Header;
        iperror: Ctypes.int := 0;
    BEGIN
      WITH ip_data = Mbuf.Array(mbuf), (* get the ip_data area *)
           ip = VIEW(ip_data^,IpPktFormat.NewT),
           ipdaddr = VIEW(s.sa_data,IpPktFormat.Address)
       DO
        TRY
          (* resolve the source ip address *)
          An1Arp.Resolve(Net.htons(An1PktFormat.AN1TYPE_IP),ip.saddr,src);

          TRY
            (* resolve the destination ip address and if we succeed send the packet *)
            An1Arp.Resolve(Net.htons(An1PktFormat.AN1TYPE_IP),ipdaddr^,dst);

            e.dhost := dst.hardwareAddress;
            e.shost := src.hardwareAddress;
            e.type := Net.htons(An1PktFormat.AN1TYPE_IP);
            (* #ifdef debug_level != NODEBUG *)
            IF debug_level # Net.oLevel.NODEBUG THEN
              Net.Debug(debug_level,Net.oLevel.INFO,"An1Gen.Output an1_output");
            END;

            WITH header = Mbuf.m_prepend(mbuf,BYTESIZE(An1PktFormat.Header),Mbuf.M_WAIT), 
                 an1_data = Mbuf.Array(header), (* get the an1_data area *)
                 an1_hdr  = VIEW(an1_data^,An1PktFormat.NewT) (* VIEW *)
             DO
              an1_hdr^ := e;
              iperror :=  Talc.Output(ifp,header,s);
            END;
          EXCEPT
          | An1Arp.NotResolved, An1Arp.NotInCache=>
            TRY
              (* we don't know the destination an1 address,
                 so we try to look it up.  Once the packet gets
                 resolved we'll send out the packet.
              *)
              An1Arp.Lookup(ifp,mbuf,s);
            EXCEPT
            | An1Arp.NotResolved =>
              IO.Put("PacketSend could not resolve ip destination address.\n");
              RAISE NoValidAn1Address;
            END;
          END;

        EXCEPT
        | An1Arp.NotResolved =>
          IO.Put("PacketSend could not resolve ip source address in local arp cache.\n");
          RAISE NoValidAn1Address;
        | An1Arp.NotInCache =>
          (*
          IO.Put("PacketSend did not find ip source address in arp cache.\n");
          *)
          RAISE NoValidAn1Address;
        END;
        RETURN iperror;
      END;
    END Ip;

  PROCEDURE Raw(): Ctypes.int = 
    BEGIN
      WITH header     = Mbuf.M_PREPEND(mbuf,BYTESIZE(An1PktFormat.Header),Mbuf.M_WAIT), 
           eth_data   = Mbuf.Array(header), (* get the an1_data area *)
           eth_out    = VIEW(eth_data^,An1PktFormat.NewT),
           eth_header = VIEW(s.sa_data,An1PktFormat.NewT)
       DO
        eth_out^ := eth_header^;
        RETURN Talc.Output(ifp,header,s);
        (* EVAL An1GenPrivate.an1_output(m := header); *)
      END;
    END Raw;

  BEGIN
    (* #ifdef debug_level != NODEBUG *)
    IF debug_level # Net.oLevel.NODEBUG THEN
      Net.Debug(debug_level,Net.oLevel.INFO,"An1Gen.PacketSend ");
    END;
    
    Spy.Enter(send_timer);
    CASE s.sa_family OF 
    | SocketRep.AF_INET => (* IP packet assumes that mbuf starts with ip header. *)
      TRY
        error := Ip();
      EXCEPT
      | NoValidAn1Address =>
        Mbuf.m_freem(mbuf);
      END;
    | SocketRep.AF_UNSPEC => (* assumes header starts with network layer information *)
      error :=  Raw();
    ELSE
      IO.Put("Address Family not supported.\n");
    END;
    Spy.Exit(send_timer);
    RETURN error;
  END PacketSend; 

BEGIN
  send_timer        := Spy.Create("an1_output");
  (* reroute output packets to this function *)
  an1_output_upcall := PacketSend;
END An1Gen. 

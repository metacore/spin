(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 *
 * 18-Dec-95  Przemek Pardyak (pardy) at the University of Washington
 *	Added Help procedure to conform to the new shell interface.
 *
 *)

(* Untrusted *)
MODULE AtmClassification;
IMPORT AtmTrusted; (* Installing handler *)
IMPORT Atm; (* event we are handling *)
IMPORT IpPktFormat;
IMPORT Ip;    (* event we are raising  *) 
(* IMPORT AtmPktFormat; reusing default stuff *)
(* IMPORT AtmGen;*)
IMPORT Net;
IMPORT Mbuf;
IMPORT Spy,SAL;

(* m3 libraries *)
IMPORT IO, Fmt;
(* IMPORT Ctypes; *)

(* DynShell support *)
IMPORT Text,SafeConvert;
IMPORT SpinShell;
<* UNUSED *> VAR shell := SpinShell.InstallCommand(EC_Guard,RunCommand,Help);

PROCEDURE EC_Guard(<* UNUSED *> argc: CARDINAL; READONLY argv: ARRAY OF TEXT): BOOLEAN =
  BEGIN
    RETURN Text.Equal(argv[0], "AtmClassification");
  END EC_Guard;

PROCEDURE RunCommand(argc: CARDINAL; READONLY argv: ARRAY OF TEXT): BOOLEAN =
  VAR
  BEGIN

   IF Text.Equal(argv[1], "debug") THEN
     debug_level := Net.MapDebug(SafeConvert.Atoi(argv[2]));
   ELSE
     IO.Put("AtmClassification.RunCommand: no such command ");
     FOR i := 0 TO argc DO
       IO.Put(argv[i]);
     END;
     IO.Put("\n");
   END;
   RETURN TRUE;
  END RunCommand;

PROCEDURE Help () =
  BEGIN
  END Help;

VAR
  ip_packet_counter   : INTEGER := 0;
  ip: REFANY;
  iptimer: Spy.T;
  debug_level : Net.Level := Net.oLevel.NODEBUG;
  dataflow : Spy.T;


PROCEDURE WhenClause_IP(<* UNUSED *> READONLY packet: Mbuf.T; <* UNUSED *> READONLY payload: Atm.T):BOOLEAN =
  BEGIN
    (* WITH atm = VIEW(payload^,T) DO *)
    (* 
       WITH atm = VIEW(payload^,NewT) DO
       IF atm.type = AtmPktFormat.ATMTYPE_IP THEN
    *)
    Spy.Enter(iptimer);
    RETURN TRUE;
    (*
      END;
      RETURN FALSE;
      END;
    *)
  END WhenClause_IP;

PROCEDURE PacketArrived_IP(READONLY packet: Mbuf.T; <* UNUSED *> READONLY payload: Atm.T):BOOLEAN = 
  CONST hdr_len = BYTESIZE(IpPktFormat.Header);
  VAR m: Mbuf.T;
  BEGIN
    (* #ifdef debug_level != NODEBUG *)
    IF debug_level # Net.oLevel.NODEBUG THEN
      Net.Debug(debug_level,Net.oLevel.INFO,"[atmIP]");
    END;

    WITH stop = SAL.Timestamp() DO
      Spy.Hit(dataflow,packet.mh_hdr.mh_timer[0],stop);
      packet.mh_hdr.mh_timer[0] := stop;
    END;

    (* check if there are enough bytes in the ip header *)
    WITH packet_buffer = Mbuf.Array(packet),
         mlen = NUMBER(packet_buffer^) 
     DO

      IF debug_level # Net.oLevel.NODEBUG THEN
        Net.Debug(debug_level,Net.oLevel.INFO," mlen = " & Fmt.Int(mlen) & " ");
      END;

      (* special case when ip packet is not in first mbuf *)
      IF mlen < hdr_len THEN
        (* make our own copy so we can manipulate it. *)
        m := Mbuf.m_copym(packet,0,Mbuf.M_COPYALL,Mbuf.M_WAIT); (* XXX *)
        IF m = NIL THEN RETURN FALSE END;

        IF debug_level # Net.oLevel.NODEBUG THEN
          Net.Debug(debug_level,Net.oLevel.DEBUG," m_copym  ");
        END;


        (* pullup the bytes to make a complete packet header *)
        m := Mbuf.m_pullup(m, hdr_len);
        IF m = NIL THEN RETURN FALSE END;

        IF debug_level # Net.oLevel.NODEBUG THEN
          Net.Debug(debug_level,Net.oLevel.DEBUG," m_pullup  ");
        END;

        (* check if the ip packet has options and if its all in one mbuf *)
        WITH ip_buf = Mbuf.Array(m),
             ip_header = VIEW(ip_buf^,IpPktFormat.NewT),
             hlen = ip_header.hlen*4
         DO

          IF hlen > NUMBER(ip_buf^) THEN

            IF debug_level # Net.oLevel.NODEBUG THEN
              Net.Debug(debug_level,Net.oLevel.DEBUG," hlen > num(ip_buf)  ");
            END;



            (* Not all in one mbuf, pull up the data into this mbuf *)
            m := Mbuf.m_pullup(m, hlen);
            IF m = NIL THEN RETURN FALSE END;

            IF debug_level # Net.oLevel.NODEBUG THEN
              Net.Debug(debug_level,Net.oLevel.DEBUG," m_pullup2 ");
            END;

          END;

          IF debug_level # Net.oLevel.NODEBUG THEN
            Net.Debug(debug_level,Net.oLevel.INFO," after hlen > test ");
          END;

        END; (* with *)

      ELSE m := packet; END; (* if mlen < hdr_len *)
    END;

    IF debug_level # Net.oLevel.NODEBUG THEN
      Net.Debug(debug_level,Net.oLevel.INFO,"[atmIP raise Ip.PA]");
    END;

    INC(ip_packet_counter,1);
    WITH ippayload = Mbuf.Array(m) DO 
      Spy.Exit(iptimer);
      EVAL Ip.PacketArrived(m, ippayload);
    END;

    RETURN FALSE;
  END PacketArrived_IP;


PROCEDURE Init() =
  BEGIN
    iptimer := Spy.Create("atm_input ip");
    (* Install the IP handler that funnels all IP packets up to the next level *)
    ip := AtmTrusted.Install(Atm.PacketArrived,WhenClause_IP,
                               PacketArrived_IP);
    IO.Put("AtmClassification() IP demuxer installed\n");
    dataflow    := Spy.Create("AtmPacket.Deliver -> AtmIp.PA");
  END Init;

BEGIN
END AtmClassification. 


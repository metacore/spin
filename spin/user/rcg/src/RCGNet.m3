(*
 *
 * Copyright 1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *)

(*
 * HISTORY
 *
 *)

UNSAFE MODULE RCGNet;

IMPORT Stdio, Wr, Thread, Fmt, Word;
IMPORT Guard;
IMPORT rpcc;

(* for networking guards *)
IMPORT IpClassification, Mbuf, MbufDep, IpPktFormat, IpFrag;

<* FATAL Thread.Alerted, Wr.Failure *>


(************************** test net guards ************************)

TYPE ProcNet =
  PROCEDURE (packet: Mbuf.T; curr: Mbuf.T; offset: CARDINAL) : Word.T;


PROCEDURE NetTest1 () =
  VAR
    testArray := NEW(REF ARRAY OF PROCANY, 3);
  BEGIN
    testArray[0] := IpClassification.Guard_ICMP;
    testArray[1] := IpClassification.Guard_UDP;
    testArray[2] := IpClassification.Guard_TCP;

    NetTest (testArray);
  END NetTest1;


PROCEDURE NetTest2 () =
  VAR
    testArray := NEW(REF ARRAY OF PROCANY, 5);
  BEGIN
    testArray[0] := IpClassification.Guard_ICMP;
    testArray[1] := IpClassification.Guard_UDP;
    testArray[2] := IpClassification.Guard_TCP;
(*  
    testArray[3] := IpFrag.Guard_UDP;
    testArray[4] := IpFrag.Guard_TCP;
*)
    NetTest (testArray);
  END NetTest2;


(* internal *)
PROCEDURE NetTest (testArray: REF ARRAY OF PROCANY) =
  VAR
    packet := NEW (Mbuf.T);
    p : PROCANY;

  PROCEDURE Test () =
    VAR
      merged := LOOPHOLE (p, ProcNet);
      val : BOOLEAN;
      mval : Word.T;
    BEGIN
      val := IpClassification.Guard_ICMP (NIL, packet, 0);
      Wr.PutText (Stdio.stdout, "ICMP returns " & BoolToText (val) & "\n");
      val := IpClassification.Guard_UDP (NIL, packet, 0);
      Wr.PutText (Stdio.stdout, "UDP returns " & BoolToText (val) & "\n");
      val := IpClassification.Guard_TCP (NIL, packet, 0);
      Wr.PutText (Stdio.stdout, "TCP returns " & BoolToText (val) & "\n");
      mval := merged (NIL, packet, 0);
      Wr.PutText (Stdio.stdout, "merged returns " & Fmt.Unsigned (mval) & "\n");
    END Test;

  PROCEDURE BoolToText (b: BOOLEAN) : TEXT =
    BEGIN
      IF b THEN RETURN "TRUE"; ELSE RETURN "FALSE"; END;
    END BoolToText;
      
  BEGIN
    
    p := Guard.OptimizeAll (testArray);
    
    packet.mh_hdr.mh_data := LOOPHOLE (ADR (packet.M_dat), Word.T);
    packet.mh_hdr.mh_len := MbufDep.MLEN;
    
    (*
      Wr.PutText (Stdio.stdout, "packet len is " & Fmt.Int (BYTESIZE (IpPktFormat.T)) & " array is " & Fmt.Int (BYTESIZE (Mbuf.Array(packet)^)) & "\n");
    *)
    WITH data = VIEW (Mbuf.Array(packet)^, IpPktFormat.T) DO
      data.vers := 4;
      data.hlen := 5;
      data.frag_off := 0;
      data.protocol := IpPktFormat.IPPROTO_ICMP;
      Wr.PutText (Stdio.stdout, "ICMP packet\n");
      Test ();
      
      data.protocol := IpPktFormat.IPPROTO_UDP;
      Wr.PutText (Stdio.stdout, "UDP packet\n");
      Test ();
      
      data.protocol := IpPktFormat.IPPROTO_TCP;
      Wr.PutText (Stdio.stdout, "TCP packet\n");
      Test ();
      
      data.protocol := IpPktFormat.IPPROTO_PUP;
      Wr.PutText (Stdio.stdout, "PUP packet\n");
      Test ();
    END;
  END NetTest;


BEGIN
END RCGNet.

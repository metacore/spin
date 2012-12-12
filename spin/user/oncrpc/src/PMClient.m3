(*
   PMClient.m3
   Simple test program for M3 Sun RPC.
   David Nichols, Xerox PARC
   March, 1992

   $Id: PMClient.m3,v 1.1 1996/02/09 18:08:15 mef Exp $
*)

(* Copyright (c) 1992 Xerox Corporation.  All rights reserved.

   Use and copying of this software and preparation of derivative works
   based upon this software are permitted.  Any distribution of this
   software or derivative works must comply with all applicable United
   States export control laws.  This software is made available AS IS, and
   Xerox Corporation makes no warranty about the software, its performance
   or its conformity to any specification. *)

MODULE Main;

IMPORT PortMapper, RPC, RPCSun;
IMPORT Fmt, ParseParams, RTMisc, Scan, Stdio, Thread, Wr;

<* FATAL Thread.Alerted, Wr.Failure, RPCSun.Erred *>

CONST
  PMProg = 100000;
  PMVers = 2;

VAR
  host       : INTEGER;
  b          : RPCSun.BindingInfo;
  pm         : PortMapper.PMAP_VERS;
  l          : PortMapper.pmaplist;
  protoString: TEXT;

BEGIN
  TRY
    TRY
      ParseParams.BeginParsing(Stdio.stderr);
      host := RPCSun.LookupHost(ParseParams.GetNext());
      ParseParams.EndParsing();
    EXCEPT
      Scan.BadFormat =>
        Wr.PutText(Stdio.stderr, "Usage: PMClient host\n");
        RTMisc.Exit(1);
    END;
    b :=
      RPCSun.CreateBindingInfo(
        host, PMProg, PMVers, PortMapper.PMAP_PORT, RPCSun.Protocol.UDP);
    pm := PortMapper.ImportPMAP_VERS(b);
    l := pm.Dump();
  EXCEPT
    RPC.Failed (e) =>
      Wr.PutText(Stdio.stderr, "RPC failure: " & e.info & "\n");
      RTMisc.Exit(1);
  END;
  Wr.PutText(Stdio.stdout,
             Fmt.F("%10s %6s %6s %6s\n", "prog", "vers", "proto", "port"));
  WHILE l # NIL DO
    IF l.map.prot = PortMapper.IPPROTO_TCP THEN
      protoString := "TCP";
    ELSIF l.map.prot = PortMapper.IPPROTO_UDP THEN
      protoString := "UDP";
    ELSE
      protoString := "unknown";
    END;
    Wr.PutText(
      Stdio.stdout,
      Fmt.F("%10s %6s %6s %6s\n", Fmt.Int(l.map.prog), Fmt.Int(l.map.vers),
            protoString, Fmt.Int(l.map.port)));
    l := l.next;
  END;
END Main.

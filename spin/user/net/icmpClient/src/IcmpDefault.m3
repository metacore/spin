(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 19-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	make guards FUNCTIONAL
 *
 * 07-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed to new spin shell command style.
 *
 * 09-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	Replaced obsolete interface Clib with IO.
 *
 *)

MODULE IcmpDefault;

IMPORT IO;
IMPORT Net;
IMPORT Mbuf;
IMPORT Icmp;

IMPORT Spy;

IMPORT ParseParams;

PROCEDURE Run(pp: ParseParams.T): BOOLEAN =
  BEGIN
    pp.reset();
    TRY
      pp.skipNext(); (* icmpdefault *)
      IF pp.testNext("-debug") THEN
        debug_level := Net.MapDebug(pp.getNextInt());
      ELSIF pp.testNext("-packets") THEN
        IO.Put("icmp stats: received ");
        IO.PutInt(icmp_packet_counter);
        IO.Put(" packets.\n");
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

VAR
  icmp_packet_counter : INTEGER := 0;
  dataflow: Spy.T;
  promiscuoustimer: Spy.T;
  promiscuous: REFANY;
  debug_level : Net.Level := Net.oLevel.NODEBUG;
(* when clauses and handlers -- the real stuff *)

FUNCTIONAL
PROCEDURE promiscuous_WC(<* UNUSED *> packet,curr: Mbuf.T; <* UNUSED *> offset: CARDINAL):BOOLEAN =
  BEGIN
(*
    IF debug_level # Net.oLevel.NODEBUG THEN
      Net.Debug(debug_level, Net.oLevel.INFO,"IcmpDefault.WC ");
    END;

    Spy.Enter(promiscuoustimer);
*)
    RETURN TRUE;
  END promiscuous_WC;

PROCEDURE promiscuous_PacketArrived(<* UNUSED *> packet,curr: Mbuf.T; <* UNUSED *> offset: CARDINAL):BOOLEAN =
  BEGIN
    (* #ifdef debug_level != NODEBUG *)
    IF debug_level # Net.oLevel.NODEBUG THEN
      Net.Debug(debug_level, Net.oLevel.INFO,"IcmpDefault.PA ");
    END;
    INC(icmp_packet_counter,1);
    Spy.Exit(promiscuoustimer);
    RETURN FALSE;
  END promiscuous_PacketArrived;

PROCEDURE Init(verbose:BOOLEAN) = 
  BEGIN
    (* install Icmp handlers *)
    dataflow := Spy.Create("icmp_input data_flow");
    promiscuoustimer := Spy.Create("icmp_input counter");
    promiscuous := Icmp.Install(Icmp.PacketArrived,
                                promiscuous_WC,
                                promiscuous_PacketArrived);
    IF verbose THEN IO.Put("IcmpDefault module initialized\n"); END;
  END Init;

BEGIN
END IcmpDefault. 

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
 *	Added support for new spin shell commands.
 *
 *)

(* Untrusted *) 
MODULE UdpDefault;

IMPORT IO;
IMPORT Udp;
(* IMPORT Net; *)
IMPORT Mbuf;
IMPORT Spy;
IMPORT Net;
IMPORT ParseParams;

PROCEDURE Run(pp: ParseParams.T): BOOLEAN =
  BEGIN
    pp.reset();
    TRY
      pp.skipNext(); (* ipdefault *)
      IF pp.testNext("-debug") THEN
        debug_level := Net.MapDebug(pp.getNextInt());
      ELSIF pp.testNext("-packets") THEN
        IO.Put("udp stats: received ");
        IO.PutInt(udp_packet_counter);
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
  udp_packet_counter : INTEGER := 0;
  debug_level : Net.Level := Net.oLevel.NODEBUG;
  dataflow: Spy.T;
  promiscuoustimer: Spy.T;
  promiscuous: REFANY;

FUNCTIONAL
PROCEDURE promiscuous_WC (<*UNUSED*>packet, curr: Mbuf.T; 
                          <*UNUSED*>offset: CARDINAL):BOOLEAN =
  BEGIN
(*
    Spy.Enter(promiscuoustimer);
*)
    RETURN TRUE;
  END promiscuous_WC; 

PROCEDURE promiscuous_PacketArrived (<*UNUSED*>packet, curr: Mbuf.T; 
                                     <*UNUSED*>offset: CARDINAL):BOOLEAN =
  BEGIN
    INC(udp_packet_counter);
    Spy.Exit(promiscuoustimer);
    RETURN FALSE; (* TRUE if consuming packet *)
  END promiscuous_PacketArrived; 

PROCEDURE Init(verbose:BOOLEAN) =
  BEGIN
    (* install promiscuous handlers *)
    dataflow := Spy.Create("udp_counter data_flow");
    promiscuoustimer := Spy.Create("udp_input counter");
    promiscuous := Udp.Install(Udp.PacketArrived,
                                      promiscuous_WC,
                                      promiscuous_PacketArrived);
    IF verbose THEN IO.Put ("UdpDefault module initialized.\n"); END;
  END Init;

BEGIN
END UdpDefault. 

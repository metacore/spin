(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 * 19-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	make guards FUNCTIONAL
 *
 * 02-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed to new IO interface and printing less debugging information.
 *      Updated to use new spin shell command style.
 *)

(* Untrusted *) 
MODULE EtherDefault;
IMPORT IO, Net, Ether, Mbuf, Spy;

VAR
  dataflow : Spy.T;
  promiscuoustimer : Spy.T;
  promiscuous: REFANY;

CONST
  timing = FALSE;
  debug = FALSE;

FUNCTIONAL
PROCEDURE PROMISCUOUSGuard(
    <* UNUSED *> packet: Mbuf.T; 
    <* UNUSED *> curr: Mbuf.T; 
    <* UNUSED *> offset: CARDINAL):BOOLEAN =
  BEGIN
(*
    IF timing THEN Spy.Enter(promiscuoustimer); END;
*)
    (* noop *)
    RETURN TRUE;
  END PROMISCUOUSGuard;

PROCEDURE PROMISCUOUSHandler(
    <* UNUSED *> packet: Mbuf.T; 
    <* UNUSED *> curr: Mbuf.T; 
    <* UNUSED *> offset: CARDINAL):BOOLEAN =
  BEGIN

    INC(ether_packet_counter,1);

    IF timing THEN 
      Spy.Exit(promiscuoustimer); 
      (*
      WITH timer = VIEW(packet.mh_hdr.mh_union[0], INTEGER),
           stop = SAL.Timestamp() 
       DO
        Spy.Hit(dataflow,timer,stop);
        timer := stop;
      END;
      *)
    END;
    IF debug AND debug_level # Net.oLevel.NODEBUG THEN
      Net.Debug(debug_level,Net.oLevel.INFO,"Ether ");
    END;

    RETURN FALSE; (* TRUE if consuming packet *)
  END PROMISCUOUSHandler;

PROCEDURE Init(verbose: BOOLEAN) = 
  BEGIN
    (* Install handlers *)
    dataflow := Spy.Create("ether_counter data_flow");
    promiscuoustimer := Spy.Create("ether_input counter");
    promiscuous := Ether.Install(Ether.PacketArrived,
                                 PROMISCUOUSGuard,
                                 PROMISCUOUSHandler);
    ether_packet_counter := 0;
    IF verbose THEN 
      IO.Put("EtherDefault module initialized.\n");
    END;
  END Init;

BEGIN
END EtherDefault.

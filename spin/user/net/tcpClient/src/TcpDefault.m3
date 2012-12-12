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
 *	Converted to use new spin shell commands.
 *
 * 09-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	Replaced obsolete interface Clib with IO.
 *
 * 02-Aug-95  Marc Fiuczynski (mef) at the University of Washington
 *	Implements a simple counter handler that just counts the number
 *	of TCP packets arrived.  Could be used for netstat like
 *	information.
 *
 *)
MODULE TcpDefault;

IMPORT IO;
IMPORT Mbuf;
IMPORT Net;
IMPORT Tcp;

IMPORT ParseParams;

PROCEDURE Run(pp: ParseParams.T): BOOLEAN =
  BEGIN
    pp.reset();
    TRY
      pp.skipNext(); (* ipdefault *)
      IF pp.testNext("-debug") THEN
        debug_level := Net.MapDebug(pp.getNextInt());
      ELSIF pp.testNext("-packets") THEN
        IO.Put("tcp stats: received ");
        IO.PutInt(tcp_packet_counter);
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
  tcp_packet_counter : INTEGER := 0;
  counter: REFANY;

  debug_level : Net.Level := Net.oLevel.WARNING;

(* when clauses and handlers -- the real stuff *)

FUNCTIONAL
PROCEDURE WhenClause_COUNTER(<* UNUSED *> packet, curr: Mbuf.T; <* UNUSED *> offset: CARDINAL):BOOLEAN =
  BEGIN
    RETURN TRUE;
  END WhenClause_COUNTER;

PROCEDURE PacketArrived_COUNTER(<* UNUSED *> packet, curr: Mbuf.T; <* UNUSED *> offset:CARDINAL):BOOLEAN = 
  BEGIN
    INC(tcp_packet_counter,1);
    (* I guess this is no uglier than a #if debug_level = NODEBUG *)
    IF debug_level # Net.oLevel.NODEBUG THEN
      Net.Debug(debug_level,Net.oLevel.INFO,"TCP_PA ");
    END;

    RETURN FALSE; (* return true if consuming packet *)
  END PacketArrived_COUNTER;

PROCEDURE Init(verbose:BOOLEAN) = 
  BEGIN
    (* install handlers *)
    counter := Tcp.Install(Tcp.PacketArrived,
                           WhenClause_COUNTER,
                           PacketArrived_COUNTER);
    IF verbose THEN IO.Put("TcpDefault module initialized\n"); END;
  END Init;

BEGIN
END TcpDefault. 

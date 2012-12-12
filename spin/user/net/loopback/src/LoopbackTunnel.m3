(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 *
 *
 * HISTORY
 * 31-May-97  David Becker at the University of Washington
 *      Replaced If interface with NetDev/EtherDev
 *
 * 14-Oct-96  Robert Grimm (rgrimm) at the University of Washington
 *      added DispatcherPrivate.KeepStub for PacketSend
 *
 * 19-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	make guards FUNCTIONAL
 *)

(* Untrusted *)
MODULE LoopbackTunnel;

IMPORT Loopback, Ip, IpRoute, If, Net, NetDev, Mbuf, IO,
       Ctypes, SocketRep, SocketAddr, SocketAddrIn, SpinException,
       Thread, ThreadExtra, Sema, ParseParams, Dispatcher;

IMPORT DispatcherPrivate;
IMPORT Debugger;

PROCEDURE Run(pp: ParseParams.T): BOOLEAN =
  BEGIN
    pp.reset();
    TRY
      pp.skipNext(); (* loopback *)
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

VAR
  ip: REFANY;
  debug_level : Net.Level := Net.oLevel.NODEBUG;
  io_thread   : Thread.T;
  ifq         : If.ifqueue;
  ifqMutex    : MUTEX;
  process_packet: Sema.T;

<*UNUSED*>
CONST
  timing = FALSE;
CONST
  debug  = FALSE;

FUNCTIONAL
PROCEDURE WhenClause_IP(
    <* UNUSED *> packet: Mbuf.T; 
    <* UNUSED *> curr: Mbuf.T; 
    <* UNUSED *> offset: CARDINAL):BOOLEAN =
  BEGIN
    RETURN TRUE;
  END WhenClause_IP;

PROCEDURE PacketArrived_IP(
    <*UNUSED*>packet: Mbuf.T; 
    curr: Mbuf.T; 
    <* UNUSED *> offset: CARDINAL):BOOLEAN =
  CONST
    ipOffset = 0;
  BEGIN
    EVAL Ip.PacketArrived(curr, curr, ipOffset);
    RETURN FALSE;
  END PacketArrived_IP;

PROCEDURE PacketSend( 
    <* UNUSED *> dev: NetDev.T;
    mbuf    : Mbuf.T; 
    VAR s   : SocketAddr.T;
    <* UNUSED *> rte     : ADDRESS := NIL) : Ctypes.int =
  BEGIN
    IF debug AND debug_level # Net.oLevel.NODEBUG THEN
      Net.Debug(debug_level,Net.oLevel.INFO,"LoopbackTunnel.PacketSend ");
    END;
    
    CASE s.sa_family OF 
    | SocketRep.AF_INET => 
      (* IP packet assumes that mbuf starts with ip header. *)
      LOCK ifqMutex DO
        If.Enqueue(ifq,mbuf);
      END;
      (* kick the I/O thread. *)
      Sema.V(process_packet);
    ELSE
      Debugger.Enter();
      IO.Put("Address Family not supported.\n");
      Mbuf.m_freem(mbuf);
    END;
    RETURN 0;
  END PacketSend; 

PROCEDURE AsyncDeliver(<*UNUSED*>arg: ThreadExtra.ArgT) : ThreadExtra.ResultT =
  VAR packet: Mbuf.T;
  BEGIN
    LOOP
      packet := NIL;
      Sema.P(process_packet);
      LOCK ifqMutex DO
        packet := If.Dequeue(ifq);
      END;
      TRY
        TRY
          EVAL Loopback.PacketArrived(packet,packet,0);
        EXCEPT
          SpinException.Exception(ec) =>
          IO.Put("loopback caught ");
          IO.Put(ec.msg);
          IO.Put(" exception\n");
        END;
      FINALLY
        Mbuf.m_freem(packet);
      END;
    END;
  END AsyncDeliver;

PROCEDURE Init() =
  (*
    VAR dev: NetDev.T;
      tmpaddr : SocketAddrIn.in_addrT;
  *)
  BEGIN
    TRY
      DispatcherPrivate.KeepStub(PacketSend);
    EXCEPT
    | Dispatcher.Error =>
      IO.PutError("LoopbackTunnel dispatcher error: can't keep stub\n");
    END;

    process_packet := Sema.Alloc();
    io_thread := ThreadExtra.PFork(AsyncDeliver, NIL);
    (* ifq initialization *)
    ifq.ifq_head := NIL;
    ifq.ifq_tail := NIL;
    ifq.ifq_len := 0;
    ifq.ifq_maxlen := If.MAXLEN;
    ifq.ifq_drops := 0;
    ifqMutex := NEW(MUTEX);


    ip := Loopback.Install(Loopback.PacketArrived,WhenClause_IP,
                           PacketArrived_IP);
    (* IO.Put("LoopbackTunnel module initialized\n"); *)
  END Init;

BEGIN
END LoopbackTunnel. 

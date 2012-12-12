(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 14-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	New interface to interrupt handling.
 *
 * 20-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Allocating mbuf in Arrived procedure with MT_DONTWAIT flag,
 *	because we are executing at SPL.IO.  
 *
 * 04-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	Changed KThread to Thread.
 *
 * 21-Dec-95  Charles Garrett (garrett) at the University of Washington
 *	Fixed mbuf leak in EtherPacket.Deliver.
 *
 * 27-Jun-95  Marc Fiuczynski (mef) at the University of Washington
 *	This module implements the transition from the trusted SPIN kernel to
 *	untrusted extensions.  For performance reasons we are using some of
 *	the fields of the underlying MACH net_kmsg.  This module needs to
 *	be cleaned up further.
 *	
 *	This module also emulates the dispatcher support for bounded,
 *	non-blocking event-handlers.  To be replaced with dispatcher
 *	support.
 *
 * 05-March-95  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *
 *)

(* TRUSTED *) 
UNSAFE (* imports unsafe interface *)
MODULE EtherPacket;

(* IMPORT SALExtern; *)
IMPORT EtherPacketExtern; (* unsafe *)

IMPORT Ether; (* ethernet event name  *)
IMPORT EtherPktFormat;
IMPORT Net; (* networking support  *)
IMPORT Mbuf; (* unix like mbuf structure *)
IMPORT MbufPublic;
IMPORT Thread, ThreadExtra, MachineCPUPrivate, Machine;  (* thread, spl *)
IMPORT Sema;
IMPORT SpinException;
IMPORT Word; <* NOWARN *>
IMPORT IO; (* libm3s *)
IMPORT Spy,SAL; (* timing *)
IMPORT If;

(* IMPORT Trap_internal; *)


IMPORT ParseParams;

PROCEDURE Run(pp: ParseParams.T): BOOLEAN =
  BEGIN
    pp.reset();
    TRY
      pp.skipNext(); (* etherdefault *)
      IF pp.testNext("-debug") THEN
        debug_level := Net.MapDebug(pp.getNextInt());
      ELSIF pp.testNext("-synctoggle") THEN
        async_dispatch := NOT async_dispatch; 
        IF async_dispatch THEN 
          IO.Put("EtherPacket sync -> async\n");
        ELSE
          IO.Put("EtherPacket async -> sync\n");
        END;
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

(********************************************************************************)
(* synchronization and queue for incoming packets *)

VAR
  process_packet   : Sema.T;
  (* toggle between asynchronous and synchronous dispatching *)
  async_dispatch   : BOOLEAN := TRUE;
  (* various timers *)
  ether_intr_timer : Spy.T;
  checkpoint0      : Spy.T;
  checkpoint1      : Spy.T;
  checkpoint2      : Spy.T;
  checkpoint3      : Spy.T;
  debug_level      : Net.Level := Net.oLevel.NODEBUG;
  io_thread        : Thread.T;
  ifq              : If.ifqueue;

CONST
  debug = FALSE;
  timing = FALSE;

VAR
  dataflow   := Spy.Create("IntrHandler -> Deliver");


(*
 * Arrived()
 * 
 * Takes an incoming packet from the ethernet and pushes it up through
 * the protocol decission tree.
 *
 * For asynchronous dispatch enqueue the packet and kick the worker
 * thread.  Otherwise, the event handlers are invoked as part of the
 * interrupt handler.  This support will be supported and checked by
 * the SPIN dispatcher.
 *
 * Executed at MachineCPU.InterruptLevel.High.
 *)

PROCEDURE Arrived(
    ifp: UNTRACED REF If.ifnet;
    READONLY ehp: EtherPktFormat.NewT; 
    m: Mbuf.T): BOOLEAN =
  CONST
    header_len = BYTESIZE(EtherPktFormat.Header);
  VAR
    packet : Mbuf.T;
  BEGIN
      (* start the timer *)
      (* Spy.Enter(ether_intr_timer);*)

      packet := Mbuf.m_gethdr(Mbuf.M_DONTWAIT,Mbuf.MT_DATA);
      IF packet = NIL THEN
        Mbuf.m_freem(m);
        RETURN FALSE;
      END;
      MbufPublic.SetPktHdrRcvIf(packet,ifp);
      packet.mh_hdr.mh_len  := header_len;
      packet.mh_hdr.mh_next := m;
      WITH eth_buf = Mbuf.Array(packet),
           eth = VIEW(eth_buf^, EtherPktFormat.NewT)
       DO
        (* XXX bad that we have to copy data to queue ether packet
           this is because the MachineCPUPrivatef/1 device driver wont stick the
           ether header into a separate mbuf.
        *)
        eth.shost := ehp.shost;
        eth.dhost := ehp.dhost;
        eth.type  := ehp.type;
      END;

      IF timing THEN 
        WITH timer = VIEW(packet.mh_hdr.mh_union[0],INTEGER) DO
          timer := SAL.Timestamp();
        END;
        Spy.Enter(checkpoint0);
      END;

      IF async_dispatch = TRUE THEN
        If.Enqueue(ifq,packet);
        (* kick the I/O thread. *)
        Sema.V(process_packet);
        IF timing THEN Spy.Exit(ether_intr_timer); END;
      ELSE
        IF timing THEN Spy.Exit(ether_intr_timer); END;
        (* Deliver the packet to the next level directly 
           from the interrupt handler. *)
        Deliver(packet);
      END;
    RETURN TRUE; (* this protocol stack consumes and frees the mbuf input *)
  END Arrived; 

PROCEDURE AsyncDeliver(<*UNUSED*>arg: ThreadExtra.ArgT) : ThreadExtra.ResultT =
  VAR packet: Mbuf.T;
      spl: MachineCPU.InterruptLevel;

  BEGIN
    LOOP
      Sema.P(process_packet);
      (* Spy.Exit(Trap_internal.deviceTimer); *)
      (* dequeue operations has to be atomic wrt to the ether interrupt handler *)
      spl := MachineCPUPrivate.SetInterruptMask(MachineCPUPrivate.InterruptClass.High);
      packet := If.Dequeue(ifq);
      MachineCPUPrivate.RestoreInterruptMask(spl);        
      Deliver(packet);
    END;
  END AsyncDeliver;

(*
 * Deliver()
 *      Pushes packet up to the next level in the protocol decission tree.
 *)
PROCEDURE Deliver(packet:Mbuf.T) = 
  VAR
    data_pkt : Mbuf.T;
  BEGIN
    IF timing THEN Spy.Exit(checkpoint0); END;
    IF timing THEN Spy.Enter(checkpoint2); END;
    IF debug AND debug_level # Net.oLevel.NODEBUG THEN
      Net.Debug(debug_level,Net.oLevel.INFO,"EtherPacket Deliver ");
    END;

    data_pkt := packet.mh_hdr.mh_next;
    (* hack to deal with sal networking modifications to mbuf *)
    IF data_pkt.mh_hdr.mh_len = 0 AND data_pkt.mh_hdr.mh_next # NIL THEN 
      data_pkt := data_pkt.mh_hdr.mh_next;
    END;

    IF timing THEN 
      WITH timer = VIEW(packet.mh_hdr.mh_union[0],INTEGER),
           stop = SAL.Timestamp()
       DO 
        Spy.Hit(dataflow,timer,stop);
        timer := stop;
      END;
    END;

    TRY
      TRY
        EVAL Ether.PacketArrived(packet,data_pkt,0);
      EXCEPT
        SpinException.Exception =>
      END;
    FINALLY
      IF timing THEN Spy.Exit(checkpoint2); END;

      IF timing THEN Spy.Enter(checkpoint3); END;
      Mbuf.m_freem(packet);
      IF timing THEN Spy.Exit(checkpoint3); END;
    END;
  END Deliver;
    
(*
 * Init()
 * Used to initialize this module, since we cannot rely on M3
 * initialization order.
 *)
PROCEDURE Init() = 
  BEGIN

    (* create ether packet thread *)
    (* IO.Put("EtherPacket I/O Thread started. NOTE: replace with async dispatch.\n");*)
    (* create the sema used to sync with the thread *)
    process_packet := Sema.Alloc();
    io_thread := ThreadExtra.PFork(AsyncDeliver, NIL);

    IF timing THEN
      ether_intr_timer := Spy.Create("Intr packet arrived");
      checkpoint0 := Spy.Create("checkpoint 0");
      checkpoint1 := Spy.Create("checkpoint 1");
      checkpoint2 := Spy.Create("checkpoint 2");
      checkpoint3 := Spy.Create("checkpoint 3");
    END;

    (* ifq initialization *)
    ifq.ifq_head := NIL;
    ifq.ifq_tail := NIL;
    ifq.ifq_len := 0;
    ifq.ifq_maxlen := If.MAXLEN;
    ifq.ifq_drops := 0;

    (* reset procedure variable in sal/standalone/net/if_ethersubr.c *)
    (* The following should be done by the lance.m3 module. *)
    EtherPacketExtern.ether_input := Arrived;
    (* IO.Put("EtherPacket module initialized.\n"); *)
  END Init;

BEGIN
END EtherPacket.

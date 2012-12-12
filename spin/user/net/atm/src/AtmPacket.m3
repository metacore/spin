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
 * 04-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	Changed KThread to Thread.
 *
 * 18-Dec-95  Przemek Pardyak (pardy) at the University of Washington
 *	Added Help procedure to conform to the new shell interface.
 *
 * 30-Nov-95  Marc Fiuczynski (mef) at the University of Washington
 *	Took out the atm_input upcall variable.  
 *	Needs to go back after SOSP.
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
MODULE AtmPacket;

IMPORT Atm; (* atmnet event name  *)
IMPORT AtmPktFormat;
IMPORT Net; (* networking support  *)
IMPORT Mbuf; (* unix like mbuf structure *)
IMPORT ThreadExtra, CPUPrivate, Machine;  (* thread, spl *)
IMPORT Thread;
(* IMPORT Word;*)
IMPORT IO; (* libm3s *)
IMPORT Spy;  (* timing *)
(* IMPORT SAL; *)
(* IMPORT Ctypes;*)
IMPORT Sema;
IMPORT If;

(* DynShell support *)
IMPORT Text,SafeConvert;
(* IMPORT Dispatcher; *)
IMPORT SpinShell;
<* UNUSED *>VAR shell := SpinShell.InstallCommand(EC_Guard,RunCommand,Help);

PROCEDURE EC_Guard(argc: CARDINAL; READONLY argv: ARRAY OF TEXT): BOOLEAN =
  BEGIN
    RETURN argc > 0 AND Text.Equal(argv[0], "AtmPacket");
  END EC_Guard;

PROCEDURE RunCommand(argc: CARDINAL; READONLY argv: ARRAY OF TEXT): BOOLEAN =
  VAR
  BEGIN
    IF Text.Equal(argv[1], "synctoggle") THEN
      async_dispatch := NOT async_dispatch; 
      IF async_dispatch THEN 
        IO.Put("AtmPacket sync -> async\n");
      ELSE
        IO.Put("AtmPacket async -> sync\n");
      END;
    ELSIF Text.Equal(argv[1], "debug") THEN
      debug_level := Net.MapDebug(SafeConvert.Atoi(argv[2]));
    ELSE
      IO.Put("AtmPacket.RunCommand: no such command ");
      FOR i := 0 TO argc-1 DO
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
  (* toggle between asynchronous and synchronous dispatching *)
  async_dispatch       : BOOLEAN := FALSE;
  (* synchronization and queue for incoming packets *)
  process_packet       : Sema.T;
  (* various timers *)
  atm_intr_timer     : Spy.T;
  thread1_latency_timer: Spy.T;
  thread2_latency_timer: Spy.T;
  process_packet_timer : Spy.T;
  debug_level          : Net.Level := Net.oLevel.NODEBUG;
  io_thread            : Thread.T;
  ifq                  : REF If.ifqueue;

(*
 * Arrived()
 * 
 * Takes an incoming packet from the atmnet and pushes it up through
 * the protocol decission tree.
 *
 * For asynchronous dispatch enqueue the packet and kick the worker
 * thread.  Otherwise, the event handlers are invoked as part of the
 * interrupt handler.  This support will be supported and checked by
 * the SPIN dispatcher.
 *
 * Executed at CPU.InterruptLevel.IO???
 *)

PROCEDURE Arrived(<* UNUSED *> ifp: UNTRACED REF If.ifnet; m: Mbuf.T) =
  CONST
    header_len = BYTESIZE(AtmPktFormat.Header);
  VAR
    packet : Mbuf.T;
  BEGIN
    packet := Mbuf.m_get(Mbuf.M_WAIT,Mbuf.MT_DATA);
    packet.mh_hdr.mh_len  := header_len;
    packet.mh_hdr.mh_next := m;

    (* dequeue operations has to be atomic wrt to the atm interrupt handler *)
    WITH spl = CPUPrivate.SetInterruptMask(CPUPrivate.InterruptClass.IO) DO
      IF async_dispatch = TRUE THEN
        If.Enqueue(ifq^,packet);
        (* kick the I/O thread. *)
        Sema.V(process_packet); 
      ELSE
        (* Deliver the packet to the next level directly from the interrupt handler. *)
        Deliver(packet);
      END;
      CPUPrivate.RestoreInterruptMask(spl);        
    END;
  END Arrived; 

PROCEDURE AsyncDeliver(<*UNUSED*>arg: ThreadExtra.ArgT) : ThreadExtra.ResultT =
  VAR packet: Mbuf.T;
  BEGIN
    LOOP
      Sema.P(process_packet);
      WITH spl = CPUPrivate.SetInterruptMask(CPUPrivate.InterruptClass.IO) DO
        packet := If.Dequeue(ifq^);
        CPUPrivate.RestoreInterruptMask(spl);        
        <* ASSERT packet # NIL *> 
      END;
      Deliver(packet);
    END;
  END AsyncDeliver;

(*
 * Deliver()
 *      Pushes packet up to the next level in the protocol decission tree.
 *)
PROCEDURE Deliver(packet:Mbuf.T) = 
  BEGIN
    WITH data_pkt = packet.mh_hdr.mh_next DO
      EVAL Atm.PacketArrived(data_pkt,NIL);
      Mbuf.m_freem(packet);
    END;
  END Deliver;

(*
 * Init()
 * Used to initialize this module, since we cannot rely on M3
 * initialization order.
 *)
PROCEDURE Init() = 
  BEGIN

    (* create atm packet thread *)
    (* IO.Put("AtmPacket I/O Thread started. NOTE: replace with async dispatch.\n"); *)
    process_packet := Sema.Alloc();
    io_thread := ThreadExtra.PFork(AsyncDeliver, NIL);

    atm_intr_timer      := Spy.Create("Intr packet arrived");
    thread1_latency_timer := Spy.Create("Intr1 -> thread latency");
    thread2_latency_timer := Spy.Create("Intr2 -> thread latency");
    process_packet_timer  := Spy.Create("Total atm pkt processing");
    

    (* ifq initialization *)
    ifq := NEW(REF If.ifqueue);
    ifq.ifq_head := NIL;
    ifq.ifq_tail := NIL;
    ifq.ifq_len := 0;
    ifq.ifq_maxlen := If.MAXLEN;
    ifq.ifq_drops := 0;

    (* reset procedure variable in sal/standalone/net/if_ethersubr.c *)
    (* The following should be done by the lance.m3 module. *)
    (* atm_input := Arrived; *)

    (* IO.Put("T3Packet module initialized.\n"); *)
  END Init;

BEGIN
END AtmPacket.

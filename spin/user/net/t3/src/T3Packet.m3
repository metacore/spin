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
 *
 * 18-Dec-95  Przemek Pardyak (pardy) at the University of Washington
 *	Added Help procedure to conform to the new shell interface.
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
MODULE T3Packet;

IMPORT T3PacketExtern; (* unsafe *)
IMPORT T3; (* t3net event name  *)
IMPORT T3PktFormat;
IMPORT Net; (* networking support  *)
IMPORT Mbuf; (* unix like mbuf structure *)
IMPORT Thread, ThreadExtra, CPUPrivate, Machine;  (* thread, spl *)
(* IMPORT Word;*)
IMPORT IO; (* libm3s *)
IMPORT Spy,SAL; (* timing *)
(* IMPORT Ctypes;*)
IMPORT Sema;
IMPORT If;

(* IMPORT Trap_internal; *)


(* DynShell support *)
IMPORT Text,SafeConvert;
IMPORT SpinShell;
<* UNUSED *>VAR shell := SpinShell.InstallCommand(EC_Guard,RunCommand,Help);

PROCEDURE EC_Guard(argc: CARDINAL; READONLY argv: ARRAY OF TEXT): BOOLEAN =
  BEGIN
    RETURN argc >= 1 AND Text.Equal(argv[0], "T3Packet");
  END EC_Guard;

PROCEDURE RunCommand(argc: CARDINAL; READONLY argv: ARRAY OF TEXT): BOOLEAN =
  VAR
  BEGIN
    IF argc > 1 AND Text.Equal(argv[1], "synctoggle") THEN
      async_dispatch := NOT async_dispatch; 
      IF async_dispatch THEN 
        IO.Put("T3Packet sync -> async\n");
      ELSE
        IO.Put("T3Packet async -> sync\n");
      END;
    ELSIF argc > 1 AND Text.Equal(argv[1], "debug") THEN
      debug_level := Net.MapDebug(SafeConvert.Atoi(argv[2]));
    ELSE
      IO.Put("T3Packet.RunCommand: no such command ");
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


(********************************************************************************)
(* synchronization and queue for incoming packets *)
VAR process_packet := Sema.Alloc();

VAR
  (* toggle between asynchronous and synchronous dispatching *)
  async_dispatch       : BOOLEAN := FALSE;
  (* various timers *)
  t3_intr_timer     : Spy.T;
  checkpoint0: Spy.T;
  checkpoint1: Spy.T;
  checkpoint2: Spy.T;
  checkpoint3: Spy.T;
  debug_level          : Net.Level := Net.oLevel.NODEBUG;
  io_thread            : Thread.T;
  ifq                  : If.ifqueue;

<* UNUSED *> VAR
  dataflow   := Spy.Create("IntrHandler -> Deliver");


(*
 * Arrived()
 * 
 * Takes an incoming packet from the t3net and pushes it up through
 * the protocol decission tree.
 *
 * For asynchronous dispatch enqueue the packet and kick the worker
 * thread.  Otherwise, the event handlers are invoked as part of the
 * interrupt handler.  This support will be supported and checked by
 * the SPIN dispatcher.
 *
 * Executed at CPU.InterruptLevel.High.
 *)

PROCEDURE Arrived (<* UNUSED *> ifp: UNTRACED REF If.ifnet; data: Mbuf.T) =
  CONST
    header_len = BYTESIZE(T3PktFormat.Header);
  VAR
    packet : Mbuf.T;

  BEGIN
    (* start the timer *)
    (* Spy.Enter(t3_intr_timer);*)

    packet := Mbuf.m_get(Mbuf.M_WAIT,Mbuf.MT_DATA);
    packet.mh_hdr.mh_len  := header_len;
    packet.mh_hdr.mh_next := data;
    packet.mh_hdr.mh_timer[0] := SAL.Timestamp();

    (* Spy.Enter(checkpoint0); *)

    IF async_dispatch = TRUE THEN
      If.Enqueue(ifq,packet);
      (* kick the I/O thread. *)
      Sema.V(process_packet);
      (* Spy.Exit(t3_intr_timer); *)
    ELSE
      (* Spy.Exit(t3_intr_timer); *)
      (* Deliver the packet to the next level directly from the interrupt handler. *)
      Deliver(packet);
    END;
    (* stop the timer *)
  END Arrived; 

PROCEDURE AsyncDeliver(<*UNUSED*>arg: ThreadExtra.ArgT) : ThreadExtra.ResultT =
  VAR packet: Mbuf.T;
      spl: CPU.InterruptLevel;

  BEGIN
    LOOP
      Sema.P(process_packet);
      (* dequeue operations has to be atomic wrt to the t3 interrupt handler *)
      (* Spy.Exit(Trap_internal.deviceTimer); *)
      spl := CPUPrivate.SetInterruptMask(CPUPrivate.InterruptClass.High);
      packet := If.Dequeue(ifq);
      CPUPrivate.RestoreInterruptMask(spl);        
      Deliver(packet);

    END;
  END AsyncDeliver;

(*
 * Deliver()
 *      Pushes packet up to the next level in the protocol decission tree.
 *)
PROCEDURE Deliver(packet:Mbuf.T) = 
  BEGIN
    (* Spy.Exit(checkpoint0); *)
    (* Spy.Enter(checkpoint2); *)
    IF debug_level # Net.oLevel.NODEBUG THEN
      Net.Debug(debug_level,Net.oLevel.INFO,"T3Packet Deliver ");
    END;

    WITH payload = Mbuf.Array(packet),
         data_pkt = packet.mh_hdr.mh_next
         (*, stop = SAL.Timestamp() *)
     DO

      (* 
         Spy.Hit(dataflow,packet.mh_hdr.mh_timer[0],stop);
         data_pkt.mh_hdr.mh_timer[0] := stop;
      *)

      EVAL T3.PacketArrived(data_pkt,payload);
      (* Spy.Exit(checkpoint2); *)

      (* Spy.Enter(checkpoint3); *)
      Mbuf.m_freem(packet);
      (* Spy.Exit(checkpoint3); *)
    END;
  END Deliver;

(*
 * Init()
 * Used to initialize this module, since we cannot rely on M3
 * initialization order.
 *)
PROCEDURE Init() = 
  BEGIN

    (* create t3 packet thread *)
    (* IO.Put("T3Packet I/O Thread started. NOTE: replace with async dispatch.\n"); *)
    process_packet := Sema.Alloc();
    io_thread := ThreadExtra.PFork(AsyncDeliver, NIL, NIL, 120);

    t3_intr_timer      := Spy.Create("Intr packet arrived");
    checkpoint0 := Spy.Create("checkpoint 0");
    checkpoint1 := Spy.Create("checkpoint 1");
    checkpoint2 := Spy.Create("checkpoint 2");
    checkpoint3 := Spy.Create("checkpoint 3");
    (* ifq initialization *)
    ifq.ifq_head := NIL;
    ifq.ifq_tail := NIL;
    ifq.ifq_len := 0;
    ifq.ifq_maxlen := If.MAXLEN;
    ifq.ifq_drops := 0;

    (* reset procedure variable in sal/standalone/net/if_ethersubr.c *)
    (* The following should be done by the lance.m3 module. *)
    T3PacketExtern.t3_input := Arrived;
    (* IO.Put("T3Packet module initialized.\n"); *)
  END Init;


BEGIN
END T3Packet.

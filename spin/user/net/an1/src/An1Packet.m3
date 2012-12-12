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
 * 20-Apr-96  Wilson Hsieh (whsieh) at the University of Washington
 *	PROCANY changes
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
UNSAFE MODULE An1Packet;

IMPORT An1; (* an1net event name  *)
IMPORT An1PktFormat;
IMPORT Net; (* networking support  *)
IMPORT Mbuf; (* unix like mbuf structure *)
IMPORT ThreadExtra, CPUPrivate, Machine;  (* thread, spl *)
IMPORT Thread;
IMPORT Sema;
IMPORT SpinException;
(* IMPORT Word;*)
IMPORT IO; (* libm3s *)
IMPORT Spy; (* timing *)
IMPORT Ctypes;
(* IMPORT Strand; *)
IMPORT If;

(* IMPORT Trap_internal; *)


(* DynShell support *)
IMPORT Text,SafeConvert;
IMPORT Dispatcher;
IMPORT SpinShell;

PROCEDURE EC_Guard(argc: CARDINAL; READONLY argv: ARRAY OF TEXT): BOOLEAN =
  BEGIN
    RETURN argc >= 1 AND Text.Equal(argv[0], "An1Packet");
  END EC_Guard;

PROCEDURE RunCommand(argc: CARDINAL; READONLY argv: ARRAY OF TEXT): BOOLEAN =
  VAR
  BEGIN
    IF argc > 1 AND Text.Equal(argv[1], "synctoggle") THEN
      async_dispatch := NOT async_dispatch; 
      IF async_dispatch THEN 
        IO.Put("An1Packet sync -> async\n");
      ELSE
        IO.Put("An1Packet async -> sync\n");
      END;
    ELSIF argc > 1 AND Text.Equal(argv[1], "debug") THEN
      debug_level := Net.MapDebug(SafeConvert.Atoi(argv[2]));
    ELSE
      IO.Put("An1Packet.RunCommand: no such command ");
      FOR i := 0 TO argc-1 DO
        IO.Put(argv[i]);
      END;
      IO.Put("\n");
    END;
    RETURN TRUE;
  END RunCommand;


(********************************************************************************)
(* synchronization and queue for incoming packets *)

VAR
  process_packet: Sema.T;
  (* toggle between asynchronous and synchronous dispatching *)
  async_dispatch       : BOOLEAN := TRUE;
  (* various timers *)
  an1_intr_timer     : Spy.T;
  checkpoint0: Spy.T;
  checkpoint1: Spy.T;
  checkpoint2: Spy.T;
  checkpoint3: Spy.T;
  dataflow   := Spy.Create("IntrHandler -> Deliver");
  debug_level          : Net.Level := Net.oLevel.NODEBUG;
  io_thread            : Thread.T;
  ifq                  : If.ifqueue;

(*
 * Arrived()
 * 
 * Takes an incoming packet from the an1net and pushes it up through
 * the protocol decission tree.
 *
 * For asynchronous dispatch enqueue the packet and kick the worker
 * thread.  Otherwise, the event handlers are invoked as part of the
 * interrupt handler.  This support will be supported and checked by
 * the SPIN dispatcher.
 *
 * Executed at CPU.InterruptLevel.High.
 *)

PROCEDURE Arrived(<* UNUSED *> ifp: UNTRACED REF If.ifnet;
  ehp: An1PktFormat.T; 
  m: Mbuf.T; 
  consumed: Ctypes.int) =
  CONST
    header_len = BYTESIZE(An1PktFormat.Header);
  VAR
    packet : Mbuf.T;
  BEGIN
    IF consumed = 0 (* by sal networking *) THEN

      (* start the timer *)
      (* Spy.Enter(an1_intr_timer);*)

      packet := Mbuf.m_get(Mbuf.M_WAIT,Mbuf.MT_DATA);
      packet.mh_hdr.mh_len  := header_len;
      packet.mh_hdr.mh_next := m;
      WITH eth_buf = Mbuf.Array(packet),
           eth = VIEW(eth_buf^, An1PktFormat.NewT)
       DO
        eth.shost := ehp.shost;
        eth.dhost := ehp.dhost;
        eth.type  := ehp.type;
      END;

      (* XXX bad that we have to copy data to queue an1 packet
         this is because the CPUPrivatef/1 device driver wont stick the
         an1 header into a separate mbuf.
      *)
      packet.mh_hdr.mh_timer[0] := Spy.rpcc();

      (* Spy.Enter(checkpoint0); *)

      IF async_dispatch = TRUE THEN
        If.Enqueue(ifq,packet);
        (* kick the I/O thread. *)
        Sema.V(process_packet);
        (* Spy.Exit(an1_intr_timer); *)
      ELSE
        (* Spy.Exit(an1_intr_timer); *)
        (* Deliver the packet to the next level directly from the interrupt handler. *)
        Deliver(packet);
      END;
      (* stop the timer *)
    ELSE
      Mbuf.m_freem(packet);
    END;
  END Arrived; 

PROCEDURE AsyncDeliver(<*UNUSED*>arg: ThreadExtra.ArgT) : ThreadExtra.ResultT =
  VAR packet: Mbuf.T;
      spl: CPU.InterruptLevel;

  BEGIN
    LOOP
      Sema.P(process_packet);
      (* Spy.Exit(Trap_internal.deviceTimer); *)
      (* dequeue operations has to be atomic wrt to the an1 interrupt handler *)
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
      Net.Debug(debug_level,Net.oLevel.INFO,"An1Packet Deliver ");
    END;

    WITH payload = Mbuf.Array(packet),
         data_pkt = packet.mh_hdr.mh_next
      (* stop = Spy.rpcc()*)
     DO

      (* hack to deal with sal networking modifications to mbuf *)
      IF data_pkt.mh_hdr.mh_len = 0 AND data_pkt.mh_hdr.mh_next # NIL THEN 
        data_pkt := data_pkt.mh_hdr.mh_next;
      END;
      (* 
         Spy.Hit(dataflow,packet.mh_hdr.mh_timer[0],stop);
         data_pkt.mh_hdr.mh_timer[0] := stop;
      *)

      TRY
        TRY
          EVAL An1.PacketArrived(data_pkt,payload);
        EXCEPT
          SpinException.Exception =>
        END;
      FINALLY
        (* Spy.Exit(checkpoint2); *)
        (* Spy.Enter(checkpoint3); *)
        Mbuf.m_freem(packet);
        (* Spy.Exit(checkpoint3); *)
      END;
    END;
  END Deliver;

(*
 * Init()
 * Used to initialize this module, since we cannot rely on M3
 * initialization order.
 *)
VAR
  shell: Dispatcher.Spindle;
PROCEDURE Init() = 
  BEGIN
    shell := Dispatcher.Install(SpinShell.RunCommand, EC_Guard, RunCommand);

    (* create an1 packet thread *)
    IO.Put("An1Packet I/O Thread started. NOTE: replace with async dispatch.\n");
    io_thread := ThreadExtra.PFork(AsyncDeliver, NIL);

    an1_intr_timer      := Spy.Create("Intr packet arrived");
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

    (* create the sema used to sync with the thread *)
    process_packet := Sema.Alloc();

    (* reset procedure variable in sal/standalone/net/if_an1subr.c *)
    (* The following should be done by the lance.m3 module. *)
    an1_input := Arrived;

  END Init;


BEGIN
END An1Packet.

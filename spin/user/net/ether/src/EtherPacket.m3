(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 31-May-97  David Becker at the University of Washington
 *      Replaced If interface with NetDev/EtherDev
 *
 * 09-Apr-97  Tsutomu Owa (owa)  at the University of Washington
 *	Get packets from user/stcp.
 *
 * 22-Nov-96  becker at the University of Washington
 *	Change to TrackStrand from Spy
 *
 * 19-Oct-96  Marc Fiuczynski (mef) at the University of Washington
 *	Updated to work with FreeBSD SAL.
 *
 * 14-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	New interface to interrupt handling.
 *
 * 03-Jun-96  Emin Gun Sirer (egs) at the University of Washington
 *	Took out a diagnostic printf which was telling us that there were no
 *	handlers for an event that was raised (a severe performance penalty).
 *	The dispatcher should be fixed to be able to enable this printout 
 *      again.
 *
 * 07-May-96  Marc Fiuczynski (mef) at the University of Washington
 *	Thread priority schedulig may have a bug. Lets just run at
 *	regular priority
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
UNSAFE (* for CPUPrivate.SetInterruptMask *)
MODULE EtherPacket;

IMPORT Ether, EtherPktFormat, Net, Mbuf, MbufPublic, Thread,
       ThreadExtra, CPUPrivate, CPU, Sema,
       SpinException, IO, Spy, TrackStrand, If, Ctypes,
       ParseParams;
IMPORT Word; <* NOWARN *>
IMPORT EtherDev;
IMPORT ProfileSupport;

CONST
  DispatcherIsFixedSoWeCanEnableDiagnosticPrints = TRUE;

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
      ELSIF pp.testNext("-profile") THEN
        async_profile := NOT async_profile;
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
  async_profile    : BOOLEAN := FALSE;
  (* various timers *)
  ether_intr_timer : Spy.T;
  ether_event      : Spy.T;
  ether_mbuf_free  : Spy.T;
  ether_output     : Spy.T;
  dataflow         : Spy.T;
  debug_level      : Net.Level := Net.oLevel.NODEBUG;
  io_thread        : Thread.T;
  ifq              : If.ifqueue;

CONST
  debug = FALSE;
  timing = FALSE;


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
 * Executed at CPU.InterruptLevel.High.
 *)

PROCEDURE Arrived( dev: EtherDev.T; m: Mbuf.T) =
  <*UNUSED*>
  CONST
    header_len = BYTESIZE(EtherPktFormat.Header);
  BEGIN
    MbufPublic.SetPktHdrRcvIf(m,dev);
    IF async_dispatch = TRUE THEN
      If.Enqueue(ifq,m);
      (* kick the I/O thread. *)
      Sema.V(process_packet);
      IF timing THEN Spy.Exit(ether_intr_timer); END;
    ELSE
      IF timing THEN Spy.Exit(ether_intr_timer); END;
      (* Deliver the packet to the next level directly 
         from the interrupt handler. *)
      Deliver(m);
    END;
  END Arrived; 

PROCEDURE AsyncDeliver(<*UNUSED*>arg: ThreadExtra.ArgT) : ThreadExtra.ResultT =
  VAR packet: Mbuf.T;
      spl: CPU.InterruptLevel;
  BEGIN
    LOOP
      Sema.P(process_packet);
      (* Spy.Exit(Trap_internal.deviceTimer); *)
      (* dequeue operations has to be atomic wrt to the ether interrupt handler *)
      IF async_profile THEN EVAL ProfileSupport.On(TRUE); END;
      spl := CPUPrivate.SetInterruptMask(CPUPrivate.InterruptClass.High);
      packet := If.Dequeue(ifq);
      CPUPrivate.RestoreInterruptMask(spl);        
      Deliver(packet);
      IF async_profile THEN EVAL ProfileSupport.Off(); END;
    END;
  END AsyncDeliver;

(*
 * Deliver()
 *      Pushes packet up to the next level in the protocol decission tree.
 *)
PROCEDURE Deliver(packet:Mbuf.T) = 
  BEGIN
    IF debug AND debug_level # Net.oLevel.NODEBUG THEN
      Net.Debug(debug_level,Net.oLevel.INFO,"EtherPacket Deliver ");
    END;

    (* NOT ON IX86_SPIN 
    IF timing THEN 
      WITH timer0 = VIEW(packet.mh_hdr.mh_union[0],INTEGER),
           timer1 = VIEW(packet.mh_hdr.mh_union[1],INTEGER),
           stop = SAL.Timestamp()
       DO 
        Spy.Hit(dataflow,timer1,stop);
        timer0 := stop;
        timer1 := stop;
      END;
    END;
    *)

    TRY
      TRY
        IF timing THEN Spy.Enter(ether_event); END;
        EVAL Ether.PacketArrived(packet,packet,0);          
        IF timing THEN Spy.Exit(ether_event); END;
      EXCEPT
        SpinException.Exception(ec) =>
        IF DispatcherIsFixedSoWeCanEnableDiagnosticPrints THEN
          IO.Put("EtherPacket caught ");
          IO.Put(ec.msg);
          IO.Put(" exception\n");
        END;
      END;
    FINALLY
      IF timing THEN Spy.Enter(ether_mbuf_free); END;
      Mbuf.m_freem(packet);
      IF timing THEN Spy.Exit(ether_mbuf_free); END;

    END;
  END Deliver;
    
PROCEDURE Output(dev: EtherDev.T; m: Mbuf.T;): Ctypes.int = 
  VAR s : CPU.InterruptLevel;
  BEGIN

    IF timing THEN Spy.Enter(ether_output); END;

    (* XXX Would a LOCK do here instead of spl?  db *)
    s := CPUPrivate.SetInterruptMask(CPUPrivate.InterruptClass.IO);
    dev.send(m);

    CPUPrivate.RestoreInterruptMask(s);

    IF timing THEN Spy.Exit(ether_output); END;

    RETURN 0;
  END Output;

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
    io_thread := ThreadExtra.PFork(AsyncDeliver, NIL(* XXX , NIL, ThreadExtra.defaultPriority+2*));

    TrackStrand.SetName(ThreadExtra.GetTracker(io_thread),"EtherThread");
    IF timing THEN
      ether_intr_timer := Spy.Create("EtherPacket.Arrived");
      ether_event := Spy.Create("EtherPacket Event");
      ether_mbuf_free := Spy.Create("EtherPacket m_freem");
      ether_output := Spy.Create("EtherPacket output");
      dataflow    := Spy.Create("EtherIntr -> Deliver");
    END;

    (* ifq initialization *)
    ifq.ifq_head := NIL;
    ifq.ifq_tail := NIL;
    ifq.ifq_len := 0;
    ifq.ifq_maxlen := If.MAXLEN;
    ifq.ifq_drops := 0;

  END Init;

BEGIN
END EtherPacket.

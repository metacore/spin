(* Copyright (C) 1993, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*| Last modified on Thu Jun 10 16:08:20 PDT 1993 by kalsow  *)
(*|      modified on Wed Jun  2 15:00:17 PDT 1993 by muller  *)
(*|      modified on Wed Apr 21 13:14:37 PDT 1993 by mcjones *)
(*|      modified on Wed Mar 10 11:01:47 PST 1993 by mjordan *)
(*|      modified on Tue Mar  9 08:45:18 PST 1993 by jdd     *)

(*
 * HISTORY
 * 06-May-97  Przemek Pardyak (pardy) at the University of Washington
 *      Made the visitor object untraced to avoid use of traced
 *	heap to be able to visit it.
 *
 * 14-Nov-96  Przemek Pardyak (pardy) at the University of Washington
 *	verbose is a level of verbosity.
 *
 * 13-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Changed to new interrupt scheme including both classes and levels.
 *
 * 14-Jun-96  Przemek Pardyak (pardy) at the University of Washington
 *	Got rid of typeidx.
 *
 * 17-Feb-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added tracing and statistics of collector operations and ambiguous
 *	roots.
 *
 * 23-Jan-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added spl-high around InvokeMonitors to be able to run them
 *	atomically at times other then garbage collection.
 *
 * 13-Nov-95  Przemek Pardyak (pardy) at the University of Washington
 *	Changed calls to RTType.Get to calls to RTType.GetIdx (see RTType.i3).
 *)

UNSAFE MODULE RTHeapRep;

IMPORT RT0u, RTType, RTMisc, RTOSMachine, Word, RT0;
IMPORT RTIO, RTHeapStats;

PROCEDURE UnsafeGetShape (r: REFANY;  VAR nDims: INTEGER;  VAR s: ArrayShape) =
  VAR def := RTType.Get (TYPECODE (r));
  BEGIN
    nDims := def.nDimensions;
    IF nDims # 0 THEN
      s := LOOPHOLE(LOOPHOLE(r, ADDRESS) + ADRSIZE(ADDRESS), ArrayShape);
    END;
  END UnsafeGetShape;

(*-------------------------------------------------------------- monitors ---*)

TYPE
  PublicMonitorClosure = UNTRACED ROOT OBJECT
                         METHODS
                           before ();
                           after  ();
                         END;

REVEAL
  MonitorClosure =
    PublicMonitorClosure BRANDED "RTHeap.MonitorClosure" OBJECT
      next, prev: MonitorClosure;
    OVERRIDES
      before := Noop;
      after  := Noop;
    END;

VAR monitorsHead, monitorsTail: MonitorClosure;

PROCEDURE InvokeMonitors (before: BOOLEAN) =
  VAR 
    m: MonitorClosure;
    spl: RTOSMachine.InterruptLevel;
  BEGIN
    spl := RTOSMachine.SetInterruptMask(RTOSMachine.InterruptClass.High);
    IF before THEN
      m := monitorsHead;
      WHILE m # NIL DO m.before(); m := m.next; END;
    ELSE
      m := monitorsTail;
      WHILE m # NIL DO m.after(); m := m.prev; END;
    END;
    RTOSMachine.RestoreInterruptMask(spl);
  END InvokeMonitors;

PROCEDURE RegisterMonitor (cl: MonitorClosure) =
  BEGIN
    cl.next := monitorsHead;
    IF monitorsHead = NIL THEN
      monitorsTail := cl;
    ELSE
      monitorsHead.prev := cl;
    END;
    monitorsHead := cl;
  END RegisterMonitor;

PROCEDURE UnregisterMonitor (cl: MonitorClosure) =
  BEGIN
    IF cl = monitorsHead THEN
      IF cl = monitorsTail THEN
        monitorsHead := NIL;
        monitorsTail := NIL;
      ELSE
        monitorsHead := monitorsHead.next;
        monitorsHead.prev := NIL;
      END;
    ELSE
      IF cl = monitorsTail THEN
        monitorsTail := monitorsTail.prev;
        monitorsTail.next := NIL;
      ELSE
        cl.prev.next := cl.next;
        cl.next.prev := cl.prev;
      END;
    END;
  END UnregisterMonitor;

PROCEDURE Noop (<*UNUSED*> cl: MonitorClosure) =
  BEGIN
  END Noop;

(*-------------------------------------------------------- initialization ---*)

PROCEDURE CheckTypes () =
  (* called by RTType.Init after type registration, but before any
     allocation *)
  VAR
    is_power: ARRAY [0 .. 8] OF BOOLEAN;
    size    : INTEGER;
  BEGIN
    (* check that it is safe to eliminate the #A call to upper ... *)
    FOR i := 0 TO RT0u.nTypes - 1 DO
      IF RTType.IsValid(i) THEN
        WITH def = RTType.Get (i)^ DO
          IF (def.traced # 0) AND (def.nDimensions = 0) THEN
            size := def.dataSize;
            <*ASSERT size = RTMisc.Upper (size, BYTESIZE (Header)) *>
          END;
        END;
      END;
    END;

    (* compute the small powers of two *)
    FOR i := FIRST(is_power) TO LAST(is_power) DO is_power[i] := FALSE END;
    is_power[1] := TRUE;
    is_power[2] := TRUE;
    is_power[4] := TRUE;
    is_power[8] := TRUE;

    (* check that all data alignments are small powers of two so that
       "RTMisc.Align (addr, alignment)" can be safely replaced by "addr +
       align [Word.And (addr, 7), alignment]" in Gcalloc.*)
    FOR i := 0 TO RT0u.nTypes - 1 DO
      IF RTType.IsValid(i) THEN
        WITH def = RTType.Get (i)^ DO
          IF (def.traced # 0) THEN
            <*ASSERT is_power [def.dataAlignment] *>
          END;
        END;
      END;
    END;
  END CheckTypes;

(* 
 * Reference tracing
 *)

PROCEDURE ObjectTenured(<*UNUSED*>ref: REFANY) =
  BEGIN
  END ObjectTenured;

PROCEDURE ObjectUntenured(<*UNUSED*>ref: REFANY) =
  BEGIN
  END ObjectUntenured;

PROCEDURE ObjectAllocated(ref: REFANY; pc: ADDRESS) =
  VAR tc: RT0.Typecode;
      addr := LOOPHOLE (ref, ADDRESS);
      size: CARDINAL;
      rh: RefHeader;
  BEGIN
    <* ASSERT ref # NIL *>
    rh := LOOPHOLE (addr-BYTESIZE(Header), RefHeader);
    tc := rh.typecode;
    size := ReferentSize (rh) + BYTESIZE (Header);

    RTHeapStats.AllocateAddr (addr, tc, size);
    RTHeapStats.AllocatePC (pc, size);
    RTHeapStats.UpdateClock (size);
  END ObjectAllocated;

PROCEDURE ObjectDeallocated(ref: Word.T) =
  VAR tc: RT0.Typecode;
      addr := LOOPHOLE (ref, ADDRESS);
      rh: RefHeader;
      size: CARDINAL;
  BEGIN
    <* ASSERT ref # 0 *>
    rh := LOOPHOLE (addr-BYTESIZE(Header), RefHeader);
    tc := rh.typecode;
    size := ReferentSize (rh) + BYTESIZE (Header);
    RTHeapStats.DeallocateAddr (addr, tc, size);
  END ObjectDeallocated;

PROCEDURE ObjectPromoted(ref: REFANY;
                         <* UNUSED *> stage: INTEGER; 
                         <* UNUSED *> ptr: ADDRESS;
                         <* UNUSED *> loc: ADDRESS;
                         <* UNUSED *> src: REFANY) =
  VAR tc: RT0.Typecode;
      addr := LOOPHOLE (ref, ADDRESS);
  BEGIN
    <* ASSERT ref # NIL *>
    tc := LOOPHOLE (addr-BYTESIZE(Header), RefHeader).typecode;
    RTHeapStats.ImplicitMoveAddr (addr, tc);
  END ObjectPromoted;

PROCEDURE ObjectMoved(before: Word.T; after: REFANY;
                      <* UNUSED *> state: INTEGER) =
  VAR tc: RT0.Typecode;
      baddr := LOOPHOLE (before, ADDRESS);
      addr := LOOPHOLE (after, ADDRESS);
  BEGIN
    <* ASSERT before # 0 AND after # NIL *>
    tc := LOOPHOLE (addr-BYTESIZE(Header), RefHeader).typecode;
    RTHeapStats.MoveAddr (baddr, addr, tc);
  END ObjectMoved;

PROCEDURE GCStarted() =
  BEGIN
    IF verbose > 0 THEN
      RTIO.PutText ("<<GC start<<");
    END;
    RTHeapStats.ResetDeallocation ();
  END GCStarted;

PROCEDURE GCDone() =
  BEGIN
    IF verbose > 0 THEN
      RTIO.PutText (">>GC done>>");
    END;
    RTHeapStats.Collection ();
  END GCDone;

PROCEDURE GCEnter() =
  BEGIN
  END GCEnter;

PROCEDURE GCExit() =
  BEGIN
  END GCExit;

BEGIN
END RTHeapRep.


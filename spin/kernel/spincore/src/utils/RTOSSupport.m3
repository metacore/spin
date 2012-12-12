(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * Service and support functions for the M3 Runtime.
 * Most of these functions are defined within interfaces that live
 * over in the runtime.
 *
 * HISTORY
 * 19-Nov-97  Tian Fung Lim (tian) at the University of Washington
 *	Added TimeOfDay.
 *
 * 15-Jul-97  Tian Fung Lim (tian) at the University of Washington
 *	Added interface for cycle ops for fine grain measurement of
 *	runtime.
 *
 * 31-May-97  David Becker at the University of Washington
 *	Replace SAL with Sal and CPU interfaces
 *	Removed all Get/SetTrace* procs
 *
 * 19-Mar-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added CompareAndSwapAddr and Yield.
 *
 * 14-Nov-96  Przemek Pardyak (pardy) at the University of Washington
 *	Picked up some VM code from Tian for growing heap.
 *
 * 14-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	New interface to interrupt handling.
 *
 * 21-Apr-96  Emin Gun Sirer (egs) at the University of Washington
 *	Added CompareAndSwap.
 *
 * 18-Apr-96  Przemek Pardyak (pardy) at the University of Washington
 *	Replaced a call to SafeConvert with a call to M3toC to convert
 *	"TEXT" to "char *".  It is a better idea to call an unsafe
 *	interface in a safe way than do allocation on the patho of the 
 *	lowest level printing routine.
 *
 * 02-Apr-96  Przemek Pardyak (pardy) at the University of Washington
 *	Do not go to the debugger when cannot get more memory for the traced
 *	heap.  Let GC deal with the problem.
 *
 * 20-Nov-95  Emin Gun Sirer (egs) at the University of Washington
 *	Added synchronization primitives for the runtime.
 *
 * 27-Oct-95  Brian Bershad (bershad) at the University of Washington
 *	Created.
 *
 *)
UNSAFE (* cast address into char_star *)
MODULE RTOSSupport EXPORTS RTOS, RTMem, RTSynch, RTOSMachine;
IMPORT AtomicOps, SafeConvert, Word, M3toC, RTHeapRep;
IMPORT CPU, CPUPrivate, Sal, RTIO;
IMPORT SalExtern, MachineMem, VirtAddr, PhysAddr, Protection;
IMPORT PhysAddrPrivate, PhysAddrRep, VMError, ThreadExtra;
IMPORT Clock;

VAR
  tracedStart, tracedEnd, tracedCurrent: ADDRESS;

PROCEDURE Inc(VAR a: INTEGER; delta: INTEGER := 1) : INTEGER =
  BEGIN
    RETURN AtomicOps.AtomicInc(a, delta);
  END Inc;

PROCEDURE Dec(VAR a: INTEGER; delta: INTEGER := 1) : INTEGER =
  BEGIN
    RETURN AtomicOps.AtomicDec(a, delta);
  END Dec;

PROCEDURE IncAddr(VAR a: ADDRESS; delta: INTEGER) : ADDRESS =
  BEGIN
    RETURN AtomicOps.AtomicIncAddr(a, delta);
  END IncAddr;

PROCEDURE DecAddr(VAR a: ADDRESS; delta: INTEGER) : ADDRESS =
  BEGIN
    RETURN AtomicOps.AtomicDecAddr(a, delta);
  END DecAddr;

<*UNUSED*>PROCEDURE CompareAndSwapAddr(VAR a: ADDRESS; old, new: ADDRESS)
  : BOOLEAN =
  BEGIN
    RETURN AtomicOps.CompareAndSwapAddr(a, old, new);
  END CompareAndSwapAddr; 

<*UNUSED*>PROCEDURE CompareAndSwap(v: REF REFANY; old, new: REFANY) : BOOLEAN =
  BEGIN
    RETURN AtomicOps.CompareAndSwap(v, old, new);
  END CompareAndSwap;

<*UNUSED*>PROCEDURE CompareAndSwapInt(VAR v:INTEGER; old,new:INTEGER):BOOLEAN =
  BEGIN
    RETURN AtomicOps.CompareAndSwapInt(v, old, new);
  END CompareAndSwapInt;

PROCEDURE Print(READONLY text: TEXT) =
  VAR
      t : TEXT := text;
  BEGIN
    IF t = NIL THEN
      t := ">> NIL <<";
    END;
    (* The call to M3toC grabs a pointer to the character string representing
       the TEXT value.  This is OK because this pointer will be passed through
       the printing routines without storing and the TEXT will be not moved
       by the GC because it is kept in this activation record *)
    Prints(M3toC.TtoS(t));
  END Print;

PROCEDURE Error(READONLY t: TEXT) =
  BEGIN
    Print("ERROR >>>");
    Print(t);
  END Error;

PROCEDURE Printi(i: INTEGER) =
  BEGIN
    Sal.Printf(M3toC.TtoS("%ld"),i);
  END Printi;

PROCEDURE Printx(x: Word.T) =
  BEGIN
    Sal.Printf(M3toC.TtoS("0x%lx"),x);
  END Printx;

PROCEDURE Printc(c: CHAR) =
  BEGIN
    Sal.PutChar(c);
  END Printc;

PROCEDURE Prints(s: ADDRESS) =
  BEGIN
    Sal.Printf(s, (* UNUSED *) 0);
  END Prints;

(* Spin malloc function wrapper *)

PROCEDURE Alloc(size: INTEGER): ADDRESS =
  BEGIN
    RETURN Sal.UntracedAlloc(size);
  END Alloc;

PROCEDURE Align(number: Word.T) : Word.T =
  VAR
    newnum : Word.T;
  BEGIN
    newnum := Word.Plus(number,7);
    newnum := Word.And(newnum,Word.Not(7));
    RETURN newnum;
  END Align;

(*
 * Careful what services we use here. No NEW.
 *)

PROCEDURE AllocGC(VAR size: Word.T) : ADDRESS = 
  VAR oldCurrent, newCurrent, oldEnd : ADDRESS;
      spl: CPU.InterruptLevel;
      newpage: PhysAddr.T;
      prot: Protection.T;
      numPages : CARDINAL;
      oldCurr, start, end : VirtAddr.Address;
      AllocMap : PhysAddr.Address;
  BEGIN
    IF RTHeapRep.verbose > 0 THEN
      RTIO.PutText("AllocGC >> called\n");
      RTIO.Flush();
    END;
 
    spl := CPUPrivate.SetInterruptMask(
               CPUPrivate.InterruptClass.High);

    oldCurrent := tracedCurrent;
    newCurrent := oldCurrent + size;
    oldEnd     := tracedEnd;
    
    IF (newCurrent <= oldEnd) THEN
      (* allocate memory from already mapped pool *)
      tracedCurrent := newCurrent;
      CPUPrivate.RestoreInterruptMask(spl);
      IF RTHeapRep.verbose > 0 THEN
        RTIO.PutText("AllocGC >> allocating "); RTIO.PutInt(size); 
        RTIO.PutText(" (0x"); RTIO.PutHex(size); RTIO.PutText(")");
        RTIO.PutText(" bytes at ");
        RTIO.PutHex(SafeConvert.AdrToWord(oldCurrent)); RTIO.PutText(" - ");
        RTIO.PutHex(SafeConvert.AdrToWord(newCurrent)); 
        RTIO.PutText("\n");
        RTIO.Flush();
      END;
      RETURN oldCurrent;
    ELSE
      (* map a little more memory after tracedEnd *)
      TRY
        (*
        IF PhysAddr.FreeMem() < 800 THEN
          RTIO.PutText("free mem getting critical, free pages = ");
          RTIO.PutInt(PhysAddr.FreeMem());
          (*DebugOption.PhysAddr := TRUE;*)
        END;
        *)

        prot := Protection.T{TRUE, TRUE, FALSE, 0};
        start := LOOPHOLE(oldEnd, Word.T);
        oldCurr := LOOPHOLE(oldCurrent, Word.T);

        IF MachineMem.RoundToPage(oldCurr) # oldCurr THEN
          RTIO.PutText("ERROR >> AllocGC: oldCurrent not page aligned: 0x");
          RTIO.PutHex(oldCurr); RTIO.PutText(" ");
          RTIO.PutHex(MachineMem.RoundToPage(oldCurr)); RTIO.PutText("\n");
        END;

        (* Increase heap by just the amount needed *)
        end := MachineMem.RoundToPage(Word.Plus(oldCurr, size));
        numPages := MachineMem.BytesToPages(Word.Minus(end, start));
        
        (* Acquire and map the pages *)
        AllocMap := SalExtern.kernel_pmap;
        (*        EncapInfo.PVDEBUG :=1;*)

        IF RTHeapRep.verbose > 0 THEN
          RTIO.PutText("First address mapped = ");
          RTIO.PutHex(start);
        END;

        FOR i := 0 TO numPages-1 DO
          (* NIL tag => pinned page *)
          newpage := PhysAddrPrivate.AllocatePinnedPage();

          IF RTHeapRep.verbose > 0 THEN
            RTIO.PutText ("{");
            RTIO.PutHex(start); 
            RTIO.PutText ("|");
            RTIO.PutHex(newpage.addr); 
            RTIO.PutText ("}");
          END;

          SalExtern.pmap_enter(AllocMap, start, newpage.addr, 
                            prot, 1, prot);
          start := start + MachineMem.PAGESIZE;
        END;

        (*EncapInfo.PVDEBUG :=0;*)

        tracedEnd := oldEnd + (MachineMem.PAGESIZE * numPages);
        tracedCurrent := tracedCurrent + size;
        CPUPrivate.RestoreInterruptMask(spl);
        
        IF RTHeapRep.verbose > 0 THEN
          RTIO.PutText("\nLast address mapped = ");
          RTIO.PutHex(start);
          RTIO.PutText("\n new traced current = ");
          RTIO.PutHex(SafeConvert.AdrToWord(tracedCurrent));
          RTIO.PutText("\n new traced end = ");
          RTIO.PutHex(SafeConvert.AdrToWord(tracedEnd));
          RTIO.PutText("\n Physfree free pages = ");
          RTIO.PutInt(PhysAddr.FreeMem());
          RTIO.Flush();
        END;
        
        RETURN oldCurrent;
      EXCEPT
      | VMError.E => 
        CPUPrivate.RestoreInterruptMask(spl);

        RTIO.PutText("AllocGC: unable to grow traced heap\n"); 
        RTIO.PutText("\tleftover free memory: ");
        RTIO.PutHex(SafeConvert.AdrToWord(tracedCurrent)); 
        RTIO.PutText(" - ");
        RTIO.PutHex(SafeConvert.AdrToWord(tracedEnd)); 
        RTIO.PutText("; requested ");
        RTIO.PutHex(size);
        RTIO.PutText(" bytes\n");
        RTIO.PutText("\tPhysFree has ");
        RTIO.PutInt(PhysAddr.FreeMem());
        RTIO.PutText(" pages free\n");
        RETURN NIL;
      END;
    END;
  END AllocGC;

PROCEDURE GetMaxGCSize() : INTEGER =
  BEGIN
    tracedStart := Sal.GetTracedStart();
    tracedEnd := Sal.GetTracedEnd();
    tracedCurrent := tracedStart;

    IF RTHeapRep.verbose > 0 THEN
      RTIO.PutText("GetMaxGCSize >> heap limits: ");
      RTIO.PutHex(SafeConvert.AdrToWord(tracedStart));
      RTIO.PutText(" - ");
      RTIO.PutHex(SafeConvert.AdrToWord(tracedEnd)); 
      RTIO.PutText("; size: ");
      RTIO.PutInt(tracedEnd - tracedCurrent); 
      RTIO.PutText(" (");
      RTIO.PutHex(tracedEnd - tracedCurrent);
      RTIO.PutText(") bytes\n");
    END;
    RETURN tracedEnd - tracedStart;
  END GetMaxGCSize;

PROCEDURE Yield () =
  BEGIN
    ThreadExtra.Yield();
  END Yield;


PROCEDURE GetTracedStart(): ADDRESS =
  BEGIN
    RETURN SalExtern.traced_start;
  END GetTracedStart; 

PROCEDURE GetTracedEnd(): ADDRESS =
  BEGIN
    RETURN SalExtern.traced_end;
  END GetTracedEnd; 

PROCEDURE SetTracedStart(a: ADDRESS) =
  BEGIN
    SalExtern.traced_start := a;
  END SetTracedStart; 

PROCEDURE SetTracedEnd(a: ADDRESS) =
  BEGIN
    SalExtern.traced_end := a;
  END SetTracedEnd; 


PROCEDURE CycleCounter(): Word.T =
  VAR
  BEGIN
    RETURN CPU.CycleCounter();
  END CycleCounter;
PROCEDURE CycleToMicrosec(cycles: Word.T) : Word.T =
  VAR
  BEGIN
    RETURN CPU.CycleToMicrosec(cycles);
  END CycleToMicrosec;
PROCEDURE CycleMinus(stop,start: Word.T): Word.T =
  BEGIN
    RETURN CPU.CycleMinus(stop,start);
  END CycleMinus;

PROCEDURE TimeOfDay () : Word.T =
  VAR
    now : Clock.TimeVal;
  BEGIN
    Clock.TimeOfDay(now);
    RETURN(now.tv_sec * 1000000 + now.tv_usec);
  END TimeOfDay;

BEGIN
END RTOSSupport.

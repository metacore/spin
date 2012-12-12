(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 31-May-97  David Becker at the University of Washington
 *	Use new Sal interfaces
 *
 * 02-Jan-97  Yasushi Saito (yasushi) at the University of Washington
 *	Removed the call to changestate in AddMapping. The caller should call
 *	that if necessary.
 * 27-Sep-96  Marc Fiuczynski (mef) at the University of Washington
 *	Bug fix in RemoveMapping's call to MachinePmap.RemoveRange
 *	provided by Tian.
 *
 * 14-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	XXX Commented out an architecture-dependent test in VerifyRange.  This
 *      needs to be done correctly at some point.
 *
 * 29-May-96  Stefan Savage (savage) at the University of Washington
 *	Fix to switch to SalTranslation when we destroy our current 
 *	translation
 *
 * 07-May-96  Stefan Savage (savage) at the University of Washington
 *	Minor changes + we currently are ignoring VirtAddr.T's (just require
 *	than virtual addresses not be in the Sal)
 *
 * 28-Apr-96  Stefan Savage (savage) at the University of Washington
 *	Moved ProtectionFault to Trap.m3
 *
 *  8-Apr-96 oystr at the University of Washington
 *      Documentation note:  VerifyRange will not complain if
 *      end < begin.  (I know somebody who called it that way one time.)
 *      Horrible things will subsequently happen and you will be thrown
 *      into the debugger.  Arguably, this behavior a *good* thing
 *      since it will lead immediately to the cause of the problem
 *      rather than getting a soft ``failure''.
 *
 * 18-Apr-96  Emin Gun Sirer (egs) at the University of Washington
 *	Wrote the default implementation of Protection Fault for user strands.
 *	We currently print something and block the strand. We'll page in the
 *      future.
 *
 * 14-Apr-96  Emin Gun Sirer (egs) at the University of Washington
 *	Moved the ProtectionFault event back to machine-specific directory.
 *      Cleaned up related dispatcher work-around.
 *
 * 02-Mar-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed ProtectionFault to call new FireWall.Fault function.
 *	Faults with a bad address in the pages addressable by the 16 bit
 *	ld/st offset are reflected as AttemptToDerefence NIL faults.  
 *	
 *
 * 17-Feb-95 Przemek Pardyak (pardy) at the University of Washington
 *	Measurements by Spy used only if DebugOption.DoTimings is on.
 *
 * 18-Jan-96 Przemek Pardyak (pardy) at the University of Washington
 *	Fixed a bug in the call to Dispatcher.KeepStub.
 *
 * 09-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	Use IO and reader/writers.
 *
 * 26-Dec-95  Emin Gun Sirer (egs) at the University of Washington
 *	Moved exception operations inside TRY blocks.
 *
 * 02-Dec-95 Przemek Pardyak (pardy) at the University of Washington
 *	Removed passing information about number of arguments 
 *	to the dispatcher.
 *
 * 01-Dec-95 Przemek Pardyak (pardy) at the University of Washington
 *	Made ProtectionFault raise exceptions.  Fixed procedure type
 *	mismatch in handler installation.  Marked unused definitions
 *	to disable warnings. 
 *
 * 28-Oct-95  Brian Bershad (bershad) at the University of Washington
 *	Made safe through the use of SafeConvert.
 *
 * 31-Oct-95  Wilson Hsieh (whsieh) at the University of Washington
 *	use REF PROCEDURE instead of PROCEDURE
 *      removed some dead code to get rid of warnings
 *
 * 02-Nov-94  Stefan Savage (savage) at the University of Washington
 *	Created
 *)
UNSAFE MODULE Translation EXPORTS Translation, TranslationPrivate;
IMPORT SalExtern, Ctypes;
IMPORT Strand, StrandRep, StrandExtern;
IMPORT TranslationRep, PhysAddr, PhysAddrPrivate, PhysAddrRep, VMError,
       VirtAddr, Word, Fmt,
       DebugOption, IO;
IMPORT Protection;
IMPORT CPU;
IMPORT Trap;
IMPORT Debugger;
FROM MachineMem IMPORT PAGESIZE;

VAR
  currentTranslation: T;
  kernelTranslation: T;
CONST DebugMessage = FALSE;
(*
 * BUGBUG Should take pool... remember to check for null
 *)
PROCEDURE Initialize(newMap: T) =
  BEGIN
    newMap.pagetableBase:= SalExtern.pmap_create(0); 
    IF DebugMessage THEN
      IO.Put("Creating new Translation Map (" & Print(newMap) &")\n");
    END;
  END Initialize;

PROCEDURE Destroy(map: T) =
  VAR base := map.pagetableBase;
  BEGIN
    IF DebugMessage THEN
      IO.Put("Destroying Translation Map (" & Print(map) & ")\n");
    END;
    (* Get us off this old map *)
    IF map = currentTranslation THEN
      Activate(kernelTranslation);
    END;
    map.pagetableBase := 0;
    SalExtern.pmap_destroy(base);
  END Destroy;

PROCEDURE Activate (map: T) =
  BEGIN
    IF currentTranslation # map THEN
      IF map.pagetableBase = 0 THEN
	(* this traslation is already destroyed. this may happen during
	   process exit. Don't be surprised. *)
	(* IO.Put("Translation.Activate: tried to activate stale pmap.\n"); *)
	map := kernelTranslation;
      END;
      currentTranslation:= map;
      SalExtern.Activate(map.pagetableBase); 
    END;
  END Activate;

FUNCTIONAL PROCEDURE GetCurrent (): T =
  BEGIN
    RETURN StrandExtern.Cur.translation;
  END GetCurrent;

PROCEDURE AddMapping (map: T; p: PhysAddr.T;
		      begin, end: VirtAddr.Page;
		      prot: Protection.T) 
  RAISES { VMError.E } =
  VAR
    vptr, pptr: Word.T;
  BEGIN
    (* pmap module is confused if "others" # 0. *)
    prot.others:= 0;
    
    IF DebugMessage THEN
      IO.Put("Enter mapping on T(" & Print(map) &") at addr: ("
	     & Fmt.Unsigned(begin) &
	     ", "&Fmt.Unsigned(end)& " to p(" & PhysAddr.Print(p) &
	     ") with protection: " & PrintProtection(prot) & "\n");
    END;

    PhysAddrPrivate.Lock(p);

    IF NOT p.valid THEN
      IF DebugMessage THEN
	IO.Put("AddMapping: stale frame@" & Fmt.Int(begin, 16) & ".\n");
      END;
      PhysAddrPrivate.Unlock(p);
      RAISE VMError.E(VMError.StaleFrame);
    END;
    
    IF DebugOption.Translation THEN
      IF p.state = PhysAddr.State.Free OR
	 p.state = PhysAddr.State.Transient THEN
	Debugger.Enter();
      END;
    END;

    IF p.state <= PhysAddr.State.Active THEN
      PhysAddrPrivate.ChangeStatePrivate(p, PhysAddr.State.Active);
    END;
    
    (* XXX need to check physaddr overrun!! *)
    vptr:= begin * PAGESIZE;
    end := end * PAGESIZE;
    pptr:= p.addr;
    WHILE vptr < end DO
      SalExtern.pmap_enter(map.pagetableBase, vptr, pptr, prot, 0,prot);
      vptr:= vptr + PAGESIZE;
      pptr:= pptr + PAGESIZE;
    END;
    PhysAddrPrivate.Unlock(p);
  END AddMapping;

VAR dummyss: CPU.SavedState;
    
PROCEDURE AccessRW(map: T; page, endPage: VirtAddr.Page;
		   closure: PROCEDURE(page: VirtAddr.Page;
				      VAR content: PhysAddr.Content))
  RAISES {VMError.E} =
  VAR
    pa: PhysAddr.Address;
    va: VirtAddr.Address;
    prot: Protection.T;
    counter:= 0;
    done: BOOLEAN;
  BEGIN
    page := page * PAGESIZE;
    endPage := endPage * PAGESIZE;
    WHILE page < endPage DO
      SalExtern.pmap_get_phys_and_protection(map.pagetableBase, page, pa, prot);
      IF prot.write THEN
	counter:= 0;
	(* Wire down the page. Note that we don't need to mask
	   interrupts during VirtToPhys and ChangeMapping, since
	   maps aren't changed by interrupt handlers. *)
	
	(* XXX readonly pages!! *)
	va:= SalExtern.phys_to_kseg(pa);
	
	closure(page, LOOPHOLE(va, UNTRACED REF PhysAddr.Content)^);
	
	INC(page, PAGESIZE);
      ELSE
	(* page in *)
	INC(counter);
	
	IF counter >= 20 THEN 
	  RAISE VMError.E(VMError.NoResponseFromPager);
	END;	 
	
	done:= FALSE;
	IF NOT prot.read THEN
	  (* page fault *)	
	  Trap.InvalidTranslation(NIL, dummyss,
					 map, page,
					 Trap.Write, done);
	ELSE
	  (* write on readonly page *)
	  Trap.ProtectionFault(NIL, dummyss,
				      map, page,
				      Trap.Write, done);
	END;
	
	IF NOT done THEN 
	  RAISE VMError.E(VMError.NoResponseFromPager);
	END;
      END;
    END;
  END AccessRW;

PROCEDURE AccessRO (map: T; page, endPage: VirtAddr.Page;
		    closure: PROCEDURE (page: VirtAddr.Address;
					READONLY content: PhysAddr.Content))
  RAISES {VMError.E} =
  VAR
    pa: PhysAddr.Address;
    va: VirtAddr.Address;
    prot: Protection.T;
    counter:= 0;
    done: BOOLEAN;
  BEGIN
    page := page * PAGESIZE;
    endPage := endPage * PAGESIZE;
    
    WHILE page < endPage DO
      SalExtern.pmap_get_phys_and_protection(map.pagetableBase, page, pa, prot);
      IF prot.read THEN
	counter:= 0;
	(* XXX race!!! *)
	va:= SalExtern.phys_to_kseg(pa);
	closure(page, LOOPHOLE(va, UNTRACED REF PhysAddr.Content)^);
	INC(page, PAGESIZE);
      ELSE
	(* page in *)
	INC(counter);
	
	IF counter >= 20 THEN 
	  RAISE VMError.E(VMError.NoResponseFromPager);
	END;	 
	
	(* page fault *)
	done:= FALSE;
	Trap.InvalidTranslation(NIL, dummyss,
				       map, page,
				       Trap.Write, done);
	IF NOT done THEN 
	  RAISE VMError.E(VMError.NoResponseFromPager);
	END;
      END;
    END;
  END AccessRO;

PROCEDURE Read (map: T; addr: VirtAddr.Address; VAR buf: ARRAY OF CHAR) =
  VAR
    th := Strand.GetCurrent();
    n := NUMBER(buf);
  BEGIN
    IF n = 0 THEN RETURN; END;
    IF th.translation # map THEN
      VAR org := th.translation;
      BEGIN
	th.translation := map;
	Activate(map);
	SalExtern.bcopy(LOOPHOLE(addr, Ctypes.void_star), ADR(buf[0]), n);
	th.translation := org;
	IF org # NIL THEN Activate(org); END;
      END;
    ELSE
      SalExtern.bcopy(LOOPHOLE(addr, Ctypes.void_star), ADR(buf[0]), n);
    END;      
  END Read;
PROCEDURE Write (map: T; READONLY buf: ARRAY OF CHAR; addr: VirtAddr.Address)=
  VAR th := Strand.GetCurrent();
    n := NUMBER(buf);
  BEGIN
    IF n = 0 THEN RETURN; END;
    IF th.translation # map THEN
      VAR org := th.translation;
      BEGIN
	th.translation := map;
	Activate(map);
	SalExtern.bcopy(ADR(buf[0]), LOOPHOLE(addr, Ctypes.void_star), n);
	th.translation := org;
	IF org # NIL THEN Activate(org); END;
      END;
    ELSE
      SalExtern.bcopy(ADR(buf[0]), LOOPHOLE(addr, Ctypes.void_star), n);
    END;      
  END Write;
  
PROCEDURE Access (map: T; addr, len: VirtAddr.Address;
		  proc: PROCEDURE (VAR content: ARRAY OF CHAR)) =
  VAR th := Strand.GetCurrent();
  BEGIN
    IF th.translation # map THEN
      VAR org := th.translation;
      BEGIN
	th.translation := map;
	Activate(map);
	proc(SUBARRAY(LOOPHOLE(addr, UNTRACED REF ARRAY [0 ..  16_FFFFFFFF] OF CHAR)^, 0, len));
	th.translation := org;
      END;
    ELSE
      proc(SUBARRAY(LOOPHOLE(addr, UNTRACED REF ARRAY [0 ..  16_FFFFFFFF] OF CHAR)^, 0, len));
    END;      
  END Access;
  
PROCEDURE RemoveMapping(map: T; from, end: VirtAddr.Page) =
  BEGIN
    IF DebugMessage THEN
      IO.Put("Removing mapping addr: (" & Fmt.Unsigned(from*PAGESIZE) &
	     ", "&Fmt.Unsigned(end*PAGESIZE)&"\n"); 
    END;
    SalExtern.pmap_remove(map.pagetableBase, from*PAGESIZE, end*PAGESIZE);
   END RemoveMapping;

PROCEDURE ExamineMapping(map: T; begin: VirtAddr.Page)
     : Protection.T =
  VAR
    prot: Protection.T;
    pa: PhysAddr.Address;
  BEGIN
    SalExtern.pmap_get_phys_and_protection(map.pagetableBase, begin*PAGESIZE, pa, prot);
    RETURN prot;
  END ExamineMapping; 

PROCEDURE ChangeMapping (map: T; begin, end: VirtAddr.Page; prot: Protection.T) =
  BEGIN
    (* pmap module is confused if "other" # 0. *)
    prot.others:= 0;
    SalExtern.pmap_protect(map.pagetableBase,
			      begin*PAGESIZE, end*PAGESIZE, prot);
  END ChangeMapping;

PROCEDURE PrintProtection(prot: Protection.T): TEXT =
  VAR
    newText: TEXT;
  BEGIN
    newText:= "";

    IF (prot.read = TRUE) THEN
      newText:= newText & "Read ";
    END;
    IF (prot.write = TRUE) THEN
      newText:= newText & "Write ";
    END;
    IF (prot.execute = TRUE) THEN
      newText:= newText & "Execute ";
    END; 
    RETURN newText;
  END PrintProtection;

PROCEDURE Print (map: T): TEXT =
  BEGIN
    RETURN(Fmt.Unsigned(map.pagetableBase));
  END Print;

PROCEDURE GetKernel(): T =
  BEGIN
    RETURN kernelTranslation;
  END GetKernel;

PROCEDURE Init(verbose: BOOLEAN) =
  BEGIN
    SalExtern.current_pmap := SalExtern.kernel_pmap;
    currentTranslation := NEW(T);
    currentTranslation.pagetableBase := SalExtern.kernel_pmap;
    kernelTranslation := currentTranslation;
    IF verbose THEN
      IO.Put("Translation initialized.  Sal Page Tables loaded at 0x" &
        Fmt.Unsigned(currentTranslation.pagetableBase) & "\n");
    END;
  END Init;

BEGIN
END Translation.

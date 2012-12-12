(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 29-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Separated from AddressSpace.m3
 *)

(*
 
 Page fault handler

 *)
MODULE PageFault;
IMPORT Debugger;
IMPORT DebugOption;
IMPORT IO;
IMPORT Fmt;
IMPORT VMError;
IMPORT VMTypes;
IMPORT VMStat;
IMPORT Strand;
IMPORT CPU;
IMPORT Translation;
IMPORT AddressSpace;
IMPORT AddressSpaceRep;
IMPORT Protection;
IMPORT AddressMapEntry;
IMPORT PhysAddr;
IMPORT Trap;
IMPORT MemoryObjectRep;
IMPORT VMDebug;
IMPORT Thread;
IMPORT Word;
IMPORT Spy;
IMPORT AddressMapQ;

VAR
  pfTime := Spy.Create("page-fault", FALSE, 200);
  
(* Pre: self is locked. *)
PROCEDURE MapLookup (self: AddressSpace.T; pageNum: VMTypes.PageNumber)
  : AddressMapEntry.T RAISES {VMError.E} =
  VAR
    itr: AddressMapQ.Iterator;
    entry: AddressMapEntry.T;
  BEGIN
    IF self.mapCache # NIL
      AND pageNum >= self.mapCache.from AND pageNum < self.mapCache.end THEN
      RETURN self.mapCache;
    END;
    
    itr := AddressMapQ.Iterate(self.maps);
    WHILE AddressMapQ.NextItr(itr, entry) DO
      IF pageNum >= entry.from AND pageNum < entry.end THEN
	self.mapCache := entry;
	RETURN entry;
      END;
    END;
    RAISE VMError.E(VMError.InvalidAddress);
  END MapLookup;
  
PROCEDURE Handler (<*UNUSED*>strand: Strand.T; 
		   <*UNUSED*>VAR ss: CPU.SavedState;
		   map: Translation.T;		    
		   addr: CPU.VirtAddress;
		   type: INTEGER;
		   VAR done : BOOLEAN) =
  VAR
    self := NARROW(map, AddressSpace.T);
    prot: Protection.T;
    entry: AddressMapEntry.T;
    page: VMTypes.PageNumber;
    frame: PhysAddr.T;
    mOff: VMTypes.PageNumber; (* mobj offset *)
  BEGIN
    IF done THEN RETURN; END;

    Spy.Enter(pfTime);
    
    IF VMDebug.DebugMessage THEN
      VAR typeName: TEXT;
      BEGIN
	CASE type OF
	| Trap.Execute => typeName := "exec";
	| Trap.Read => typeName := "read";
	| Trap.Write => typeName := "write";
	ELSE
	  typeName := "unknown(" & Fmt.Int(type) & ")";
	END;
	IO.Put("page fault@" & Fmt.Int(addr, 16) & "," & typeName & ".\n");
      END;
    END;

    IF VMStat.Enabled THEN
      INC(VMStat.fault);
    END;
    
    page := Word.Divide(addr, CPU.PAGESIZE);
    
    LOCK self.lock DO
      TRY
	entry := MapLookup(self, page);
	IF entry = NIL THEN
	  Spy.Exit(pfTime);
	  RETURN;
	END;
	
	IF DebugOption.PhysAddr AND page - entry.from + entry.mOff < 0 THEN
	  Debugger.Enter()
	END;

	mOff := page - entry.from + entry.mOff;
	IF entry.mObj.request(mOff, type, frame, prot) THEN
	  IF VMStat.Enabled THEN
	    IF PhysAddr.GetState(frame) = PhysAddr.State.Reclaimed THEN
	      INC(VMStat.react);
	    END;
	  END;
	  Translation.AddMapping(map, frame, page, page+1, prot);
	  entry.mObj.pager.pageMapNotify(mOff, self, page);
	  done := TRUE;
	END;
      EXCEPT
      | VMError.E(ec) =>
	IF ec # VMError.StaleFrame THEN
	  IO.Put("page fault: exception " & VMError.Message(ec) & ".\n");
	  Debugger.Enter();
	ELSE
	  IO.Put("page fault: access race, retrying..\n");
	  (* Wait for .1 sec and retry. The wait interval is totally
	     arbitrary. *)
	  Thread.Pause(100000);
	  done := TRUE;
	END;
      END;
    END; (* lock self.lock *)
    Spy.Exit(pfTime);
  END Handler;

BEGIN
  
END PageFault.

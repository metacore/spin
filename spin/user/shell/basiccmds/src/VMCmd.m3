(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 10-Jan-97  Yasushi Saito (yasushi) at the University of Washington
 *	Eliminated the intf file. CoreCommands is just a placeholder.
 *)
MODULE VMCmd EXPORTS CoreCommands;
IMPORT PhysAddr;
IMPORT PhysAddrPrivate;
IMPORT DefaultPager;
IMPORT ParseParams;
IMPORT IO, Fmt;
IMPORT VMDebug;
IMPORT VMStat;
IMPORT InfoFile, Wr, Error;
IMPORT Commands;

CONST Help =  " params | setparams REQ RECLAIM REPOSSES | stat | debug | undebug| swapon DEV";
  
PROCEDURE Run (<*UNUSED*>c: REFANY; pp : ParseParams.T) : BOOLEAN =
  VAR
    p : PhysAddr.Params;
    s : PhysAddr.Stat;
  BEGIN
    TRY
      pp.reset();
      pp.skipNext();
      IF pp.testNext("params") THEN
	PhysAddr.GetParams(p);
	IO.Put("request\treclaim\treposses\n");
	IO.Put(Fmt.Int(p.requestThreshold));
	IO.Put("\t" & Fmt.Int(p.reclaimThreshold));
	IO.Put("\t" & Fmt.Int(p.repossesThreshold) & "\n");
      ELSIF pp.testNext("setparams") THEN
	p.requestThreshold := pp.getNextInt();
	p.reclaimThreshold := pp.getNextInt();
	p.repossesThreshold := pp.getNextInt();
	PhysAddrPrivate.SetParams(p);
      ELSIF pp.testNext("stat") THEN
	PhysAddr.GetStat(s);
	IO.Put("tmp\tdead\tzombie\tfree\treclaim\tactive\npin");
	FOR i := FIRST(PhysAddr.State) TO LAST(PhysAddr.State) DO
	  IO.Put(Fmt.Int(s.sizes[i]) & "\t");
	END;
	IO.Put("\n");
	VMDebug.PrintStat();
	DefaultPager.Stat(NIL);
      ELSIF pp.testNext("swapon") THEN
	DefaultPager.SwapOn(pp.getNext());
      ELSE
	IO.Put("vm:" & Help & "\n");
      END;
    EXCEPT
    ELSE
      IO.Put("vm:" & Help & "\n");
    END;
    RETURN TRUE;
  END Run;
  

PROCEDURE VmStats (wr: Wr.T) =
  VAR
    p : PhysAddr.Params;
    s : PhysAddr.Stat;
  BEGIN
    PhysAddr.GetParams(p);
    PhysAddr.GetStat(s);
    IO.Put("active: " & Fmt.Int(s.sizes[PhysAddr.State.Active]) & ","
	       & Fmt.Int(p.requestThreshold) & "\n", wr);
    IO.Put("reclaim: "
	       & Fmt.Int(s.sizes[PhysAddr.State.Reclaimed]) & ","
	       & Fmt.Int(p.reclaimThreshold) & "\n", wr);
    IO.Put("free: " & Fmt.Int(s.sizes[PhysAddr.State.Free]) & ","
		& Fmt.Int(p.repossesThreshold) & "\n", wr);
    IO.Put("pinned: "
	       & Fmt.Int(s.sizes[PhysAddr.State.StronglyPinned]) & ",0\n", wr);
    IO.Put("trans: "
	       & Fmt.Int(s.sizes[PhysAddr.State.Transient]) & ",0\n", wr);
    IO.Put("fault: " & Fmt.Int(VMStat.fault) & ", 0\n", wr);
    IO.Put("cow: " & Fmt.Int(VMStat.cow) & ", 0\n", wr);
    IO.Put("zero: " & Fmt.Int(VMStat.zero) & ", 0\n", wr);
    IO.Put("react: " & Fmt.Int(VMStat.react) & ", 0\n", wr);
    IO.Put("pagein: " & Fmt.Int(VMStat.pin) & ", 0\n", wr);
    IO.Put("pageout: " & Fmt.Int(VMStat.pout) & ", 0\n", wr);
    VMStat.fault := 0;
    VMStat.cow := 0;
    VMStat.zero := 0;    
    VMStat.react := 0;
    VMStat.pin := 0;
    VMStat.pout := 0;
  END VmStats;

BEGIN
  EVAL Commands.Install(Run, "vm", Help);
  TRY
    InfoFile.Create("/proc/vmstats",VmStats);
  EXCEPT
  | Error.E(e) =>
    IO.PutError("gc procfs files:" & e.message() & "\n");
  END;
END VMCmd.

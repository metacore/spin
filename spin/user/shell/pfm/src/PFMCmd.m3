(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 24-Feb-97  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
MODULE PFMCmd;
IMPORT IO, Fmt;
IMPORT Commands;
IMPORT PFM;
IMPORT ParseParams;
IMPORT Word;
VAR
  openMode := PFM.PCNTOPENALL;
  open: BOOLEAN;
  
PROCEDURE OpenIfNecessary() =
  BEGIN
    IF NOT open THEN
      PFM.Open(openMode);
      open := TRUE;
    END;
  END OpenIfNecessary;

VAR
  range := PFM.AddrRange{0, -1};
  items: INTEGER := Word.Or(PFM.PFM_COUNTERS, PFM.PFM_PROFILING);
  
PROCEDURE Run (<*UNUSED*>c: REFANY; pp: ParseParams.T): BOOLEAN =
  BEGIN
    pp.reset();
    TRY
      pp.skipNext();
      IF pp.testNext("open") THEN
	openMode := pp.getNextInt();
	OpenIfNecessary();
      ELSIF pp.testNext("enable") THEN
	OpenIfNecessary();
	PFM.Enable();
	PFM.SetItems(items);
	PFM.SetKaddr(range);
      ELSIF pp.testNext("disable") THEN
	OpenIfNecessary();
	PFM.Disable();
      ELSIF pp.testNext("close") THEN
	PFM.Close();
      ELSIF pp.testNext("setitems") THEN
	items := 0;
	IF pp.testNext("counters") THEN
	  items := Word.Or(items, PFM.PFM_COUNTERS);
	END;
	IF pp.testNext("ipl") THEN
	  items := Word.Or(items, PFM.PFM_IPL);
	END;
	IF pp.testNext("profiling") THEN
	  items := Word.Or(items, PFM.PFM_PROFILING);
	END;
	PFM.SetItems(items);
      ELSIF pp.testNext("setmux") THEN
	VAR c: PFM.Iccsr;
	BEGIN
	  c.disable := 0;
	  c.pc0 := 0;
	  c.pc1 := 0;
	  c.mux0 := 0;
	  c.mux1 := 0;
	  WHILE pp.next <= LAST(pp.arg^) DO
	    IF pp.testNext("off0") THEN c.disable := 1; 
	    ELSIF pp.testNext("off1") THEN c.disable := 2; 
	    ELSIF pp.testNext("freq0") THEN c.pc0 := 1; 
	    ELSIF pp.testNext("freq1") THEN c.pc1 := 1; 
	    ELSIF pp.testNext("issues") THEN c.mux0 := PFM.PF_ISSUES;
	    ELSIF pp.testNext("pipedry") THEN c.mux0 := PFM.PF_PIPEDRY;
	    ELSIF pp.testNext("loadi") THEN c.mux0 := PFM.PF_LOADI;
	    ELSIF pp.testNext("pipefrozen") THEN c.mux0 := PFM.PF_PIPEFROZEN;
	    ELSIF pp.testNext("branchi") THEN c.mux0 := PFM.PF_BRANCHI;
	    ELSIF pp.testNext("cycles") THEN c.mux0 := PFM.PF_CYCLES;
	    ELSIF pp.testNext("palmode") THEN c.mux0 := PFM.PF_PALMODE;
	    ELSIF pp.testNext("nonissues") THEN c.mux0 := PFM.PF_NONISSUES;
	    ELSIF pp.testNext("dcache") THEN c.mux1 := PFM.PF_DCACHE;
	    ELSIF pp.testNext("icache") THEN c.mux1 := PFM.PF_ICACHE;
	    ELSIF pp.testNext("dual") THEN c.mux1 := PFM.PF_DUAL;
	    ELSIF pp.testNext("branchmiss") THEN c.mux1 := PFM.PF_BRANCHMISS;
	    ELSIF pp.testNext("fpinst") THEN c.mux1 := PFM.PF_FPINST;
	    ELSIF pp.testNext("intops") THEN c.mux1 := PFM.PF_INTOPS;
	    ELSIF pp.testNext("storei") THEN c.mux1 := PFM.PF_STOREI;
	    ELSE
	      IO.Put("unknown setmux option.\n");
	      Help();
	      RETURN TRUE;
	    END;
	  END;
	  OpenIfNecessary();
	  PFM.SetMux(c);
	END;
      ELSIF pp.testNext("clear") THEN
	PFM.ClearCount();
      ELSIF pp.testNext("getcount") THEN
	VAR c: PFM.Counter;
	BEGIN
	  PFM.GetCount(c);
	  IO.Put("cntr0 = 0x" & Fmt.Int(c.cntr0) & ".\n");
	  IO.Put("cntr1 = 0x" & Fmt.Int(c.cntr1) & ".\n");
	  IO.Put("cycle = 0x" & Fmt.Int(c.cycle) & ".\n");
	END;
(*      WITH pos = Text.FindChar(arg, '.') DO
	IF pos >= 0 THEN
	  unit := Text.Sub(arg, 0, i);
	  name := Text.Sub(arg, i+1, Text.Length(arg) - i - 1);
   END;*)
      ELSE
	Help();
      END;
    EXCEPT
    | ParseParams.Error => 
      Help();
      RETURN FALSE;
    END;
    RETURN TRUE;
  END Run;

CONST CommandHelp = " type \"pfm help\" for the help message";
  
PROCEDURE Help () =
  BEGIN
    IO.Put("pfm COMMAND...\n")
    IO.Put("COMMAND is one of:\n");
    IO.Put("enable : enable interrupts.\n");
    IO.Put("disable : disable interrupts. profiler works only when disabled\n");
    IO.Put("clear : clear the profile count.\n");
    IO.Put("setitems [ipl|profiling|counters]\n");
    IO.Put("getcount : get the current counter values.\n");
    IO.Put("setmux [off0|off1|freq0|freq1|issues|pipedry|loadi|pipefrozen\n");
    IO.Put("        |branchi|cycles|palmode|nonissues|dcache|icache|dual\n");
    IO.Put("        |branchmiss|fpinst|intops|storei]*\n");
    IO.Put(".\n");
  END Help;
  
BEGIN
  EVAL Commands.Install(Run, "pfm", CommandHelp);
END PFMCmd.

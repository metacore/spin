(* 
 * HISTORY
 * 28-Jun-95  Marc Fiuczynski (mef) at the University of Washington
 *	Initialization module called started by the dynamic linker after
 *	the M3 code has been initialized.
 *)

MODULE IcmpClient;
IMPORT IcmpClassification, IcmpGen, IcmpClientLink;
IMPORT IO, ParseParams, Commands, Net;

CONST ICMPGENCommandName = "icmpgen";
      ICMPGENCommandHelp = "-- -debug level";
PROCEDURE ICMPGEN(
    <*UNUSED*> closure : REFANY;
    pp      : ParseParams.T): BOOLEAN =
  BEGIN
    pp.reset();
    TRY
      pp.skipNext(); (* icmpgen *)
      IF pp.testNext("-debug") THEN
        IcmpGen.debug_level := Net.MapDebug(pp.getNextInt());
        RETURN TRUE;
      END;
    EXCEPT
      ParseParams.Error =>
    END;
    IO.Put("Usage: " & ICMPCLASSIFICATIONCommandName & 
      ":" & ICMPCLASSIFICATIONCommandHelp & "\n");
    RETURN TRUE;
  END ICMPGEN; 

CONST ICMPCLASSIFICATIONCommandName = "icmpclassification";
      ICMPCLASSIFICATIONCommandHelp = "-- -debug level";
PROCEDURE ICMPCLASSIFICATION(
    <*UNUSED*> closure : REFANY;
    pp      : ParseParams.T): BOOLEAN =
  BEGIN
    pp.reset();
    TRY
      pp.skipNext(); (* icmpclassification *)
      IF pp.testNext("-debug") THEN
        IcmpClassification.debug_level := Net.MapDebug(pp.getNextInt());
        RETURN TRUE;
      END;
    EXCEPT
      ParseParams.Error =>
    END;
    IO.Put("Usage: " & ICMPCLASSIFICATIONCommandName &
      ":" & ICMPCLASSIFICATIONCommandHelp & "\n");
    RETURN TRUE;
  END ICMPCLASSIFICATION;

PROCEDURE Init(verbose:BOOLEAN) = 
  BEGIN
    IcmpClassification.Init(verbose);
    IcmpClientLink.Init();

    EVAL Commands.Install(ICMPCLASSIFICATION,
                     ICMPCLASSIFICATIONCommandName,
                     ICMPCLASSIFICATIONCommandHelp);

    EVAL Commands.Install(ICMPGEN,
                     ICMPGENCommandName,
                     ICMPGENCommandHelp);

    IF verbose THEN IO.Put("IcmpClient module initialized.\n"); END;
  END Init;

CONST verbose = FALSE;
BEGIN
  Init(verbose);
END IcmpClient. 

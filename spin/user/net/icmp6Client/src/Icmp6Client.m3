(* 
 * HISTORY
 * 28-Jun-95  Marc Fiuczynski (mef) at the University of Washington
 *	Initialization module called started by the dynamic linker after
 *	the M3 code has been initialized.
 *)

MODULE Icmp6Client;
IMPORT Icmp6Classification;
IMPORT Icmp6Gen;
IMPORT Icmp6ClientLink;
IMPORT IO, Net;
IMPORT ParseParams, Commands;

VAR
  ICMP6CLASSIFICATIONBinding : REFANY;
CONST
  ICMP6CLASSCommandName = "icmp6classification";
  ICMP6CLASSCommandHelp = "-- debug level | uninstall [-v]";

PROCEDURE ICMP6CLASSIFICATION(
    closure: REFANY;
    pp: ParseParams.T): BOOLEAN =
  VAR
    verbose: BOOLEAN;
  BEGIN
    pp.reset();
    TRY
      pp.skipNext(); (* icmp6classification *)
      IF pp.testNext("debug") THEN
        Icmp6Classification.debug_level := Net.MapDebug(pp.getNextInt());
      ELSIF pp.testNext("uninstall") THEN
        verbose := pp.testNext("-v");
        Icmp6Classification.Uninit(verbose);
        ICMP6CLASSIFICATIONBinding := NIL;
      ELSE
        Commands.ParseError(closure);
      END;
    EXCEPT
    | ParseParams.Error => 
      Commands.ParseError(closure);
    END;
   RETURN TRUE;
 END ICMP6CLASSIFICATION; 

VAR
  ICMP6GENBinding : REFANY;
CONST
  ICMP6GENCommandName = "icmp6gen";
  ICMP6GENCommandHelp = "-- debug level | uninstall [-v]";

PROCEDURE ICMP6GEN(
    closure: REFANY;
    pp: ParseParams.T): BOOLEAN =
  VAR
    verbose: BOOLEAN;
  BEGIN
    pp.reset();
    TRY
      pp.skipNext(); (* etherdefault *)
      IF pp.testNext("-debug") THEN
        Icmp6Gen.debug_level := Net.MapDebug(pp.getNextInt());
      ELSIF pp.testNext("uninstall") THEN
        verbose := pp.testNext("-v");
        (* Icmp6Gen.Uninit(verbose); *)
        ICMP6GENBinding := NIL;
      ELSE
        Commands.ParseError(closure);
      END;
    EXCEPT
    | ParseParams.Error =>
      Commands.ParseError(closure);
    END;
   RETURN TRUE;
 END ICMP6GEN; 

PROCEDURE Init(verbose:BOOLEAN) = 
  BEGIN
    Icmp6Classification.Init(verbose);
    Icmp6ClientLink.Init();
    ICMP6CLASSIFICATIONBinding := 
        Commands.Install(ICMP6CLASSIFICATION,
                         ICMP6CLASSCommandName,
                         ICMP6CLASSCommandHelp);
    ICMP6GENBinding := 
        Commands.Install(ICMP6GEN,
                         ICMP6GENCommandName,
                         ICMP6GENCommandHelp);

    IF verbose THEN IO.Put("Icmp6Client module initialized.\n"); END;
  END Init;

CONST verbose = FALSE;
BEGIN
  Init(verbose);
END Icmp6Client. 

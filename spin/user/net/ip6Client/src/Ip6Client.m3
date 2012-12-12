(* 
 * HISTORY
 *)

MODULE Ip6Client;
IMPORT Ip6ClientLink; <* NOWARN *>
IMPORT Ip6Classification; <* NOWARN *>
IMPORT Ip6Gen; <* NOWARN *>

IMPORT Commands, ParseParams, IO, Net;

VAR IP6CLASSIFICATIONBinding : REFANY;
CONST
  IP6CLASSCommandName = "ip6classification";
  IP6CLASSCommandHelp = "-- -debug level";
PROCEDURE IP6CLASSIFICATION(
    closure: REFANY; 
    pp: ParseParams.T): BOOLEAN =
  VAR
    verbose: BOOLEAN;  
  BEGIN
    pp.reset();
    TRY
      pp.skipNext(); (* ip6classifcation *)
      IF pp.testNext("debug") THEN
        Ip6Classification.debug_level := Net.MapDebug(pp.getNextInt());
      ELSIF pp.testNext("uninstall") THEN
        verbose := pp.testNext("-v"); 
        Ip6Classification.Uninit(verbose);
        Commands.Uninstall(IP6CLASSIFICATIONBinding);
        IP6CLASSIFICATIONBinding := NIL;
      ELSE
        Commands.ParseError(closure);
      END;
    EXCEPT
    | ParseParams.Error => 
      Commands.ParseError(closure);
    END;
    RETURN TRUE;
  END IP6CLASSIFICATION;

(* shell command support *)
VAR
  IP6GENBinding : REFANY;
CONST
  IP6GENCommandName = "ip6gen";
  IP6GENCommandHelp = "-- debug level | uninstall [-v]";

PROCEDURE IP6GEN(
    closure: REFANY;
    pp: ParseParams.T): BOOLEAN =
  VAR
    verbose: BOOLEAN;
  BEGIN
    pp.reset();
    TRY
      pp.skipNext(); (* ip6gen *)
      IF pp.testNext("debug") THEN
        Ip6Gen.debug_level := Net.MapDebug(pp.getNextInt());
      ELSIF pp.testNext("uninstall") THEN
        verbose := pp.testNext("-v");
        Ip6Classification.Uninit(verbose);
        Commands.Uninstall(IP6GENBinding);
        IP6GENBinding := NIL;
      ELSE
        Commands.ParseError(closure);
      END;
    EXCEPT
    | ParseParams.Error =>
      Commands.ParseError(closure);
    END;
   RETURN TRUE;
 END IP6GEN; 

PROCEDURE Init(verbose:BOOLEAN) = 
  BEGIN
    Ip6Gen.Init(verbose);
    Ip6Classification.Init(verbose);
    Ip6ClientLink.Init();
    IP6CLASSIFICATIONBinding := 
        Commands.Install(IP6CLASSIFICATION,
                         IP6CLASSCommandName,
                         IP6CLASSCommandHelp);
    IP6GENBinding := 
        Commands.Install(IP6GEN,
                         IP6GENCommandName,
                         IP6GENCommandHelp);
    IF verbose THEN IO.Put("Ip6Client module initialized.\n"); END;
  END Init;

CONST verbose = TRUE;
BEGIN
  Init(verbose);
END Ip6Client.

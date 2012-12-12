INTERFACE PacketCounter;
IMPORT ParseParams;

(* shell command support *)
CONST CommandName = "netstats";
      CommandHelp = "-- init | dump | reset";
PROCEDURE Run(pp: ParseParams.T): BOOLEAN;

CONST Brand = "PacketCounter";
END PacketCounter.

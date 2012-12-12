INTERFACE Command;
IMPORT ParseParams;

(* shell command support *)
CONST CommandName = "security";
      CommandHelp = "\n" &
        "-- whoami\n" &
        "-- performance\n";

PROCEDURE Run(pp: ParseParams.T): BOOLEAN;

CONST Brand = "Command";
END Command.

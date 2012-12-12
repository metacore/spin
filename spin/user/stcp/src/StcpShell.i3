INTERFACE StcpShell;
IMPORT ParseParams;


CONST CommandName = "stcp";
      CommandHelp = "[zap|fetch <filename>]";
PROCEDURE Run(pp: ParseParams.T): BOOLEAN;

END StcpShell.

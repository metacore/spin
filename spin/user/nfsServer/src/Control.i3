INTERFACE Control;
IMPORT ParseParams;
(* shell command support *)
CONST CommandName = "nfsd";
      CommandHelp = "-zap";
PROCEDURE Run(pp: ParseParams.T): BOOLEAN;
END Control.

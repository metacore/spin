INTERFACE Heap;
IMPORT ParseParams;

CONST CommandName = "heap";
CONST CommandHelp = "dump some info, zap to get rid of it";

PROCEDURE Run(pp: ParseParams.T): BOOLEAN;

END Heap.

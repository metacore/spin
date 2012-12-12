(*
 * HISTORY
 * 03-Dec-97  Przemek Pardyak (pardy) at the University of Washington
 *	Created.
 *)

(*
 * They can be of the form job-type [number] [size] where job-type is
 * one of the names static, dynamic, bursty or neglect. number and
 * size are optional integer parameters.  We can have any number of
 * arguments on the command line.  One thread will be forked for each
 * of the job-types, and each thread will operate on its own tree.
 *)

INTERFACE GCBench;

IMPORT ParseParams;


CONST CommandName = "gcbench";

CONST CommandHelp = "zap | verbose | <static|dynamic|bursty|neglect> [number] [size] [iter] ...";

PROCEDURE Run(pp: ParseParams.T): BOOLEAN; 

END GCBench.

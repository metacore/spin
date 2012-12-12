(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 20-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	Whisted.  Parseparams
 *
 *)
INTERFACE Benchmark;

IMPORT ParseParams;

CONST CommandName = "benchmark";
CONST CommandHelp = " <benchmarkname> run a cool benchmark";

PROCEDURE Run (pp: ParseParams.T): BOOLEAN;

END Benchmark.





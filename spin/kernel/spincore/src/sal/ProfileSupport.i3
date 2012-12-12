(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 29-Jul-96  Charles Garrett (garrett) at the University of Washington
 *	Added SetNumArcs.
 *
 * 29-May-96  Charles Garrett (garrett) at the University of Washington
 *	Added the Flush command and the option to keep previous profile
 *	 data when calling On.
 *
 * 05-May-96  Charles Garrett (garrett) at the University of Washington
 *	Created. This is the procedural interface to profiling. In order
 *	 to gather a profile, you should call StartGprof, then execute
 *	 your code and then call StopGprof. Be sure to read out the
 *	 information before you call StartGprof again, because it will 
 *	 overwrite the earlier profiles.
 *
 *)
INTERFACE ProfileSupport;
IMPORT Word;

(* If you touch this interface, you must make sure that m3gdbttd
   has the correct offsets for these seven variables. *)
VAR
  numSamples : INTEGER;
  flatProfile : Word.T;
  numArcs : INTEGER;
  graphProfile : Word.T;
  numStubs : INTEGER;
  stubInfo : Word.T;
  clockRate : INTEGER;

CONST CompiledIn = FALSE;

PROCEDURE Flush();
PROCEDURE SetNumArcs(n: CARDINAL);
PROCEDURE SetNumCallers(n: CARDINAL);
PROCEDURE On(keep: BOOLEAN := FALSE): BOOLEAN;
PROCEDURE Off(): BOOLEAN;
PROCEDURE Enabled(): BOOLEAN;
PROCEDURE PrintSamples();
PROCEDURE StoreSamples();

END ProfileSupport.



(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description.
 *
 * HISTORY
 * 19-Dec-96  Brian Dewey (dewey) at the University of Washington
 *      Created.
 *)

(* This shell command tests the LRU-WT cache. *)
(* See CacheTest.m3 for full documentation *)
INTERFACE CacheTest;

IMPORT ParseParams;

CONST CommandName = "CacheTest";
CONST CommandHelp = "-init|-write|-verify";

PROCEDURE Run(pp: ParseParams.T): BOOLEAN;

END CacheTest.

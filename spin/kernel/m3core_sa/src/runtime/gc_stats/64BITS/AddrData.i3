
(*
 *
 * Copyright 1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *)


(*
 * HISTORY
 * 24-Feb-97  Wilson Hsieh (whsieh) at the University of Washington
 *	created
 *
 *)

INTERFACE AddrData;

IMPORT RT0;

CONST Brand = "Address Data";

TYPE T = RECORD
  tenure, implicitMove, move, collection : [0..16_ffffffff] := 0;
  tc: RT0.Typecode;
END;

END AddrData.

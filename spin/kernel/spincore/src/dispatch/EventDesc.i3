(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 06-Aug-95  Stefan Savage (savage) at the University of Washington
 *	Added Brand
 *
 * 02-Apr-95  Przemyslaw Pardyak (pardy) at the University of Washington
 *      Created. Event dispatcher.
 *
 *)
INTERFACE EventDesc;
IMPORT DispatcherRep;

TYPE T = DispatcherRep.EventDescT;

CONST
  Brand = "EventDesc-1.0";

END EventDesc.


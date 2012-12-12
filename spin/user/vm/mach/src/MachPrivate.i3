(*
 * Copyright 1995,1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * PURPOSE
 *	Private access to Mach 3.0 emulation routines.
 *	Currently used only for Init function.
 *
 * HISTORY
 * 12-Dec-95  Stefan Savage (savage) at the University of Washington
 *	Created
 *
 *)


INTERFACE MachPrivate;

IMPORT Mach;

PROCEDURE Init(verbose: BOOLEAN);

END MachPrivate.

(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 02-May-97  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
INTERFACE VMDebug;
IMPORT PhysAddr;
PROCEDURE CalculateChecksum(frame: PhysAddr.T): INTEGER;
END VMDebug.

(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 28-Oct-95  Brian Bershad (bershad) at the University of Washington
 *	Moved externs to extern interface.
 *
 * 02-Nov-94  Stefan Savage (savage) at the University of Washington
 *	Created
 * 
 *)

INTERFACE PhysFreeMemPrivate;
IMPORT PhysAddr;

PROCEDURE Init(verbose: BOOLEAN; tracedHeapStart: PhysAddr.Address;
	       tracedHeapSize: PhysAddr.Size);


  
END PhysFreeMemPrivate.

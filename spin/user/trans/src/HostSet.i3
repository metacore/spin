(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 01-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *	
 *	
 *)
INTERFACE HostSet;
IMPORT TransGroup;

  
TYPE
  T = ARRAY [0 .. TransGroup.MaxGroup-1] OF BITS 1 FOR BOOLEAN;

  
END HostSet.

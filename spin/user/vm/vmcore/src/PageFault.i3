(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 29-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Separated from AddressSpace.i3
 *)
INTERFACE PageFault;
IMPORT Strand;
IMPORT CPU;
IMPORT Translation;
IMPORT Word;

PROCEDURE Handler(strand: Strand.T; 
		  VAR ss: CPU.SavedState;
		  map : Translation.T;		    
		  addr: CPU.VirtAddress;
		  type: Word.T;
		  VAR done : BOOLEAN);

END PageFault.

(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 29-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Separated from Translation.T
 * html
 *)

INTERFACE Protection;

TYPE T = RECORD
    read: BITS 1 FOR BOOLEAN;
    write: BITS 1 FOR BOOLEAN;
    execute: BITS 1 FOR BOOLEAN;
    others: BITS 29 FOR [0 .. 16_FFFFFFF];
      (* "others" is not used by the spincore; extensions are free to use
         it for their own purpose. One use of "others" is as
         the copy-on-write bit. See also MemoryObject.i3 in vmcore. *)
      
      (* XXX others should be 13 bit long really. *)

  END;
  (* XXX We make a possibly wrong assumption here that the representation of
   "T" is same as the one used in pmap ("VM_PROT_READ", etc). *)

      
CONST
  ReadOnly = T{TRUE,FALSE,FALSE,0};
  All = T{TRUE,TRUE,TRUE,0};

END Protection.


(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 28-Aug-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

(* "TransPager.T" defines a [PagerObject] that is used by 
   the SPIN VM system. *)

INTERFACE TransPager;
IMPORT PagerObject;
IMPORT Storage;
IMPORT TransGroup;

TYPE T <: PagerObject.T;
  
PROCEDURE Create(st: Storage.T): PagerObject.T;
(* Create a new pager. *)
  
PROCEDURE InvalidateMappingsForSpace(t: T; group: TransGroup.T);
(* Unmap all the frames that belong to "t" and are mapped on "space".
   This is called when a transaction terminates.
*)
   
  
END TransPager.

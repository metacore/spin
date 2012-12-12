(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 03-Apr-97  Yasushi Saito (yasushi) at the University of Washington
 *	Created
 *)
INTERFACE ShadowPager;
IMPORT PagerObject;
IMPORT Storage;

TYPE T <: PagerObject.T;
  
PROCEDURE Create(st: Storage.T): PagerObject.T;
(* Create a new pager. *)
  
END ShadowPager.

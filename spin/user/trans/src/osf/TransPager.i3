(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 06-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
INTERFACE TransPager;
IMPORT Storage;
IMPORT TransGroup;
IMPORT PagerObject;
TYPE T <: PagerObject.T;
  
PROCEDURE Create(st: Storage.T): T;
PROCEDURE GetStorage(t: T): Storage.T;

PROCEDURE InvalidateMappingsForSpace(t: T; group: TransGroup.T);
    
END TransPager.

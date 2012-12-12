(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 11-Jun-96  Eric Christoffersen (ericc) at the University of Washington
 *	Object for storing directory info.  Essentially an expandable array that 
 *	supports alphabetically sorting.
 *)
INTERFACE DirInfoArray;
IMPORT DirInfo;


CONST Brand = "INodeInfoArray";
CONST DEFAULTSIZE = 10;


EXCEPTION DirInfoArrayError;

TYPE

  Public = OBJECT
  METHODS
    addElement(READONLY newDirInfo:DirInfo.T):BOOLEAN RAISES {DirInfoArrayError};
    getElement(index:CARDINAL):DirInfo.T RAISES {DirInfoArrayError};
    sort():BOOLEAN;
    size():CARDINAL;
    getArray():REF ARRAY OF DirInfo.T; (* 1 is first element *)
    isMember(fileName:TEXT;VAR iNum:BITS 16 FOR [0..65535]):BOOLEAN;
    initialize(initialSize:CARDINAL):T;
  END;
  T<: Public;

  PROCEDURE NewDirInfoArray(initialSize:CARDINAL):T;

END DirInfoArray.











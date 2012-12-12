(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 30-May-96  Eric Christoffersen (ericc) at the University of Washington
 *	Created.  Array used to communicate updated blocks to inodes.
 *
 *)
INTERFACE INodeInfoArray;
IMPORT INodeInfo;


CONST Brand = "INodeInfoArray";
CONST DEFAULTSIZE = 10;


EXCEPTION INodeInfoArrayError;

TYPE

  Public = OBJECT
  METHODS
    addElement(READONLY newINodeInfo:INodeInfo.T):BOOLEAN;
    getElement(index:CARDINAL):INodeInfo.T RAISES {INodeInfoArrayError};
    sort():BOOLEAN;
    size():CARDINAL;
    getArray():REF ARRAY OF INodeInfo.T; (* 1 is first element *)
    initialize(initialSize:CARDINAL):T;
  END;
  T<: Public;

  PROCEDURE NewINodeInfoArray(initialSize:CARDINAL):T;

END INodeInfoArray.








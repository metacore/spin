(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 30-May-96  Eric Christoffersen (ericc) at the University of Washington
 *	Created.  Object used to populate INodeInfoArray, useful for
 *      compare function.
 *)
INTERFACE INodeInfo;

FROM Segment IMPORT DiskAddress;
FROM LFSTypes IMPORT ShortCard;

CONST BRAND = "INodeInfo";

TYPE T = OBJECT

  (* inum of file that holds the block at below diskaddress *)
  iNode      : ShortCard;

  (* block number in file or inode where block is refered to *)
  block      : ShortCard;

  (* disk address of block in question *)   
  diskAddress: DiskAddress;

END;

PROCEDURE Compare(READONLY a, b: T) : [-1 .. 1];

END INodeInfo.

(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 11-Jun-96  Eric Christoffersen (ericc) at the University of Washington
 *	Type for populating dirObjects, maps Text to INum, supports alphabetic sorting of text.
 *)
INTERFACE DirInfo;

FROM Segment IMPORT DiskAddress;
FROM LFSTypes IMPORT ShortCard;
FROM LFSTypes IMPORT MAXNAMELEN;
CONST BRAND = "DirInfo";

TYPE T = OBJECT

  (* inum of file that text name refers to *)
  iNum      : ShortCard;

  (* name of file *)
  name      : ARRAY [0..MAXNAMELEN -1] OF CHAR;

END;

PROCEDURE Compare(READONLY a, b: T) : [-1 .. 1];

END DirInfo.



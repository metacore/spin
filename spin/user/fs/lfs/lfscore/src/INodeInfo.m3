(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 30-May-96  Eric Christoffersen (ericc) at the University of Washington
 *	Created.  Object used for populating INodeInfoArray.  Useful because 
 *	of compare().
 *)
MODULE INodeInfo;

PROCEDURE Compare(READONLY a, b: T) : [-1 .. 1] =
  BEGIN


    (*****************************************************)
    (* If the objects are NIL then we want them to migrate
       to the end of the array, ie: they are bigger*)

    IF a = NIL THEN 
      IF b = NIL THEN 
        RETURN 0;
      ELSE
        RETURN 1;END;
    END;

    IF b = NIL THEN
      RETURN -1;
    END;

    (* end of nil comparison stuff *)
    (*****************************************************)



    (*****************************************************)
    IF (a.iNode < b.iNode) THEN        (* Inode number is most important *)
      RETURN 1;
    ELSIF (a.iNode > b.iNode) THEN 
      RETURN -1;
    ELSIF (a.block < b.block) THEN     (* Then block number *)
      RETURN 1;
    ELSIF (a.block > b.block) THEN
      RETURN -1;
    ELSIF (a.diskAddress.segment < b.diskAddress.segment) THEN (* then segment number of block *)
      RETURN 1;
    ELSIF (a.diskAddress.segment > b.diskAddress.segment) THEN
      RETURN -1;
    ELSIF (a.diskAddress.block < b.diskAddress.block) THEN     (* Then block number of block *)
      RETURN 1;
    ELSIF (a.diskAddress.block > b.diskAddress.block) THEN
      RETURN -1;
    ELSE
      RETURN 0;   (* If this occurs then we've got two entries that are the same...*)
    END;
  END Compare;

BEGIN
END INodeInfo.




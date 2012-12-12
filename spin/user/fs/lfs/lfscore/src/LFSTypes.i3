(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 30-May-96  Eric Christoffersen (ericc) at the University of Washington
 *	Created.  Holds pissy types and constants for use across LFS.
 *
 *)

INTERFACE LFSTypes;
IMPORT Word, Error;
FROM Segment IMPORT DiskAddress;

TYPE

  ID = Word.T;

  ShortCard = BITS 16 FOR [0..65535];   (* this must be the same as the ShrtCard type in segment/src/Segment.i3 *)


  RCharArray = REF ARRAY OF CHAR;


  (* This is used for communication between
     iNodes and the iMap, needs to be here... *)
  INumInfo = RECORD
    iNum,version:ShortCard;
  END;

  LFSErrorT <: LFSErrorTPublic;
  LFSErrorTPublic = Error.T OBJECT
	messageE : TEXT;
  END;

  INodeBlockInfo = RECORD
    diskAddress:DiskAddress;
    block      :ShortCard;
  END;


    
  CONST
    SHORTCARDMAX       : ShortCard = LAST(ShortCard)-1;
    MAXINODES          : ShortCard = LAST(ShortCard)-1;
    MAXNAMELEN         : ShortCard = 256;
    DIR                : ID = 0;  (* used to indicate file type in iNode *)
    FILE               : ID = 1;  (* used to indicate file type in iNode *)
    OEXIST  = 0;                  (* is this useless now? *)
    OCREATE = 1;
    OTRUNC  = 2;
    ORDONLY = 3;
    OWRONLY = 4;
    
    (* these are used by INode.Update and by INode.VerifyMeta, to store location of inode meta-data blocks *)
    INODEOFFSET = 0;
    INDIRECTOFFSET = 1;
    FIRSTDIRECTOFFSET = 2;

    (* values for segment descriptor flag *)
    DATA = 0;  (* if entry holds data *)
    META = 1;  (* if entry holds meta-data *)
    
    
    (* if this value is seen in a disk address then
       you know something is awry *)
    ILLEGALDISKVALUE   : ShortCard = LAST(ShortCard);
    
  TYPE
    DirectoryData = RECORD
      name  : ARRAY [0..MAXNAMELEN -1] OF CHAR;
      iNum  : ShortCard;
    END;
    
    
  END LFSTypes.



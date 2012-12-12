(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 01-Nov-97  Tsutomu Owa (owa) at the University of Washington
 *	Cleaned up.
 *
 * 30-May-96  Eric Christoffersen (ericc) at the University of Washington
 *	Created.
 *
 *)


INTERFACE IMap;

IMPORT Segment;
IMPORT SegBuffer;
FROM Segment  IMPORT DiskAddress;
FROM LFSTypes IMPORT RCharArray, ShortCard;

CONST Brand = "IMap";
      IMAPENTRYSIZE = BYTESIZE(IMapEntry);
	(* size of each imap entry placed in the superblock. *)

EXCEPTION OutOfINums;
EXCEPTION InvalidINum;

TYPE

  IMapEntry = RECORD
    diskAddress: DiskAddress;
    version: ShortCard;
  END;

  (* This is what the imap an array of *)
  IMapElement = RECORD
    entry: IMapEntry;
    iNode: ROOT;	(* the imap stores inodes as type ROOT,
                         *  this way it doesn't need to know what
                         * an inode is
			 *)
  END;

  T <: Public;

  Public = BRANDED OBJECT
  METHODS

    (* Used for creating or gaining access to inodes *)
    getNewINode():ROOT RAISES {OutOfINums};
    getINode(iNum:ShortCard):ROOT RAISES {InvalidINum};

    (* below function probably only needed internally *)
    (* if you need it then uncomment *)
    (* getNewINum():INumInfo RAISES {OutOfINums};*)

    (* Used to set the location of the inodes *)
    getLocation(iNum:ShortCard):DiskAddress; 
    setLocation(iNum:ShortCard;newLocation:DiskAddress):BOOLEAN;

    (* renders an iNum free to be reused *)
    rmINode(iNum:ShortCard);

    flush():RCharArray;  (* returns alloced and populated ref array of chars *)
    initialize(segInfo:Segment.T;
               buffer:SegBuffer.Buffer;
               numberOfINums:ShortCard;
               READONLY charArray: REF ARRAY OF CHAR):T;
  END;

PROCEDURE NewIMap(segInfo:Segment.T; 
                  buffer:SegBuffer.Buffer; 
                  READONLY charArray: REF ARRAY OF CHAR):T;


END IMap.




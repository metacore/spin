
(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 30-May-96  Scott VanWoudenberg (scottv) at the University of Washington
 *	Created.  Private interface for iMap to use, it doesn't know what
 *      inodes are.
 *
*)
(*
  header/copyright stuff goes here
*)

INTERFACE INodePrivate;

IMPORT Segment, SegBuffer, IMap;
FROM Segment IMPORT DiskAddress;
FROM LFSTypes IMPORT INumInfo;

PROCEDURE ConstructINode(segBuffer : SegBuffer.Buffer; imap : IMap.T;
                         from : DiskAddress) : ROOT;

PROCEDURE NewINode(segBuffer : SegBuffer.Buffer; imap : IMap.T;
                   info : INumInfo) : ROOT;

END INodePrivate.

(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 01-Nov-97  Tsutomu Owa (owa) at the University of Washington
 *	Made cleaner thread fork'ed.
 *
 * 30-May-96  Eric Christoffersen (ericc) at the University of Washington
 *	Whisted.
 *
 *)
INTERFACE Cleaner;

IMPORT Segment;
IMPORT SegBuffer;
IMPORT IMap;
IMPORT LFSLock;

CONST Brand = "Cleaner";

TYPE

  (* define the cleaner object *)
  T <: Public;
  
  Public = BRANDED OBJECT
  METHODS
    (* cleans the next segment! *)
    cleanNextSegment():BOOLEAN;
    
    (* init cleaner and fork it.  By default, cleaner is stopped. *)
    initialize(segInfo: Segment.T;
	       buffer: SegBuffer.Buffer;
	       iMap: IMap.T;
	       lock: LFSLock.T):T;
    
    (* start cleaner *)
    start();

    (* stop cleaner *)
    stop()
    
  END;  (* End of cleaner object methods *)
  
END Cleaner.

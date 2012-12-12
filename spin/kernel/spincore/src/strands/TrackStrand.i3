(*
 * Copyright 1994, 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
  	TrackStrand.i3
  
   Keep track of strand usage per extension.
 *)

INTERFACE TrackStrand;
IMPORT Strand;
IMPORT Track, TrackExtension, TrackMalloc;

TYPE T <: Track.T;

PROCEDURE Init();
PROCEDURE Create(s: Strand.T);
PROCEDURE Delete(track: T);
PROCEDURE SetName(tracker:T; label: TEXT);

(* operations on the current strand *)
PROCEDURE Extension() : TrackExtension.T;
PROCEDURE SetExtension(extension: TrackExtension.T);
PROCEDURE Table() : Track.ResourceTable;
PROCEDURE Malloc(): TrackMalloc.T; (* shortcuts lookup of "malloc"*)

(* ContextSwitch is the Strand.Run handler.  It is exported so Thread
	can install it ahead of its RunHandler.  The Thread RunHandler
	does not return so we have to be installed ahead of its handler *)
PROCEDURE ContextSwitch(strand:Strand.T) ;

VAR
  untracked : T;

END TrackStrand.

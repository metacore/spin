(*
 * Copyright 1994, 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
  	TrackMalloc.i3
  
   Keep track of malloc usage per extension.
 *)

INTERFACE TrackMalloc;
IMPORT Track;

TYPE T <: Track.T;

PROCEDURE Init();

(* track general purpose malloc space *)
PROCEDURE Malloc(bytes:INTEGER);
PROCEDURE Free(bytes:INTEGER);

END TrackMalloc.

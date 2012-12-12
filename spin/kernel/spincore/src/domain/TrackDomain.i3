(*
 * Copyright 1994, 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
  	TrackDomain.i3
  
   Keep track of domain usage per extension.
 *)

INTERFACE TrackDomain;
IMPORT Domain, Track, TrackExtension;

TYPE T <: Track.T;

PROCEDURE Create(d: Domain.T);

(* track domain malloc usage during construction *)
PROCEDURE SetNewborn(d: Domain.T);
PROCEDURE UnsetNewborn(d: Domain.T);
PROCEDURE Malloc(bytes:INTEGER);
PROCEDURE Free(bytes:INTEGER);

(* reassign to new extension after domain is constructed *)
PROCEDURE SetExtension(ext:TrackExtension.T; d: Domain.T);

END TrackDomain.

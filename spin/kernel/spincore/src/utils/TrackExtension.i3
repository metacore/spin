(*
 * Copyright 1994, 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
  	TrackExtension.i3
  
   Keep track of resources of an extension.
 *)

INTERFACE TrackExtension;
IMPORT Domain, Track;

TYPE T <: Track.T;

PROCEDURE Init();

PROCEDURE Create(domain: Domain.T) : T;
PROCEDURE Table(extension: T) : Track.ResourceTable;

END TrackExtension.

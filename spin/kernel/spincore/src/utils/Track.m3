(*
 * Copyright 1994, 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
  	Track.m3
  
   Keep track of resource usage per extension.
 *)

MODULE Track;
IMPORT Wr, NameServer, NSName;
IMPORT TrackExtension, TrackStrand, TrackMalloc;
IMPORT IO;

REVEAL
  T = Public BRANDED OBJECT
  OVERRIDES
    print:=Print;
  END;

REVEAL
  ResourceTable = ResourceTablePublic BRANDED OBJECT
  OVERRIDES
    mkdir := MakeDir;
  END;

PROCEDURE Print(<*UNUSED*>self:T; wr:Wr.T) =
  BEGIN
    Wr.PutText(wr,"Track base class.\n");
  END Print;

VAR
  globalTrack: ResourceTable ;
  nsRoot: NameServer.T;

PROCEDURE MakeDir(self: ResourceTable; dirname: TEXT): ResourceTable =
  VAR
    subTable := NEW(ResourceTable).init();
    name: NameServer.Name;
  BEGIN
    name := NSName.FromText(dirname);
    TRY
      self.attach(name, subTable);
    EXCEPT
    | NameServer.Error =>
      IO.Put("Track.MakeDir: " &dirname &" already exists\n");
    END;
    RETURN subTable;
  END MakeDir;

PROCEDURE GlobalTrack(): ResourceTable =
  BEGIN
    RETURN globalTrack;
  END GlobalTrack;

PROCEDURE Init() =
  BEGIN
    nsRoot := NameServer.Lookup(NIL, "/../svc");
    globalTrack := NEW(ResourceTable).init();
    NameServer.Attach(nsRoot, "track", globalTrack);

    TrackExtension.Init();
    TrackStrand.Init();
    TrackMalloc.Init();
  END Init;

BEGIN
END Track.

(*
 * HISTORY
 * 23-Jul-97  Yasushi Saito (yasushi) at the University of Washington
 *	Changed representation of Nameserver.Name
 *)
(*
 * Copyright 1994, 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
  	Track.i3
  
   Keep track of a resource, and NameServer tables of such tracker.
 *)

INTERFACE Track;
IMPORT Wr;
IMPORT NameServer;

TYPE
  T <: Public;
  Public = OBJECT METHODS
    print(wr:Wr.T);
  END;
  
  ResourceTable <: ResourceTablePublic;
  ResourceTablePublic = NameServer.Default OBJECT METHODS
    mkdir(dirname: TEXT): ResourceTable;
    (* XXX THIS IS SAME AS create(). DONT USE THIS ANY MORE!! - yaz *)
  END;
  

PROCEDURE Init();
PROCEDURE GlobalTrack(): ResourceTable;

END Track.

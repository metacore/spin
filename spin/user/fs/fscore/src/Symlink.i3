(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 22-Jul-97  Yasushi Saito (yasushi) at the University of Washington
 *	Changed NameServer.Name representation.
 * 07-Oct-96  Yasushi Saito (yasushi) at the University of Washington
 *  Created
 *)
INTERFACE Symlink;
IMPORT NameServer;
IMPORT FileStat;

TYPE T <: TPublic;
  TPublic = NameServer.Alias OBJECT
  METHODS
    init(dir: NameServer.T; READONLY name: NameServer.Name): T;
    stat(VAR stat: FileStat.T);
    getName((*OUT*)VAR name: NameServer.Name);
    (* get the "linkee" file name. *)
  END;
  
END Symlink.

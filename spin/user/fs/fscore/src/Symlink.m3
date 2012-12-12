(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 22-Jul-97  Yasushi Saito (yasushi) at the University of Washington
 *	Changed NameServer.Name representation.
 * 07-Oct-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created
 *)
MODULE Symlink;
IMPORT NameServer;
IMPORT NSName;
IMPORT FileSystem;

REVEAL T = TPublic BRANDED OBJECT
  dir: NameServer.T;
  name: NameServer.Name;
OVERRIDES
  init := Init;
  getName := GetName;
  getObject := GetObject;
END;

PROCEDURE Init (self: T; dir: NameServer.T; READONLY name: NameServer.Name): T =
  BEGIN
    self.dir := dir;
    self.name := NSName.DeepCopy(name);
    RETURN self;
  END Init;

PROCEDURE GetName (self: T; VAR name: NameServer.Name) =
  BEGIN
    name := self.name;
  END GetName;
  
(* This getobject is very bogus and unsafe. Needs renovation. *)
PROCEDURE GetObject (self: T): REFANY =
  BEGIN
    RETURN FileSystem.LookupName(self.dir, self.name);
  END GetObject;
  

BEGIN
END Symlink.

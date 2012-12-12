(* 
 * HISTORY
 * 07-Oct-96  Yasushi Saito (yasushi) at the University of Washington
 *	Removed iterate, added getentries.
 * 09-Jul-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *
 *)

MODULE MountDirectory;
IMPORT MountDirectoryRep;
IMPORT NameServer;
IMPORT Text, IO; <*NOWARN*>
IMPORT Word;
IMPORT Error;
IMPORT File;

REVEAL T = MountDirectoryRep.T BRANDED OBJECT
  OVERRIDES
    lookup  := Lookup;
    attach  := Attach;
    detach  := Detach;
    getEntries := GetEntries;
    create  := Create;
    mkfile := Mkfile;
    (* XXX need to override other methods, too *)
  END;

PROCEDURE Lookup(
    self                  : T; 
    VAR name                  : NameServer.Name;
    getalias              : BOOLEAN): REFANY 
  RAISES {NameServer.Error} =
  BEGIN
    RETURN self.filesystemRoot.lookup(name, getalias);
  END Lookup; 

PROCEDURE Attach(
    self : T; 
    READONLY name : NameServer.Name; 
    obj  : REFANY) 
  RAISES {NameServer.Error} = 
  BEGIN
    self.filesystemRoot.attach(name, obj);
  END Attach;

PROCEDURE Detach(
    self            : T; 
    READONLY name            : NameServer.Name) RAISES {NameServer.Error} = 
  BEGIN
    self.filesystemRoot.detach(name);
  END Detach;

PROCEDURE GetEntries (self: T; offset: Word.T;
		      VAR ent: ARRAY OF NameServer.Entry): CARDINAL =
  BEGIN
    RETURN self.filesystemRoot.getEntries(offset, ent);
  END GetEntries;

PROCEDURE Create(
    self : T; 
    name : NameServer.Name): NameServer.T RAISES {NameServer.Error} = 
  BEGIN
    RETURN self.filesystemRoot.create(name);
  END Create;

PROCEDURE Mkfile (self: T; name: NameServer.Name): File.T RAISES {Error.E} =
  BEGIN
    RETURN self.filesystemRoot.mkfile(name);
  END Mkfile;
  
BEGIN
END MountDirectory.

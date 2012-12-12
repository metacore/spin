(*
 * HISTORY
 * 09-Jul-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *
 *)

INTERFACE MountDirectoryPrivate;
IMPORT FileSystem, Directory, NameServer, MountDirectory;
IMPORT Word;


PROCEDURE methodInit(self: MountDirectory.T; root: Directory.T): MountDirectory.TPublic;

PROCEDURE methodLookup(
    self                  : MountDirectory.T; 
    name                  : NameServer.Name;
    VAR (*OUT*) component : NameServer.Name;
    VAR (*OUT*) parent    : NameServer.TBase; 
    getalias              : BOOLEAN): REFANY 
  RAISES {NameServer.Error};

PROCEDURE methodAttach(
    self : MountDirectory.T; 
    name : NameServer.Name; 
    obj  : REFANY) 
  RAISES {NameServer.Error};

PROCEDURE methodDetach(
    self            : MountDirectory.T; 
    name            : NameServer.Name; 
    VAR (*OUT*) obj : REFANY) RAISES {NameServer.Error};

PROCEDURE methodIterate(self: MountDirectory.T; cookie: Word.T)
  : NameServer.Iterator;
  
PROCEDURE methodCreate(
    self : MountDirectory.T; 
    name : NameServer.Name;
    root : NameServer.TBase): NameServer.TBase;

END MountDirectoryPrivate.

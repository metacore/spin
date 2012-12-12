(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 01-Apr-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

UNSAFE MODULE TransOS EXPORTS TransOS;
IMPORT SID;
IMPORT Fmt;
IMPORT IO;
IMPORT Text;
IMPORT NameServer;
IMPORT FileSystem;
IMPORT HostID;
IMPORT NSName;
FROM TransUtils IMPORT Msg;

(* Storage ID is just a hash value of its name. *)
  
PROCEDURE FileNameToSID (fileName: TEXT) : SID.T =
  BEGIN
    RETURN SID.T{hid := HostID.myID,
		 lid := Text.Hash(fileName) MOD (LAST(SID.LID)+1)};
  END FileNameToSID;
  
PROCEDURE SIDToFileName (sid: SID.T): TEXT =
  VAR
    cookie := 0;
    ent: ARRAY [0 .. 16] OF NameServer.Entry;
    nEnt: CARDINAL;
    dir: NameServer.T := FileSystem.Lookup(NIL, ".");
  BEGIN
    <*ASSERT sid.hid = HostID.myID*>
    
    (* Look for a file in the current dir which has the id SID.
       XXX Note there is no guarantee that sid is unique. We need to
       create a special database for this purpose, but I don't care
       for the moment. *)
    LOOP
      nEnt := dir.getEntries(cookie, ent);
      IF nEnt <= 0 THEN
	Msg("file sid ", Fmt.Int(sid.lid), " not found.");
	RETURN NIL;
      END;
      FOR i := 0 TO nEnt-1 DO
	IF FileNameToSID(NSName.ToText(ent[i].name)) = sid THEN
	  RETURN NSName.ToText(ent[i].name);
	END;
      END;
      cookie := ent[nEnt-1].cookie;
    END;
  END SIDToFileName;

PROCEDURE Exit (<*UNUSED*>x : INTEGER) =
  BEGIN
    IO.Put("transos:end\n");
  END Exit;

PROCEDURE GetDefaultLogFileName (): TEXT =
  BEGIN
    RETURN "trans_log";
  END GetDefaultLogFileName;

BEGIN
END TransOS.

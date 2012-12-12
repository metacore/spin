(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 24-Feb-97  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
MODULE VmtpNameServer;
FROM VmtpPktFormat IMPORT Entity, IPEntity;
IMPORT Text;
IMPORT Debugger;<*NOWARN*>
IMPORT IO;
IMPORT NetDb;
IMPORT VmtpUtils;

IMPORT Word;

PROCEDURE GetFamousDiscriminator(name: TEXT): INTEGER =
  BEGIN
    IF Text.Equal(name, "trans") THEN RETURN 999;
    ELSE
      RETURN Text.Hash(name);
    END;
  END GetFamousDiscriminator;

PROCEDURE Lookup (name: TEXT; VAR e: Entity): BOOLEAN =
  VAR
    pos: INTEGER;
    discName, host: TEXT;
  BEGIN
    WITH ipe = VIEW(e, IPEntity) DO
      pos := Text.FindChar(name, '@');
      IF pos = -1 THEN
	discName := name;
	host := "default";
      ELSE
	discName := Text.Sub(name, 0, pos);
	host := Text.Sub(name, pos+1);
      END;
      TRY
	IF Text.Equal(host, "default") THEN
	  ipe.ipAddr := VmtpUtils.MyIpAddr();
	ELSE
	  ipe.ipAddr := NetDb.GetHostByName(host);
	END;
      EXCEPT
      | NetDb.HostNotFound =>
	IO.Put("VmtpNameServer.Lookup:" & host & " not found.\n");
	RETURN FALSE;
      END;
      ipe.disc := Word.And(GetFamousDiscriminator(discName), 16_FFFFFFFF);
    END;
    RETURN TRUE;
  END Lookup;

BEGIN
END VmtpNameServer.

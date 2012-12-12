(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 21-Apr-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

MODULE Storage;
IMPORT Text, StorageProxy, StorageLocal;
IMPORT TransError;

PROCEDURE Open (fileName : TEXT) : T RAISES {TransError.E}=
VAR
  pos := Text.FindChar(fileName, ':');
  hostName : TEXT;
BEGIN
  IF pos >= 0 THEN
    hostName := Text.Sub(fileName, 0, pos);
    fileName := Text.Sub(fileName, pos+1);
    RETURN StorageProxy.Open(hostName, fileName);
  ELSE
    RETURN StorageLocal.Open(fileName);
  END;
END Open;

BEGIN
END Storage.

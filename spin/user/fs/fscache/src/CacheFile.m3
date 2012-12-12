(*
 * Copyright 1994-1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 21-Jun-97  David Dion (ddion) at the University of Washington
 *	Created.
 *
 *
 *)

MODULE CacheFile;

IMPORT FileStat, Word;
IMPORT IO, Error;

REVEAL T = TPublic BRANDED OBJECT
  ino: FileStat.InoT;
  validIno: BOOLEAN := FALSE;
OVERRIDES
  equal := methodEqual;
  hash  := methodHash;
END;

(* ------------------ Table Generic Support ------------------------*)

PROCEDURE SetInode(f: T; ino: FileStat.InoT) =
  BEGIN
    f.ino := ino;
    f.validIno := TRUE;
  END SetInode;

PROCEDURE methodEqual(f1: T; f2: T) : BOOLEAN =
  VAR
    fstat1, fstat2: FileStat.T;
    ino1, ino2: FileStat.InoT;
    need1 : BOOLEAN := TRUE;
    need2 : BOOLEAN := TRUE;
  BEGIN
    (* figure out why these files all have zero ids *)
    (* RETURN FileId.Equal(f1.id, f2.id); *)

    IF f1.validIno THEN
      need1 := FALSE;
      ino1 := f1.ino;
    END;
    IF f2.validIno THEN
      need2 := FALSE;
      ino2 := f2.ino;
    END;
    IF need1 OR need2 THEN
      TRY
        IF need1 THEN
          f1.stat(fstat1);
          ino1 := fstat1.ino;
          SetInode(f1, ino1);
        END;
        IF need2 THEN
          f2.stat(fstat2);
          ino2 := fstat2.ino;
          SetInode(f2, ino2);
        END;
      EXCEPT
      | Error.E(ec) =>
        IO.Put("CacheFile.Equal fstat: " & ec.message() & "\n");
        RETURN FALSE;
      END;
    END;
    RETURN ino1 = ino2;
  END methodEqual;

PROCEDURE methodHash(f: T) : Word.T =
  VAR 
    fstat: FileStat.T;
    ino: FileStat.InoT;
  BEGIN
    (* figure out why these files all have zero ids *)
    (* RETURN FileId.Hash(f.id); *)

    IF f.validIno THEN
      RETURN f.ino;
    ELSE
      TRY
        f.stat(fstat);
      EXCEPT
      | Error.E(ec) =>
        IO.Put("CacheFile.Hash fstat: " & ec.message() & "\n");
        RETURN ec.errno();
      END;
    END;
    ino := fstat.ino;
    SetInode(f, ino);
    RETURN ino;    
  END methodHash;


PROCEDURE Equal(f1, f2: T) : BOOLEAN =
  BEGIN
    RETURN f1.equal(f2);
  END Equal;

PROCEDURE Hash(f: T) : Word.T =
  BEGIN
    RETURN f.hash();
  END Hash;

BEGIN
END CacheFile.

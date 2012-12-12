(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description.
 *
 * HISTORY
 * 19-Dec-96  Brian Dewey (dewey) at the University of Washington
 *      Created.
 *)

(*
 * This module is a shell command to test the LRU-WT cache
 * file system.  It exports one command with three options:
 *
 *     -init  -- Initialize for testing.
 *               Creates directories /cache and /device, and
 *               mounts /cache on /device as an lruwt file system.
 *     -write -- Creates a file /cache/data, and writes to it.
 *               NOTE:  Currently the command only creates /cache/data
 *                      without writing any information.
 *     -verify --Verifies that /cache/data and /device/data contain
 *               the same information.
 *               NOTE:  Currently a NOP.
 *)

MODULE CacheTest;

IMPORT IO, Error, NameServer,
       File, Directory, FileSystem,
       ParseParams;

EXCEPTION E(TEXT);

PROCEDURE InitTest() RAISES {E} =
  VAR
    rootDir : Directory.T;
  BEGIN
    TRY
      rootDir := FileSystem.GetRoot();
      EVAL rootDir.create("device");
      EVAL rootDir.create("cache");
      FileSystem.Mount("lruwt", "device", "cache");
    EXCEPT
    | Error.E(e) =>
      IO.Put(e.message() & " during cache test initialization.\n");
      RAISE E("Error during initialization.\n");
    END (* TRY *);
  END InitTest;

PROCEDURE WriteTest() RAISES {E} =
  CONST
    NUMITER   = 1000;                 (* Number of iterations *)
  VAR
    fp,fp2    : File.T;
    component : NameServer.Name;
    parent    : NameServer.TBase;
    rootDir   : Directory.T;
    cacheDir  : REFANY;
    chBuf     : ARRAY [1..1] OF CHAR;
  BEGIN
      IO.Put("In CacheTest.WriteTest\n");
      rootDir := FileSystem.GetRoot();
      TRY
        cacheDir := rootDir.lookup("/cache/data", component, parent);
      EXCEPT
      | NameServer.Error (ec) =>
        IF ec = NameServer.EC.NameNotFound THEN
          cacheDir := NIL;
        ELSE
          RAISE E("WriteTest:Unknown name server error.\n");
        END (* if *);
      END (* try *);
      
      IF cacheDir # NIL THEN
        RAISE E("/cache/data already exists.\n");
      ELSE
        IO.Put("Got /cache, now creating /cache/data\n");
        TYPECASE parent OF
        | Directory.T(dir) =>
          fp := dir.mkfile("data");
          IF fp # NIL THEN
            IO.Put("Successful.\n");
          ELSE
            IO.Put("Couldn't make data file.\n");
          END (* if *);
        ELSE
          (* Bug if we get here... *)
          RAISE E("/cache is not a directory!\n");
        END (* typecase *);
      END (* if *);
  END WriteTest;

PROCEDURE Verify() RAISES {E} =
  BEGIN
    RAISE E("-verify option not yet supported.\n");
  END Verify;

PROCEDURE Run(pp : ParseParams.T) : BOOLEAN =
  BEGIN
    TRY
      IF (pp.keywordPresent("-init")) THEN
        InitTest();
      ELSIF (pp.keywordPresent("-write")) THEN
        WriteTest();
      ELSIF (pp.keywordPresent("-verify")) THEN
        Verify();
      END (* IF *);
      RETURN TRUE;
    EXCEPT
    | E(Msg) =>
      IO.Put(Msg);
      RETURN FALSE;
    END (* TRY *);
  END Run;


BEGIN
  IO.Put("Cache test extension.\n");
END CacheTest.

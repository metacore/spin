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

INTERFACE CacheFile;

IMPORT File, Word;

TYPE T <: TPublic;
     TPublic = File.T OBJECT
     METHODS
       (* for purposes of table generics *)
       equal(f2: T) : BOOLEAN;
       hash() : Word.T;
     END;

CONST Brand = "CacheFile";

CONST Debug = TRUE;

PROCEDURE Equal(f1, f2: T) : BOOLEAN;

PROCEDURE Hash(f: T) : Word.T;

END CacheFile.

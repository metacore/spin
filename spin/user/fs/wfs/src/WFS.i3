(* Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 * 
 * HISTORY
 * 09-Aug-96  Marc Fiuczynski (mef) at the University of Washington
 *	Updated to new filesystem interface.
 *
 * 01-Mar-96  Brian Bershad (bershad) at the University of Washington
 *	Created.
 *)

(* "WFS" implements a readonly web server based file system *)

INTERFACE WFS;
IMPORT File, Directory;
TYPE FileT      <: File.T;
TYPE DirectoryT <: Directory.T;
END WFS.


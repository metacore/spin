(* Copyright (C) by IBM Corporation, 1991.                    *)
(* File Udir.i3                                               *)
(* created by Dick Orgass at 11:38:53 on Tue Sep 18 1990.     *)
(*                                                            *)
(* Last modified on Fri Sep 24 09:46:53 PDT 1993 by kalsow    *)
(*      modified by Orgass on 6-Mar-91.                       *)

INTERFACE Udir;

IMPORT Utypes;
FROM Ctypes IMPORT char_star, int, long, unsigned_long, unsigned_short;

<*UNUSED*> CONST
  UdirCopyright = "Copyright (C) by IBM Corporation, 1991.";
  UdirRCSHeader = "$Header: /afs/cs/project/spin/cvsroot/spin/local/m3core/src/unix/aix-3-2/Udir.i3,v 1.1.1.1 1996/07/18 05:33:07 whsieh Exp $";
  UdirDate = "$Date: 1996/07/18 05:33:07 $";
  UdirRevision = "$Revision: 1.1.1.1 $";

(* For documentation of this interface (except NameToText) see Calls and
Subroutines Reference: Base Operating System, Volume 1, IBM Form number
SC23-2198-00, pp 1-522 to 1-524. *)

CONST
  MAXNAMELEN = 255;     (* Maximum length of component of file path name. *)
  MAXPATHLEN = 1024;    (* Maximum length of file path name. *)

TYPE
  D_name = ARRAY [0..MAXNAMELEN] OF CHAR;
  DIR = RECORD
    dd_fd: int;
    dd_blksize: int;
    dd_buf: char_star;
    dd_size: long;
    dd_flag: long;
    dd_loc: long;
    dd_curoff: long
  END;
  DIR_star = UNTRACED REF DIR;

  struct_dirent = RECORD
    d_offset: unsigned_long;
    d_ino: Utypes.ino_t;
    d_reclen: unsigned_short;
    d_namelen: unsigned_short;
    d_name: D_name;
  END;

  struct_dirent_star = UNTRACED REF struct_dirent;

<* EXTERNAL *> PROCEDURE opendir(dir: char_star): DIR_star;
<* EXTERNAL *> PROCEDURE readdir(dirPtr: DIR_star): struct_dirent_star;
<* EXTERNAL *> PROCEDURE telldir(dirPtr: DIR_star): long;
<* EXTERNAL *> PROCEDURE seekdir(dirPtr: DIR_star; location: long);
<* EXTERNAL *> PROCEDURE rewinddir(dirPtr: DIR_star);
<* EXTERNAL *> PROCEDURE closedir(dirPtr: DIR_star): int;

PROCEDURE NameToText(READONLY name: D_name): TEXT RAISES {};
(* Converts the d_name field of a struct_dirent to a TEXT and returns it.  *)

END Udir.

(* Change Log

  $Log: Udir.i3,v $
  Revision 1.1.1.1  1996/07/18 05:33:07  whsieh
  user-level version of m3core

Revision 1.3  1991/03/07  00:57:25  muller
*** empty log message ***


*)

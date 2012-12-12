(* File Udir.m3, created by Dick Orgass at 11:49:05 on Tue Sep 18 1990. *)

(*   Copyright (C) by IBM Corporation, 1991. *)

MODULE Udir;
IMPORT Text;

<*UNUSED*> CONST
  UdirImplCopyright = "Copyright (C) by IBM Corporation, 1991.";
  UdirImplRCSHeader = "$Header: /afs/cs/project/spin/cvsroot/spin/local/m3core/src/unix/aix-3-2/Udir.m3,v 1.1.1.1 1996/07/18 05:33:07 whsieh Exp $";
  UdirImplDate = "$Date: 1996/07/18 05:33:07 $";
  UdirImplRevision = "$Revision: 1.1.1.1 $";

PROCEDURE NameToText(READONLY name: D_name): TEXT RAISES {} =
(* Converts the d_name field of a struct_dirent to a TEXT and returns it.  *)
  VAR len: CARDINAL := 0;
  BEGIN
    FOR i := FIRST(name) TO LAST(name) DO
      IF name[i] = '\000' THEN EXIT ELSE INC(len) END
    END;
    RETURN Text.FromChars(SUBARRAY (name, 0, len))
  END NameToText;

BEGIN
END Udir.

(* Change Log

  $Log: Udir.m3,v $
  Revision 1.1.1.1  1996/07/18 05:33:07  whsieh
  user-level version of m3core

# Revision 1.3  1991/03/21  23:05:52  muller
# Removed the dependency on Char.i3
#
# Revision 1.2  1991/03/07  00:57:33  muller
# *** empty log message ***
#

*)

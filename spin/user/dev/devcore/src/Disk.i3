(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 * 01-Jul-97  Yasushi Saito (yasushi) at the University of Washington
 *	Added "stat" method.
 *)

(* "Disk" device interface. *)

INTERFACE Disk;
IMPORT CharDevice;
IMPORT Error;

TYPE
  T = CharDevice.T OBJECT
  METHODS
    stat(VAR buf:Stat) RAISES {Error.E};
  END;

  Stat = RECORD
    nSecs: CARDINAL; (* total # of sectors *)
    secSize: CARDINAL; (* size of a sector *)
  END;
  
END Disk.

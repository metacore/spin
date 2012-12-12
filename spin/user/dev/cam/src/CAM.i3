(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
 * HISTORY
 * 07-Aug-95  Brian Bershad (bershad) at the University of Washington
 *	Created.
 *
 *)

(* "CAM" - Common Access Method for DEC SCSI
   Interface to CAM device. The initialization code registers all the
   disks into the name server. Thus, this interface doesn't export any
   procedure by itself. 
*)

INTERFACE CAM;
END CAM.

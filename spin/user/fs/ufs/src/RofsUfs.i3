(* Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)

(*
 * HISTORY
 * 30-Nov-95  Yasushi Saito (yasushi) at the University of Washington
 *	Removed new_fp and free_fp. open_file and close_file do them
 *      automatically.
 * 21-Oct-95  Yasushi Saito (yasushi) at the University of Washington
 *	Moved all private informations into .m3. Added rudimentary
 *	opendir/readdir procedures.
 *
 * 02-Oct-95  Marc Fiuczynski (mef) at the University of Washington
 *	Created from file_io.h.
 *
 *)

(* Read-only UFS. *)

INTERFACE RofsUfs;
END RofsUfs.

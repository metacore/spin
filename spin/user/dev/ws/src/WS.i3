(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 *  1-Apr-96 Jan Sanislo (oystr) at the University of Washington
 *	Created
 *
 *)

INTERFACE WS;
IMPORT BSDtty;

(*
 * We don't really want to be a tty but
 * we do want to do ioctls.  And the hell
 * with fooling around at this point since
 * Device changes about every day.
 *)
TYPE WSDevice <: BSDtty.T;

(*TYPE WSErrorT <: Error.T;*)

CONST
   WS_BASE_ERROR = 500;
   WS_ALREADY_OPEN = 500;
   WS_INVALID_READ = 501;
   WS_INVALID_WRITE = 502;
   WS_NOT_OPEN = 503;
   WS_LAST_ERROR = 503;
   
CONST Brand = "DEC_WS_Device";

END WS.

(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 26-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Whisted.
 *)
INTERFACE TransDaemon;

PROCEDURE Loop(port: CARDINAL);
  (* Start the transactions server. "port" is TCP port number used by
     the server. If it is 0, then the "TransRPC.Port" is used. This
     procedure never returns. *)

PROCEDURE Stop();
    
END TransDaemon.

(*
 *
 * Copyright 1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *)

(*
 * HISTORY
 * 13-Jun-97  Wilson Hsieh (whsieh) at the University of Washington
 *	remove EXPORT of Main
 *
 *)


MODULE LoopbackServer;
IMPORT Loopback;
IMPORT LoopbackTunnel;

BEGIN
  Loopback.Init();
  LoopbackTunnel.Init();
  (* IO.Put("LoopbackServer module initialized.\n"); *)
END LoopbackServer.

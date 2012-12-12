(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 02-May-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed to split URT functionality.
 *
 * 26-Apr-95  Przemek Pardyak (pardy) at the University of Washington
 *	Added an argument to authorization. 
 *
 * 29-Feb-96  Brian Bershad (bershad) at the University of Washington
 *	Updated to new Auth interface.
 *
 * 06-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Initializing URT modules in right order. 
 *
 * 02-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Converted output to use IO.
 *
 * 18-Oct-95  Marc Fiuczynski (mef) at the University of Washington
 *	Exports the SPURT domain to the Nameserver.
 *
 * 16-Oct-95  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *
 *)

MODULE SpinUnixNet;
IMPORT SpinUnixNetInterface;
IMPORT NameServer;
IMPORT IO;

IMPORT OsfNet;

BEGIN
  TRY
    EVAL SpinUnixNetInterface.Export(NIL);
  EXCEPT
  | NameServer.Error (* (ec) *)  => 
    IO.PutError("SpinUnixNet init FAILED\n");
  END;

  (* Initialize modules in right order *)
  OsfNet.Init(TRUE);
  IO.Put("SpinUnixNet init done.\n"); 
END SpinUnixNet.


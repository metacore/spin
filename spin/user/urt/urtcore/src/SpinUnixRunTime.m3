(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 31-May-97  David Becker at the University of Washington
 *      Moved IoctlPosix and IoctlCompat to the unix emulator sphinx
 *
 * 11-Feb-97  Robert Grimm (rgrimm) at the University of Washington
 *      Removed Identity
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


MODULE SpinUnixRunTime (* EXPORTS Main, SpinUnixRunTime*);
IMPORT SpinUnixRunTimeInterface;
IMPORT NameServer;
IMPORT IO;
IMPORT Auth;

IMPORT Ioctl, Net, Mbuf;  

TYPE
  T = Auth.T OBJECT
    all: BOOLEAN;
  OVERRIDES
    authorize := Authorize;
  END;

PROCEDURE Authorize (self: T; 
                     <* UNUSED *> key: Auth.Key;
                     <* UNUSED *> arg: REFANY): BOOLEAN =
  BEGIN
    IF self.all THEN RETURN TRUE; 
    ELSE IO.PutError(" -- Auth Failed\n"); RETURN FALSE; END;
  END Authorize;

VAR t : T;
BEGIN
  TRY
    t := NEW(T);
    t.all := TRUE;
    EVAL SpinUnixRunTimeInterface.Export(t);
  EXCEPT
  | NameServer.Error (* (ec) *)  => 
    IO.PutError("SpinUnixRunTime init FAILED\n");
  END;
  (* Initialize modules in right order *)
  Ioctl.Init();
  Net.Init();
  Mbuf.Init();
  IO.Put("SpinUnixRunTime init done.\n"); 
END SpinUnixRunTime.


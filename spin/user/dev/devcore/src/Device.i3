(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
 * HISTORY
 *)

(* "Device" interface defines the super type for device meta data like
   its name.  Also nameserver front-end procs are here which manage
   devices placed in the nameserver.
 *)

INTERFACE Device;
IMPORT Auth, NameServer, Error AS ErrorBase;

CONST Brand = "Device"; (* Use this name to link with this extension *)

TYPE T <: Public; (* The device supertype *)
  Public = OBJECT METHODS
    name() : TEXT;
    (* Return the registered name of the device. *)
  END;
  
TYPE Error <: ErrorBase.T;

<*OBSOLETE*>
PROCEDURE Register(name: TEXT; dev: T; auth: Auth.T:=NIL)
  RAISES {NameServer.Error} ;

<*OBSOLETE*>
PROCEDURE Lookup (name: TEXT): REFANY RAISES {NameServer.Error};
END Device.


(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 03-Apr-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

(* "HostID.T" defines an unique host ID.
 This is typically the IP address of the host
 written in big endian.
 *)

INTERFACE HostID;

(* 4 bytes for IP address *)
TYPE T = [0 .. 16_FFFFFFFF];

CONST Void = 0;
  
VAR myID: T;

PROCEDURE Init(); (* set myID the value *)
PROCEDURE HostNameToT(host: TEXT): T;
(* gethostbyname() wrapper. *)

END HostID.

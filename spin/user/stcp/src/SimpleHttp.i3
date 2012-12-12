(*
 * Copyright 1997, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 11-Apr-97  Tsutomu Owa (owa) at the University of Washington
 *	Made SetServer private.  Changed Get() I/F.
 *
 * 03-Feb-97  Tsutomu Owa (owa) at the University of Washington
 *	Created.
 *)

INTERFACE SimpleHttp;

IMPORT Salnet;

EXCEPTION
  (* code returned from server.  see user/httpd for more info *)
  Error(TEXT);

(*
TYPE HttpdInfo = RECORD
  server : TEXT;
  ipAddress : IpPktFormat.AddressArray;
  port : CARDINAL;
  httpdType : [0..1];		(* 0 -- Netscape Commerce. 1 -- SPIN httpd *)
END;
*)

PROCEDURE Init();
(* XXX.  It's private now.
PROCEDURE SetServer(server : TEXT);
 *)
PROCEDURE Get(path : TEXT ; server : Salnet.IpAddr) : REF ARRAY OF CHAR RAISES {Error};
PROCEDURE FreeMemory();
PROCEDURE Uninit();
(* deregister the etherevent handler *)
END SimpleHttp.

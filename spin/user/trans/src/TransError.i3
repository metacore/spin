(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 26-Mar-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
INTERFACE TransError;
IMPORT Error;

CONST
  SUCCESS = 0;
  INVALID_STORAGE = 3000;
  NO_SUCH_STORAGE = 3001;
  CANT_ABORT = 3002;
  ABORTED = 3003;
  DEADLOCK = 3004;
  TIMEOUT = 3005;
  SERVER_RPC_FAILURE = 3006; (* failure in one of queued rpc request. *)
  
TYPE T <: Error.T;

PROCEDURE Raise(code: INTEGER) RAISES {Error.E};
  (* "Raise" always raises an exception *)
  
END TransError.

INTERFACE RPC;
IMPORT Ctypes, XDR, Mbuf;

EXCEPTION Failed(Failure);
TYPE Failure = TEXT;

CONST
  (* Current RPC version. *)
  RPCVERS              : Ctypes.unsigned_int = 2;

  (* Messages types. *)
  CALLMSG              : Ctypes.unsigned_int = 0;
  REPLYMSG             : Ctypes.unsigned_int = 1;

  (* Reply types. *)
  MSG_ACCEPTED         : Ctypes.unsigned_int = 0;
  MSG_DENIED           : Ctypes.unsigned_int = 1;

  (* Accepted return codes. *)
  ACCEPT_SUCCESS       : Ctypes.unsigned_int = 0; (* RPC executed successfully       *)
  ACCEPT_PROG_UNAVAIL  : Ctypes.unsigned_int = 1; (* remote hasn't exported program  *)
  ACCEPT_PROG_MISMATCH : Ctypes.unsigned_int = 2; (* remote can't support version #  *)
  ACCEPT_PROC_UNAVAIL  : Ctypes.unsigned_int = 3; (* program can't support procedure *)
  ACCEPT_GARBAGE_ARGS  : Ctypes.unsigned_int = 4; (* procedure can't decode params   *)

  (* Rejection return codes *)
  REJECT_RPC_MISMATCH  : Ctypes.unsigned_int = 0; (* RPC version number # 2           *)
  REJECT_AUTH_ERROR    : Ctypes.unsigned_int = 1; (* remote can't authenticate caller *)

  (* Auth failure reasons. *)
  AUTH_BADCRED         : Ctypes.unsigned_int = 1; (* bad credentials               *)
  AUTH_REJECTEDCRED    : Ctypes.unsigned_int = 2; (* client must begin new session *)
  AUTH_BADVERF         : Ctypes.unsigned_int = 3; (* bad verifier                  *)
  AUTH_REJECTED_VERF   : Ctypes.unsigned_int = 4; (* verifier expired or replayed  *)
  AUTH_TOOWEAK         : Ctypes.unsigned_int = 5; (* rejected for security reasons *)

  (* Authentication types. *)
  AUTH_NULL            : Ctypes.unsigned_int = 0;
  AUTH_UNIX            : Ctypes.unsigned_int = 1;
  AUTH_SHORT           : Ctypes.unsigned_int = 2;
  AUTH_DES             : Ctypes.unsigned_int = 3;

TYPE Credentials = RECORD
  flavor: Ctypes.unsigned_int;    (* flavor of auth info *)
  len   : Ctypes.unsigned_int;
  opaque: ARRAY [1..400] OF CHAR; (* generic buffer to hold auth info *)
END;

PROCEDURE GetCallHeader (
    m             : Mbuf.T; 
    VAR pos       : CARDINAL;
    VAR xid       : Ctypes.unsigned_int;
    VAR prog      : Ctypes.unsigned_int;
    VAR vers      : Ctypes.unsigned_int;
    VAR proc      : Ctypes.unsigned_int;
    VAR cred      : Credentials;
    VAR verf      : Credentials)
  RAISES {Failed, XDR.Failed};

PROCEDURE PutReplyHeader (
    m             : Mbuf.T;
    VAR pos       : CARDINAL;
    xid           : Ctypes.unsigned_int;
    accept        : Ctypes.unsigned_int;
    code          : Ctypes.unsigned_int;
    authWhy       : Ctypes.unsigned_int := 0;
    low, high     : Ctypes.unsigned_int := 0;
    READONLY verf : Credentials) : CARDINAL
  RAISES {XDR.Failed};

TYPE
  RejectReason = {RPCVersion, ProgUnavail, ProgMismatch, BadProc, BadArgs};

CONST
  Rejections = ARRAY RejectReason OF TEXT {
  "RPC version mismatch",
  "Program unavailable",
  "Program mismatch",
  "Bad procedure",
  "Bad arguments"
  };

PROCEDURE SendRejection (
    m       : Mbuf.T; 
    VAR pos : CARDINAL;
    xid     : Ctypes.unsigned_int;
    vers    : Ctypes.unsigned_int;
    READONLY verf    : Credentials;
    why     : RejectReason) : CARDINAL
    RAISES {Failed, XDR.Failed};
END RPC.

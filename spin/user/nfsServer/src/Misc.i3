INTERFACE Misc;
IMPORT Mbuf, RPC, XDR, Ctypes;

PROCEDURE GetMbuf(totlen:CARDINAL):Mbuf.T;

PROCEDURE SendRejection(
    why: RPC.RejectReason;
    xid           : Ctypes.unsigned_int;
    vers          : Ctypes.unsigned_int;
    READONLY verf : RPC.Credentials;
    VAR output    : Mbuf.T; 
    VAR outputPos : CARDINAL) : CARDINAL
  RAISES { RPC.Failed, XDR.Failed};
END Misc.

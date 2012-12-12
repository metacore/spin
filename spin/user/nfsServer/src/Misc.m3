MODULE Misc;
IMPORT Mbuf, RPC, XDR, Ctypes, IO;

PROCEDURE GetMbuf(totlen:CARDINAL):Mbuf.T = 
  VAR
    m : Mbuf.T;
  BEGIN
    m := Mbuf.m_get(Mbuf.M_WAIT, Mbuf.MT_DATA);
    m.mh_hdr.mh_len := totlen;
    RETURN m;
  END GetMbuf;

PROCEDURE SendRejection(
    why: RPC.RejectReason;
    xid           : Ctypes.unsigned_int;
    vers          : Ctypes.unsigned_int;
    READONLY verf : RPC.Credentials;
    VAR output    : Mbuf.T; 
    VAR outputPos : CARDINAL) : CARDINAL
  RAISES { RPC.Failed, XDR.Failed} = 
  VAR 
    size : CARDINAL;
  BEGIN
    outputPos := 0;
    output := GetMbuf(Mbuf.MLEN); (* XXX get new mbuf *)
    IO.Put("Sending Rejection: ");
    IO.Put(RPC.Rejections[why]);
    IO.Put("\n");
    size := RPC.SendRejection(output, outputPos, xid, vers, verf, why);
    output.mh_hdr.mh_len := size;
    RETURN size;
  END SendRejection;


BEGIN
END Misc.

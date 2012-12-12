MODULE RPC;
IMPORT Ctypes, XDR, Mbuf, IO;

PROCEDURE GetCallHeader (
    m        : Mbuf.T; 
    VAR pos  : CARDINAL;
    VAR xid  : Ctypes.unsigned_int;
    VAR prog : Ctypes.unsigned_int;
    VAR vers : Ctypes.unsigned_int;
    VAR proc : Ctypes.unsigned_int;
    VAR cred : Credentials;
    VAR verf : Credentials)
  RAISES {Failed, XDR.Failed} =

  (* Decode credentials.  *)
  PROCEDURE GetAuth (VAR c:Credentials)
    RAISES {XDR.Failed} =
    BEGIN
      c.flavor := XDR.GetWord32(m,pos); (* auth flavor *)
      c.len := XDR.GetWord32(m,pos);    (* auth info size *)
      IF c.len < 0 OR c.len > 400 THEN
        IO.PutError("Array bounds error");
        RAISE XDR.Failed(XDR.Failure.SourceNoSpace);
      END;
      IF c.len > 0 THEN 
        (* Charlie: A check is made here. *)
        XDR.GetBytes(m,pos,SUBARRAY(c.opaque,0,c.len));
      END;
  END GetAuth;

  BEGIN
    xid := XDR.GetWord32(m,pos);

    IF XDR.GetWord32(m,pos) # CALLMSG THEN
      RAISE Failed("not a call msg");
    END;
    IF XDR.GetWord32(m,pos) # RPCVERS THEN 
      RAISE Failed("incorrect rpc version"); 
    END;

    prog := XDR.GetWord32(m,pos);
    vers := XDR.GetWord32(m,pos);
    proc := XDR.GetWord32(m,pos);
    GetAuth(cred);
    GetAuth(verf);
  END GetCallHeader;

PROCEDURE PutReplyHeader (
    m             : Mbuf.T;
    VAR pos       : CARDINAL;
    xid           : Ctypes.unsigned_int;
    accept        : Ctypes.unsigned_int;
    code          : Ctypes.unsigned_int;
    authWhy       : Ctypes.unsigned_int := 0;
    low, high     : Ctypes.unsigned_int := 0;
    READONLY verf : Credentials) : CARDINAL
  RAISES {XDR.Failed} =
  VAR 
    size : CARDINAL;

  (* Encode credentials.  *)
  PROCEDURE PutAuth (READONLY cred: Credentials)
    RAISES {XDR.Failed} =
    BEGIN
      size := size + XDR.PutWord32(m,pos,cred.flavor);
      size := size + XDR.PutWord32(m,pos,cred.len);
      IF cred.len < 0 OR cred.len > 400 THEN 
        RAISE XDR.Failed(XDR.Failure.SinkNoSpace);
      END;
      IF cred.len > 0 THEN 
        (* Charlie: A check is made here. *)
        size := size + XDR.PutBytes (m,pos,SUBARRAY(cred.opaque,0,cred.len));
      END;
    END PutAuth;

  BEGIN
    size := XDR.PutWord32(m, pos, xid);
    size := size + XDR.PutWord32(m, pos, REPLYMSG);
    size := size + XDR.PutWord32(m, pos, accept);
    CASE accept OF              <* NOWARN *>
      MSG_ACCEPTED =>
        PutAuth(verf);
        size := size + XDR.PutWord32(m, pos, code);
        CASE code OF            <* NOWARN *>
          ACCEPT_SUCCESS, 
          ACCEPT_PROG_UNAVAIL, 
          ACCEPT_PROC_UNAVAIL,
          ACCEPT_GARBAGE_ARGS => (* no data *)
        | ACCEPT_PROG_MISMATCH =>
          size := size + XDR.PutWord32(m, pos, low);
          size := size + XDR.PutWord32(m, pos, high);
        END;
    | MSG_DENIED =>
      size := size + XDR.PutWord32(m, pos, code);
        CASE code OF            <* NOWARN *>
          REJECT_RPC_MISMATCH =>
          size := size + XDR.PutWord32(m, pos, low);
          size := size + XDR.PutWord32(m, pos, high);
        | REJECT_AUTH_ERROR => 
          size := size + XDR.PutWord32(m, pos, authWhy);
        END;
    END;
    RETURN size;
  END PutReplyHeader;

PROCEDURE SendRejection (
    m       : Mbuf.T; 
    VAR pos : CARDINAL;
    xid     : Ctypes.unsigned_int;
    vers    : Ctypes.unsigned_int;
    READONLY verf    : Credentials;
  why     : RejectReason) : CARDINAL
    RAISES {Failed, XDR.Failed} <*NOWARN*> =
  BEGIN
    CASE why OF
    | RejectReason.RPCVersion =>
      RETURN PutReplyHeader(
          m      := m,
          pos    := pos,
          xid    := xid, 
          accept := MSG_DENIED,
          code   := REJECT_RPC_MISMATCH, 
          low    := RPCVERS, 
          high   := RPCVERS, 
          verf   := verf);

    | RejectReason.ProgUnavail =>
      RETURN PutReplyHeader(
          m      := m,
          pos    := pos,
          xid    := xid, 
          accept := MSG_ACCEPTED,
          code   := ACCEPT_PROG_UNAVAIL, 
          verf   := verf);

    | RejectReason.ProgMismatch =>
      RETURN PutReplyHeader(
          m      := m,
          pos    := pos,
          xid    := xid, 
          accept := MSG_ACCEPTED,
          code   := ACCEPT_PROG_MISMATCH, 
          low    := vers,
          high   := vers, 
          verf   := verf);

    | RejectReason.BadProc =>
      RETURN PutReplyHeader(
          m      := m,
          pos    := pos,
          xid    := xid, 
          accept := MSG_ACCEPTED,
          code   := ACCEPT_PROC_UNAVAIL, 
          verf   := verf);

    | RejectReason.BadArgs =>
      RETURN PutReplyHeader(
          m      := m,
          pos    := pos,
          xid    := xid, 
          accept := MSG_ACCEPTED,
          code   := ACCEPT_GARBAGE_ARGS, 
          verf   := verf);
    END;
  END SendRejection;

BEGIN
END RPC. 

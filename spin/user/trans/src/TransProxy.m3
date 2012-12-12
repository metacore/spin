(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *)

MODULE TransProxy;
IMPORT TransRPC, TID;
IMPORT Error;
IMPORT IO;

PROCEDURE Resolve (tid : TID.T) : BOOLEAN =
  VAR
    hid := TID.GetHostID(tid);
    server := TransRPC.OpenServer(hid, 0);
    out := TransRPC.CreateSendBuf();
    in := TransRPC.CreateRecvBuf();
    commit : BOOLEAN;
  BEGIN
    TRY
      out.packInt(TransRPC.ResolveTransInDoubt);
      out.packInt(tid);
      EVAL TransRPC.DoRPC(server, out, in);
      commit := in.unpackBool();
      in.endUnpack();
      TransRPC.CloseServer(server);
    EXCEPT
    | Error.E(e) =>
      IO.Put("transproxy.resolve:" & e.message());
    END;
    RETURN commit;
  END Resolve;

BEGIN
END TransProxy.

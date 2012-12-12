(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 *)

INTERFACE UdpVideo;
IMPORT Ctypes;
IMPORT Word;

CONST
  Data : Word.T = 1;
  Req  : Word.T = 2;
  Resp : Word.T = 3;

TYPE DataPacket = RECORD
  type: BITS 32 FOR Ctypes.unsigned_int;
  (* data : ADDRESS; *)
END;

TYPE ReqPacket = RECORD
  type: Ctypes.unsigned_int;
  req : CARDINAL;
END;
  
TYPE Response = { OK, NotFound };
TYPE RespPacket = RECORD
  type: Ctypes.unsigned_int;
  resp: Response;
END;

END UdpVideo.

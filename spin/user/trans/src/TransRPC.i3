(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 29-Nov-96  Yasushi Saito (yasushi) at the University of Washington
 *	Whisted. Separated T into read and write buffer.
 *)

(* Marshall/unmarshalling various elementary data types into arrays *)

INTERFACE TransRPC;
IMPORT SID;
IMPORT TID;
IMPORT HostID;
IMPORT Error;
IMPORT Ctypes;

VAR 
  Port: CARDINAL;
  (*Default TCP port number*)

(* RPC Error codes *)
CONST
  OK = 0;
  UnknownFunc = 5001;
  LockTimeout = 5002;
  
(* RPC Function IDs *)
CONST
  Hello = 100; (* Welcome message *)
  Open = 101; (* Storage *)
  Read = 102; (* Storage *)
  Close = 103; (* Storage *)
  Prepare = 104; (* Trans *)
  Commit = 105; (* Trans *)
  Abort = 106; (* Trans *)
  OnePhaseCommit = 107; (* Trans *)
  LogUndo = 108; (* Storage *)
  LogRedo = 109; (* Storage *)
  ResolveTransInDoubt = 110; (* Trans, used in 2P recovery *)
(*  PollStorage = 112; (* Storage, used in 2P recovery when the storage
			crashed after it voted YES, but before it sent
			commit completion ack to the trans manager. *) *)
  Last = 110;

(* Format of a RPC request:

   One RPC packet may contain multiple requests.

   | len: 4
   | nrequests: 4
   | requests: array [0 .. nrequests-1] of request

   The first 4 bytes of a packet is the total length of the packet,
   including the "len" field itself. The next 4 bytes is the # of
   requests. The format of each request depends on the type of the
   request. See StorageProxy.m3 also.

   All the requests other than the last one should require only yes or no
   reply. If the RPC requires some other complicated answer, this should be
   sent as a separate packet.

   XXX All the integers are machine native words. This means that the
   protocol is machine dependent. Alphas can only talk to Alphas, x86s
   can only talk to x86s. I'll fix this soon so that at least machines
   with same byte order can talk to each other.
*)
  
TYPE
  Header = RECORD
    len, nRequests: Ctypes.int;
  END;
  (* RPC header structure *)
  
  SendBuf <: SendBufPublic;
  SendBufPublic = OBJECT
    (*XXX this structure should be hidden in arch dependent directory *)
    nRequests: CARDINAL;
    buf: REF ARRAY OF CHAR;
    idx: INTEGER;
  METHODS
    clear();
    (* Prepare writing on the buffer. Clears all the buffer contents. *)
    
    startNewCommand();
    (* Start appending new RPC command *)
    
    stretchIfNecessary(size: INTEGER);
    (* Stretch the size of buffer so that it can hold at least "size" bytes. *)
    
    packInt(val: INTEGER);
    (* packInt2(v1, v2: INTEGER);
    packInt3(v1, v2, v3: INTEGER);
    packInt4(v1, v2, v3, v4: INTEGER);
    packInt5(v1, v2, v3, v4, v5: INTEGER); *)
    packHeader2(op: INTEGER; sid: SID.T);
    packHeader3(op: INTEGER; sid: SID.T; tid: TID.T);
    packHeader4(op: INTEGER; sid: SID.T; tid: TID.T; v: INTEGER);
    packHeader5(op: INTEGER; sid: SID.T; tid: TID.T; v1, v2: INTEGER);
    packText(s: TEXT);
    packArray(READONLY x: ARRAY OF CHAR);
    packBool(b: BOOLEAN);
    endPack(); (* notify the rpc stub that we have no more bytes to pass. *)
  END;
  
  RecvBuf = OBJECT
  (* This is (in C++ terminology) a pure abstract class; the actual
     implementation is provided in either UNIXRPC or SpinRPC. *)
    nRequests: CARDINAL;
  METHODS
    currentIdx(): CARDINAL; (* return the current read ptr *)
    unpackInt(): INTEGER;
    unpackInt2(VAR v1, v2: INTEGER);
    unpackInt3(VAR v1, v2, v3: INTEGER);
    unpackInt4(VAR v1, v2, v3, v4: INTEGER);
    unpackHeader(VAR sid: SID.T);
    unpackHeader2(VAR sid: SID.T; VAR tid: TID.T);
    unpackHeader3(VAR sid: SID.T; VAR tid: TID.T; VAR v: INTEGER);
    unpackHeader4(VAR sid: SID.T; VAR tid: TID.T; VAR v1, v2: INTEGER);
    
    unpackText(): TEXT;
    unpackArray(VAR x: ARRAY OF CHAR): CARDINAL;
    unpackBool(): BOOLEAN;
    endUnpack();(* notify the rpc stub that we have no more args to unpack. *)
  END;
  
TYPE
  Server <: ServerPublic;
  (* "Server" represents a virtual curcuit between two hosts. *)
  
  ServerPublic = OBJECT
    hid: HostID.T;
  END;

PROCEDURE CreateSendBuf(size: CARDINAL := 8192): SendBuf;
PROCEDURE CreateRecvBuf(): RecvBuf;
PROCEDURE OpenServer(hid: HostID.T; port: CARDINAL): Server RAISES {Error.E};
PROCEDURE CloseServer(server: Server) RAISES {Error.E};
  
PROCEDURE DoRPC(server: Server; out: SendBuf; in: RecvBuf): BOOLEAN;
(* Send the request packet "out" to "server", and get reply into "in". *)

  
PROCEDURE QueueRPC(server: Server; out: SendBuf);
(* Queue in the request into "out". This is a nop actually. *)
  
END TransRPC.

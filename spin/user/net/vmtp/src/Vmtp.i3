(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 24-Mar-97  Yasushi Saito (yasushi) at the University of Washington
 *	Whisted.
 * html
 *)

(*
   VMTP(Versatile Message Transaction Protocol) is an efficient protocol
   designed for remote procedure calls. See RFC1045 for the specification.

   This module implements a subset of VMTP, as described in yasushi's
   <a href="http://www.cs.washington.edu/homes/yasushi/552/vmtp.doc">
   552 project report</a>.

   *)

INTERFACE Vmtp;
IMPORT VmtpPktFormat;
IMPORT Mbuf;

TYPE
  T <: Public;
  Public = OBJECT
    TC1 := 10000;
    TC2 := 5000;
    TC3 := 3000;
    TS1 := 3000;
    TS2 := 3000;
    TS3 := 3000;
    TS4 := 3000;
    TS5 := 3000;
  END;
  (* Various timeout values. Unit is milliseconds. See rfc1045.
     
     | TS1 
       The estimated maximum expected interpacket time.  Set
       when waiting for subsequent Request packets within a
       packet group before timing out.
     | TS2
       The time to wait to hear from a client before
       terminating the server processing of a Request.  This
       limits the time spent processing orphan calls, as well
       as limiting how out of date the server's RECORD of the
       Client state can be.  In particular, TS2 should be
       significantly less than the minimum time within which it
       is reasonable to reuse a transaction identifier.
     | TS3
       Estimated roundtrip time to the Client
     | TS4
       The time to wait after sending a Response before discarding the
       state associated with the Request which allows it to filter
       duplicate Request packets and regenerate the Response.
     | TS5
       The time to wait for an acknowledgment after sending a
       Response before retransmitting the Response, or giving
       up (after some number of retransmissions).
     *)
  
  LocalClient <: T;
  (* "LocalClient" denotes a proxy server on the client side. *)
  RemoteClient <: T;
  (* "RemoteClient" denotes a proxy client on the server side. *)
  LocalServer <: T;
  (* "LocalServer" is a server control block. *)
  
  MCB = VmtpPktFormat.MCB;
  (* MCB is a user-settable protocol control block. *)
  
  Entity = VmtpPktFormat.Entity;
  (* Entity is a opaque ID that designates a communication endpoint. *)

  Mbufs = RECORD
    mbufs: ARRAY [0..31] OF Mbuf.T;
    offs: ARRAY [0..31] OF CARDINAL;
  END;
  (* Incoming packets are passed in to this record. Max. 31 mbufs are
     passed. "mbufs" and "offs" are guaranteed to have no gaps in them. *)

VAR
  Debug: BOOLEAN;
  
PROCEDURE Invoke(c: LocalClient;
		 VAR (*INOUT*)mcb: MCB; input: Mbuf.T;
		 VAR (*OUT*)reply: Mbufs;
		 timeout: INTEGER): INTEGER;
(* Send a request "input" to the server "mcb.server", and receive reply.
   Returns one of vmtp error codes. "Vmtp.OK" means successful.
   "timeout" is ignored now. *)
  
PROCEDURE RecvRequest(s: LocalServer;
		      VAR (*OUT*)mcb: MCB;
		      VAR (*OUT*)rc: RemoteClient;
		      VAR (*OUT*)request: Mbufs;
		      timeout: INTEGER);
(* Receive a request from a client. "timeout" is ignored now. *)
  
PROCEDURE SendReply(rc: RemoteClient; VAR mcb: MCB; input: Mbuf.T);
(* Send a reply to the client "rc". *)

(*
   <b>Server/client registration.</b>
*)
  
PROCEDURE CreateClient(): LocalClient;
  (* Create a client state record. *)
  
PROCEDURE CreateServer(name: TEXT): LocalServer;
  (* Create a server state record. *)
  
PROCEDURE GetEntityID(t: T): VmtpPktFormat.Entity;
PROCEDURE DeleteCSR(t: T);
  (* XXX don't use this currently!! *)
  
(*
   <b>Return codes.</b>
*)  
CONST
  OK = 0;
  RETRY = 1;
  RETRY_ALL = 2;
  BUSY = 3;
  NONEXISTENT_ENTITY = 4;
  ERROR = 8;
  BAD_TRANSACTION_ID = 10;
  STREAMING_NOT_SUPPORTED = 11;
  RETRANS_TIMEOUT = 13;
  USER_TIMEOUT = 14;
  RESPONSE_DISCARDED = 15;
  SECURITY_NOT_SUPPORTED = 16;
  BAD_REPLY_SEGMENT = 17;
  LAST_CODE = 17;

(*
   <b>Reserved request codes.</b>
*)
  
  ReqProbeEntity = 16_00000101;
  (* ProbeEntity(CREntity, entityId, authDomain) -> (code, <staterec>) *)
  RepProbeEntity = 16_00000102;
  ReqNotifyVmtpClient = 16_0000010F;
  (* NotifyVmtpClient(client,cntrl,recSeq, transact,delivery,code)->() *)
  ReqNotifyVmtpServer = 16_00000110;
  (* NotifyVmtpServer(server,client, transact,delivery,code)->() *)
  ReqEntityDeleted = 16_00000111;
  (* Notify the server that the client is deleted. *)

END Vmtp.

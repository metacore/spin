(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 12-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Added WalkIfList, ioctl, and SetIfOutput procedure to abstract
 *	unsafe/trusted access to C routines and datastructures.
 *
 * 06-Nov-95  Marc Fiuczynski (mef) at the University of Washington
 *	Hack.
 *
 *)

INTERFACE IfPublic;
IMPORT If;
IMPORT Ioctl;
IMPORT Net;
IMPORT Ctypes;
IMPORT SocketRep;
IMPORT Mbuf;
IMPORT Word;

CONST Brand = "IfPublic";

TYPE T = RECORD
  name: TEXT;
  unit: CARDINAL;
  ifp: UNTRACED REF If.ifnet;
END;

TYPE NewIfEvent = PROCEDURE (READONLY ifp: UNTRACED REF If.ifnet):BOOLEAN;
PROCEDURE NewIf(READONLY ifp: UNTRACED REF If.ifnet):BOOLEAN; 
(* Announce to the world that a new interface dynamically popped up. *)

PROCEDURE ioctl(ifp: ADDRESS; cmd: Ioctl.T; VAR data: ARRAY OF Net.BYTE): Ctypes.int;
(* make if ioctl to device *)

PROCEDURE WalkIfList(closure: PROCEDURE(READONLY ifp: UNTRACED REF If.ifnet));
(* Walks the if list datastructure and upcalls to the user provided closure for
   each interface in the list.
*)

PROCEDURE SetIfOutput(ifoutput:PROCEDURE(VAR ifp: If.ifnet; mbuf: Mbuf.T; VAR s: SocketRep.sockaddr; rte: ADDRESS): Ctypes.int);
(* XXX This procedure erroneously assumes that the first registered device is the ethernet device. Requires fix. *)

PROCEDURE GetIf(name:TEXT; unit:INTEGER): UNTRACED REF If.ifnet;
(* XXX This procedure erroneously assumes that the first registered device is the ethernet device. Requires fix. *)

(* generic table support *)
PROCEDURE Equal(a,b:T): BOOLEAN;
PROCEDURE Hash(a:T) : Word.T;

END IfPublic.

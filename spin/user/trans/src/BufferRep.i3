(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 03-Apr-97  Yasushi Saito (yasushi) at the University of Washington
 *	Renamed from Buffer.i3
 *	
 * 08-Feb-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

(* Buffer manages a physical page. A page is a fixed sized(MMU page)
   array of bytes. It's actual size and contents aren't not relevant 
   to the Buffer.
 *)  
INTERFACE BufferRep;
IMPORT WAL, Q;
IMPORT Storage;
IMPORT PhysAddr;
IMPORT LightMutex;
IMPORT ActiveTrans;
IMPORT Buffer;

REVEAL 
  Buffer.T = Q.T BRANDED OBJECT
    (* The queue is used to link buffers to activetrans that modified
       the transaction. *)
    at: ActiveTrans.T;
    st: Storage.T; (* the storage the content belongs to. *)
    pos: Buffer.Offset; (* page location (page unit) in the database file *)
    dirty: BOOLEAN;
    valid: BOOLEAN;
    (* page is dirty iff lsn # 0. XXX This means we don't need "dirty"
       field. *)
    lsn: WAL.LSN; (* log record for the last update on this buffer *)
    recoveryLSN: WAL.LSN; (* first log record that caused the
			     buffer to be dirty *)
    frame: PhysAddr.T; (* the content. *)
    shadow: PhysAddr.T;
    hashLink: Buffer.T; (* link to the next entry in the same hash bucket *)
    mu: LightMutex.T;
  END;

TYPE
  T = Buffer.T;
CONST
  Brand = "Buffer";
  
END BufferRep.

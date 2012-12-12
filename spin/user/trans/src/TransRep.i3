(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 01-Jan-97  Yasushi Saito (yasushi) at the University of Washington
 *	Renamed from TransCommonRep.
 *)
INTERFACE TransRep;
IMPORT TransT;
IMPORT TID;
IMPORT TransGroup;
IMPORT TransMode;
IMPORT WAL;
IMPORT TransService;
IMPORT LightMutex;

CONST
  MaxService = 8; (* Max # of local services that can participate in
		     a single transaction. *)
  Brand = "TransRep";
  
REVEAL
  TransT.T = BRANDED OBJECT
    next, prev: TransT.T;
    
    tid: TID.T;
    group: TransGroup.T; (* The [TransGroup:transaction group] that this
			    transaction belongs to. *)
    mode: TransMode.Set;
    mu: LightMutex.T;
    lsn: WAL.LSN; (* Last LSN of the record produced by this transaction.
		     This includes the records produced by transaction only
		     (ie, 2pc records only), and does not include
		     records produced by storages. *)
    aborted: BOOLEAN; (* TRUE if this transaction is aborted. Aborted
			 transaction can't proceed any more *)
    service: ARRAY [0 .. MaxService] OF TransService.T;
    (* List of storages participating in a single  transaction. *)
    nService: [0 .. MaxService];
    (* # of non-nil elements in "storages". *)
  END;

TYPE T = TransT.T;

END TransRep.

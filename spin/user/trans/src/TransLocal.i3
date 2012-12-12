(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *)

(* "TransLocal.T" is the control block for a local transaction.

 A local transaction is a transaction controled by the trans manager
 on this host, but it may contain remote storage accesses. 
 
 *)
INTERFACE TransLocal;
IMPORT TransCommon, WAL;
  
TYPE T = TransCommon.T BRANDED OBJECT
  lsn: WAL.LSN; (* Last LSN of the record produced by this transaction.
		  This includes the records produced by transaction only
		  (ie, 2pc records only), and does not include
		  records produced by storages. *)
  
  aborted: BOOLEAN; (* TRUE if this transaction is aborted. Aborted
			transaction can't proceed any more *)
END;

END TransLocal.

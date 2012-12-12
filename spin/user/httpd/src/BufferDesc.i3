(*
 *
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 16-Oct-96  Marc Fiuczynski (mef) at the University of Washington
 *	Decreased the maxBufferDesc variable to 128.  It was set to 5000,
 *	which in the works case could all contain Buffer.T with a page
 *	worth of data, which roughly adds up to 40MB.  Lets not be so
 *	aggressive, yet.
 *
 * 02-Dec-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Buffer manager and recycler.
 *)
INTERFACE BufferDesc;
IMPORT FastList, Buffer, BlockList;

TYPE T = FastList.T OBJECT
    blocklist: BlockList.T;
    blockno: CARDINAL;
    buffer: Buffer.T;
END;

VAR
  maxBufferDesc : CARDINAL := 128 (* was 5000 *);

PROCEDURE Allocate() : T;

PROCEDURE Deallocate(bufferdesc: T);

END BufferDesc.

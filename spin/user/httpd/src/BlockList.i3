(*
 *
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 15-May-96  Marc Fiuczynski (mef) at the University of Washington
 *	merged in tryretrieveblock form egs.
 *
 * 29-Nov-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. List of blocks in a file.
 *
 *)
INTERFACE BlockList;
IMPORT Buffer, Extent;

CONST Brand = "BlockList";

CONST
  DefaultPolicy = Policy.Swap;
  MaxBlockNo = 1024;

TYPE
  Block = Buffer.T;
  BlockNo = [0..MaxBlockNo-1];
  (*
   * Swap swaps out the entire blocklist.
   * LRU writes out the least recently used blocks onto secondary storage
   * MRU writes out the most recently used stuff onto secondary storage
   * Random writes out random blocks out to the disk.
   * The last three policies write out blocks until the total size of the
   * blocks in memory drops below maxsize.
   *)
  Policy = {Swap, LRU, MRU, Random};

TYPE Public = OBJECT 
    refcount: INTEGER;
  METHODS
    (* release the space consumed by this object *)
    destroy();
    (* add a block to the blocklist *)
    addBlock(blockno: BlockNo; block: Block; size: CARDINAL);
    (* retrieve a block, locked, no copy is done *)
    retrieveBlock(blockno: BlockNo; VAR size: CARDINAL) : Block;
    (* retrieve a block, no copy, locked if in memory, fail otherwise *)
    tryRetrieveBlock(blockno: BlockNo; VAR size: CARDINAL; VAR b: Block) : BOOLEAN;
    (* retrieve the data in a block, no lock, copied from the blocklist *)
    retrieveData(blockno: BlockNo; block: REF ARRAY OF CHAR; VAR size: CARDINAL);
    (* unlock a previously retrieved block *)
    unlockBlock(blockno: BlockNo);
    (* amount of primary memory consumed by this blocklist *)
    size() : INTEGER;
    (* max amount of primary memory to be consumed before flush to secondary *)
    setMaxSize(newsize: INTEGER);
    (* block choose strategy for flush to secondary storage *)
    setPolicy(policy: Policy);
    (* register a swap extent to which we have readwrite access for swapping *)
    registerSwap(swap: Extent.T; devName: TEXT);
    (* page out any blocks that are over the limit *)
    pageout();
END;

TYPE T <: Public;

(* maxsize of zero means there is no limit *)
PROCEDURE New(sizehint: INTEGER; maxsize: INTEGER := 0) : T;

END BlockList.

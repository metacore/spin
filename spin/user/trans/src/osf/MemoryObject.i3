(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 06-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
INTERFACE MemoryObject;
IMPORT PagerObject;
IMPORT TransCache;
IMPORT Word;

TYPE
  T <: TPublic;
  TPublic = OBJECT
    base: Word.T;
    pager: PagerObject.T;
    enabled: REF ARRAY OF BOOLEAN;
  METHODS
    init(size: INTEGER;
	 pager: PagerObject.T;
	 cache: TransCache.T): T;
    map(addr: Word.T);
    unmap(addr: Word.T);
    virtualSize(): CARDINAL;
  END;

END MemoryObject.

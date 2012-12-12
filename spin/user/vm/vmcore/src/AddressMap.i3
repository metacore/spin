(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 07-May-96  Stefan Savage (savage) at the University of Washington
 *	Created.
 *)

INTERFACE AddressMap;

IMPORT AddressMapEntry, SortedAddressMapTbl;
IMPORT VMError;

TYPE
  PageNumber = CARDINAL;
  PageCount = CARDINAL;

TYPE
  T <: Public;
  Public = OBJECT
  METHODS
    init(first: PageNumber; last: PageNumber): T;
    insert(from, end: PageNumber): BOOLEAN;
    findSpace(VAR pos: PageNumber; size: PageNumber): BOOLEAN;
    delete(from, end: PageNumber): BOOLEAN;
    lookup(pageNum: PageNumber) : AddressMapEntry.T RAISES {VMError.E};
    operate(pageNum: PageNumber; numPages: PageCount; p: OperationProc;
	    arg: REFANY) RAISES {VMError.E};
    print(): TEXT;
    (* XXX these methods don't need to be methods. *)
  END;
  OperationProc = PROCEDURE (self: T; entry: AddressMapEntry.T;
			     arg: REFANY): VMError.Code;

    
  
END AddressMap.






(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 09-Oct-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
MODULE VMDebug;
IMPORT RefRefTbl;
IMPORT IO;
IMPORT Fmt;
IMPORT Word;

IMPORT VMTypes;
IMPORT VMError;
IMPORT PhysAddr;
IMPORT MemoryObject;
IMPORT MemoryObjectRep;
IMPORT CacheObject;
IMPORT Debugger;

FROM CPU IMPORT PAGESIZE;

PROCEDURE CalculateChecksum (p: PhysAddr.T): INTEGER =
  VAR sum: INTEGER := 0;
  PROCEDURE Callback (VAR buf: PhysAddr.Content) =
    BEGIN
      WITH ibuf = VIEW(buf, ARRAY [1..PAGESIZE DIV BYTESIZE(Word.T)]
		       OF Word.T) DO
	FOR i := FIRST(ibuf) TO LAST(ibuf) DO 
	  sum := Word.Plus(sum, ibuf[i]);
	END;
      END;
    END Callback;
  BEGIN
    TRY
      PhysAddr.Access(p, Callback);
    EXCEPT
    | VMError.E =>
      Debugger.Enter();
    END;
    RETURN sum;
  END CalculateChecksum;
    
PROCEDURE PrintStat () =
  VAR
    r, r2: REFANY;
    mObj: MemoryObject.T;
    itr: RefRefTbl.Iterator;
  BEGIN
    LOCK mu DO
      IO.Put("vm detailed stats:\n");
      itr := memObjTbl.iterate();
      WHILE itr.next(r, r2) DO
	<*ASSERT r = r2*>
	mObj := r;
	PrintMemObj(mObj);
      END;
    END;
  END PrintStat;

PROCEDURE PrintMemObj (mObj: MemoryObject.T) =
  VAR
    itr: CacheObject.Iterator;
    off: VMTypes.PageNumber;
    frame: PhysAddr.T;
    nPages: CARDINAL := 0;
    state: PhysAddr.State;
  BEGIN
    IO.Put("MemObj " & mObj.print());
    itr := mObj.cache.iterate();
    WHILE itr.next(off, frame) DO
      TRY
	state := PhysAddr.GetState(frame);
	IO.Put(" " & Fmt.Int(off, 16) & ":(" & Fmt.Int(ORD(state)) & ")");
      EXCEPT
      ELSE
	IO.Put(" " & Fmt.Int(off, 16) & ":(stale)");
      END;
      INC(nPages);
    END;
    IO.Put("\n  total " & Fmt.Int(nPages) & ".\n");
  END PrintMemObj;
  
BEGIN
  IF DebugStat THEN
    mu := NEW(MUTEX);
    memObjTbl := NEW(RefRefTbl.Default).init();
    spaceTbl := NEW(RefRefTbl.Default).init();
  END;
END VMDebug.

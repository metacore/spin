(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 17-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
MODULE OpenFile;
IMPORT RefRefTbl;
IMPORT MemoryObject;

VAR
  mu := NEW(MUTEX);
  memObjTTbl := NEW(RefRefTbl.Default).init();

PROCEDURE Register (t: T): BOOLEAN =
  VAR
    arry, arry2: REF ARRAY OF T;
    r: REFANY;
    status: BOOLEAN;
  BEGIN
    LOCK mu DO 
     IF memObjTTbl.get(t.memObj, r) THEN
       arry := r;
       FOR i := 0 TO LAST(arry^) DO
	 <*ASSERT arry[i] # t*>
	 IF arry[i] = NIL THEN
	   arry[i] := t;
	   RETURN FALSE;
	 END;
       END;
       
       (* Realloc "arry" into "arry2". *)
       arry2 := NEW(REF ARRAY OF T, 2*NUMBER(arry^));
       SUBARRAY(arry2^, 0, NUMBER(arry^)) := arry^;
       arry := arry2;
       arry[LAST(arry^)] := t;
       EVAL memObjTTbl.put(t.memObj, arry);
       RETURN FALSE;
     ELSE
       arry := NEW(REF ARRAY OF T, 4);
       arry[0] := t;
       status := memObjTTbl.put(t.memObj, arry);
       RETURN TRUE;
     END;
    END; (* lock *)
  END Register;
  
PROCEDURE Find (memObj: MemoryObject.T): REF ARRAY OF T =
  VAR
    r: REFANY;
    status: BOOLEAN;
  BEGIN
    LOCK mu DO 
      status := memObjTTbl.get(memObj, r);
      <*ASSERT status*>
    END;
    RETURN r;
  END Find;
  
PROCEDURE Delete (memObj: MemoryObject.T) =
  VAR
    r: REFANY;
    status: BOOLEAN;
  BEGIN
    LOCK mu DO 
      status := memObjTTbl.delete(memObj, r);
      <*ASSERT status*>
    END;
  END Delete;

BEGIN
END OpenFile.

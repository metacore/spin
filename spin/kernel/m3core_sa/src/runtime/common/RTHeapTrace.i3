INTERFACE RTHeapTrace;
IMPORT Word, RT0;

PROCEDURE ObjectAllocated(ref: REFANY; pc: ADDRESS);
PROCEDURE ObjectDeallocated(ref: Word.T);
PROCEDURE ObjectPromoted(ref: REFANY; stage: INTEGER; 
                         ptr: ADDRESS; loc: ADDRESS; src: REFANY);
PROCEDURE ObjectMoved(before: Word.T; after: REFANY; state: INTEGER);
PROCEDURE GCStarted();
PROCEDURE GCDone();

PROCEDURE TraceTypeOn(tc: RT0.Typecode);
PROCEDURE TraceTypeOff();
PROCEDURE TraceAllOn();
PROCEDURE TraceAllOff();

PROCEDURE Dump();
PROCEDURE GetAllocationPC(r: REFANY): ADDRESS;
PROCEDURE PutRefDesc(r: REFANY);
PROCEDURE Init ();
PROCEDURE Reset ();

END RTHeapTrace.

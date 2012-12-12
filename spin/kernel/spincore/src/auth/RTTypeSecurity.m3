(*
 * HISTORY
 * 18-Nov-97  Tian Fung Lim (tian) at the University of Washington
 *	Aligned record size to header size. Necessary for the copying
 *	collector, which assumes objects are multiples of header sizes.
 *
 * 03-Nov-97  Robert Grimm (rgrimm) at the University of Washington
 *      Fixed various small bugs to fit RTTypeSecurity with the
 *      security manager, and added synchronization to IsSecure and
 *      SetTypeSecurity.
 *
 * 14-Oct-97  Tian Fung Lim (tian) at the University of Washington
 *	Created.
 *
 *)

(* This module implements the runtime support for type-based security.
   It needs to be optimized.
 *)

UNSAFE MODULE RTTypeSecurity;
IMPORT RT0,RT0u;
IMPORT RTType, RTHeapRep, RTAllocator;
IMPORT RTIO;
IMPORT RTMisc;
IMPORT RTOS;

IMPORT SecurityManager;

CONST
  verbose = TRUE;

TYPE
  Header = RT0.RefHeader;
  RefHeader = UNTRACED REF Header;

(* Lookup typecode in type security vector to see if security is enabled
   for that type *)
PROCEDURE IsSecure(tc : Typecode) : BOOLEAN =
  VAR
    secure : BOOLEAN;
  BEGIN
    RTOS.LockHeap();
    IF NOT RTType.IsValid(tc) THEN
      RTMisc.FatalErrorI ("IsSecure: improper typecode: ", tc);
    END;
    secure := typeSecurityVector[tc];
    RTOS.UnlockHeap();
    RETURN secure;
  END IsSecure;

(* enable or disable type security for tc *)
PROCEDURE SetTypeSecurity(tc : Typecode; sec : BOOLEAN) =
  BEGIN
    (* if this procedure needs to be fast, then take out the typecode
       check.  A checked runtime error will occur on the array access
       anyways. *)
    RTOS.LockHeap();
    IF NOT RTType.IsValid(tc) THEN
      RTMisc.FatalErrorI ("SetTypeSecurity: improper typecode: ", tc);      
    END;
    IF typeSecurityVector[tc] # sec THEN
      RTAllocator.FlushInitCache(tc);
      typeSecurityVector[tc] := sec;
    END;
    RTOS.UnlockHeap();
  END SetTypeSecurity;

(* May need a more realistic failure mode.  Currently returns NIL 
   if object is not secure.
*)
PROCEDURE Get(r : REFANY) : SR =
  VAR
    h : RefHeader;
    s : CARDINAL;
  BEGIN
    (* get header *)
    h := LOOPHOLE(LOOPHOLE(r,ADDRESS)-BYTESIZE(Header), RefHeader);

    (* XXX FIXME - using Z as header security bit! *)
    IF NOT h.Z THEN
      RETURN NIL; 
    END;

    s := h.size - RecordSize;
    RETURN LOOPHOLE(LOOPHOLE(r,INTEGER)+s, SR);
  END Get;


(* assumes the runtime lock is held *)
PROCEDURE Reinitialize(ntypes : INTEGER) =
  BEGIN
    IF typeSecurityVector = NIL THEN
      RETURN; (* not yet initialized *)
    END;
    VAR
      newVector := NEW(UNTRACED REF ARRAY OF BOOLEAN, ntypes);
    BEGIN
      FOR i := 0 TO LAST(typeSecurityVector^) DO
        newVector[i] := typeSecurityVector[i];
      END;
      DISPOSE(typeSecurityVector);
      typeSecurityVector := newVector;
    END;
  END Reinitialize;

(* Called during allocation to initialize the new record, based on
   current security context.  Input is a reference to the cookie
   in the object.
 *)
PROCEDURE InitRecord(sr : SR)=
  BEGIN
    (*
       XXX: This is a performance bug,
            since we are making yet another procedure call.
     *)
    sr.sid := SecurityManager.GetCurrentObjectSid();
  END InitRecord;

(* set up type security vector *)
PROCEDURE Init()=
  BEGIN
    IF NOT RTHeapRep.TypeSecurityOn THEN
      RETURN;
    END;
    IF typeSecurityVector = NIL THEN
      typeSecurityVector := NEW(UNTRACED REF ARRAY OF BOOLEAN, RT0u.nTypes);
    END;

    (* need to keep record size a multiple of header size *)
    RecordSize := RTMisc.Upper(BYTESIZE(SC), BYTESIZE(RT0.RefHeader));

    IF verbose THEN
      RTIO.PutText("Type security initialized.\n");
    END;
  END Init;


BEGIN
END RTTypeSecurity.



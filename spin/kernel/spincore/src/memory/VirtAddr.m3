(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 *
 * 1-Dec-95 Przemek Pardyak (pardy) at the University of Washington
 *	Marked unused definition to disable warnings. 
 *
 * 02-Nov-94  Stefan Savage (savage) at the University of Washington
 *	Created
 *)

MODULE VirtAddr;

IMPORT VirtAddrRep, Fmt, MachineMem, Word;

PROCEDURE Allocate(begin: Address; end: Address;
                   <* UNUSED *> attrib: Attrib): T RAISES {Failure} = 
  VAR
    newV: T;
  BEGIN
    begin := MachineMem.TruncToPage(begin);
    end := MachineMem.RoundToPage(end);
    IF Word.GT(begin, end) THEN
      RAISE Failure;
    END;
    newV := NEW(T);
    newV.beginAddress := begin;
    newV.endAddress := end;
    RETURN newV;
  END Allocate;

PROCEDURE Deallocate(v: T; begin: Address; end: Address): T RAISES {Failure} =
  BEGIN
    begin := MachineMem.TruncToPage(begin);
    end := MachineMem.RoundToPage(end);
    IF NOT VerifyRange(v,begin,end) THEN
      RAISE Failure;
    END;
    IF begin = v.beginAddress THEN
      v.beginAddress := end;
    END;
    IF end = v.endAddress THEN
      v.beginAddress := begin;
    END;
    RETURN v;
  END Deallocate;

PROCEDURE VerifyRange(v: T; begin: Address; end: Address): BOOLEAN =
  BEGIN
    IF (Word.GE(begin, v.beginAddress) AND Word.LE(end,v.endAddress)) THEN
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END VerifyRange;

PROCEDURE Print(v: T): TEXT =
  BEGIN
    IF v # NIL THEN
      RETURN "( "&Fmt.Unsigned(v.beginAddress) &", "&
             Fmt.Unsigned(v.endAddress) &")"
    ELSE
      RETURN "(NIL)";
    END;
  END Print;
BEGIN
END VirtAddr.







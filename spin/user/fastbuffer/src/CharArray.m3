(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 * 06-Aug-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed the indexing to support allocations greater than
 *	Sizes[LAST(Index)] bytes.  Allocation & Free of buffers larger
 *	than Sizes[LAST(Index)] are not managed by the CharArray pool.
 *
 * 23-Jun-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created
 *)

MODULE CharArray;
IMPORT IO, Fmt;
IMPORT Debugger;
IMPORT LightMutex;
IMPORT FastBufferInterface, Auth;
TYPE Index = [0..3];

CONST
  Debug = FALSE;
  MaxFreeBuffer = 1024;
  Sizes = ARRAY [FIRST(Index) .. LAST(Index)] OF INTEGER{16_0100, 
                                                         16_0400, 
                                                         16_2000, 
                                                         16_8000};
VAR
  mu := LightMutex.Create();
  freeIndexes: ARRAY [ 0 .. MaxFreeBuffer-1] OF [ 0 .. MaxFreeBuffer-1];
  (* freeIndexes is used in FIFO manner.
   freeIndexes[0 .. freeTop] are the unused slots in "pool".
   *)
  freeTop: CARDINAL;
  
  buckets: ARRAY [ FIRST(Index) .. LAST(Index)] OF [-1 .. MaxFreeBuffer-1];
  (* buckets[i] holds the first free buffer in the "pool" that is of
   size Sizes[i]. Subsequent buffers of that size is linked using the
   first word of the buffer.
   *)
  pool: ARRAY [ 0 .. MaxFreeBuffer-1] OF REF ARRAY OF CHAR;
  (* each buf in "pool" is linked to one of "buckets" or "freeIndexes". *)

PROCEDURE SizeToIndex (size: CARDINAL): [FIRST(Index) .. LAST(Index)+1] =
  BEGIN
    (* XXX the compiler ought to inline the loop, but since it's not the case,
       we do manual inlining. *)
    IF size <= Sizes[0] THEN RETURN 0;
    ELSIF size <= Sizes[1] THEN RETURN 1;
    ELSIF size <= Sizes[2] THEN RETURN 2;
    ELSIF size <= Sizes[3] THEN RETURN 3;
    ELSE RETURN 4;
    END;
  END SizeToIndex;

PROCEDURE Sane (): BOOLEAN =
  VAR
    nFree := 0;
    p: [-1 .. MaxFreeBuffer-1];
    buf: REF ARRAY OF CHAR;
  BEGIN
    FOR i := FIRST(Index) TO LAST(Index) DO
      p := buckets[i];
      WHILE p # -1 DO
	buf := pool[p];
	INC(nFree);
	<*ASSERT buf # NIL*>
	<*ASSERT NUMBER(buf^) = Sizes[i]*>
	p := VIEW(buf^, INTEGER);
      END;
    END;
    IF nFree + freeTop # MaxFreeBuffer THEN
      IO.Put("chararray internal corruption: " & Fmt.Int(nFree) & ","
	     & Fmt.Int(freeTop) & "," & Fmt.Int(MaxFreeBuffer) & ".\n");
      <*ASSERT FALSE*>
    END;
    RETURN TRUE;
  END Sane;
  
PROCEDURE Allocate (size: CARDINAL): REF ARRAY OF CHAR =
  VAR
    i := SizeToIndex(size);
    buf: REF ARRAY OF CHAR;
  BEGIN
    IF Debug AND size >= 16_80000 THEN
      IO.Put("chararray.allocate: size " & Fmt.Int(size)
	     & " ridiculously big.\n");
      Debugger.Enter();
    END;
    
    LightMutex.Lock(mu);
    
    <*ASSERT Sane()*>
    IF i = LAST(Index) + 1 THEN 
      (* this is the regular NEW case *)
      buf := NEW(REF ARRAY OF CHAR, size);
    ELSIF buckets[i] < 0 THEN
      (* no free buf *)
      buf := NEW(REF ARRAY OF CHAR, Sizes[i]);
    ELSE
      buf := pool[buckets[i]];
      pool[buckets[i]] := NIL; (* just a paranoid *)
      <*ASSERT NUMBER(buf^) >= size*>
      
      (* the slot pool[buckets[i]] is now free. *)
      freeIndexes[freeTop] := buckets[i];
      INC(freeTop);
      
      (* Swing the link to the next free block *)
      buckets[i] := VIEW(buf^, INTEGER);
      <*ASSERT Sane()*>
    END;

    LightMutex.Unlock(mu);
    RETURN buf;
  END Allocate;

PROCEDURE Free (buf: REF ARRAY OF CHAR) =
  VAR
    i := SizeToIndex(NUMBER(buf^));
    slot: INTEGER;
  BEGIN
    IF i <= LAST(Index) AND NUMBER(buf^) = Sizes[i] THEN
      LightMutex.Lock(mu);
      <*ASSERT Sane()*>
      IF freeTop = 0 THEN
	(* XXX we have to initiate pool cleanup. *)
      ELSE
	(* Get a slot in the pool *)
	DEC(freeTop);
	slot := freeIndexes[freeTop];
	
	(* Chain the buffer into the bucket *)
	VIEW(buf^, INTEGER) := buckets[i];
	pool[slot] := buf;
	buckets[i] := slot;
      END;
      LightMutex.Unlock(mu);
    END;
  END Free;

BEGIN
  EVAL FastBufferInterface.Export(NEW(Auth.AuthAlways));
  
  (* Initially, all the slots are free. *)
  FOR i := 0 TO MaxFreeBuffer-1 DO
    freeIndexes[freeTop] := i;
    INC(freeTop);
  END;
  freeTop := MaxFreeBuffer;
  FOR i := FIRST(buckets) TO LAST(buckets) DO
    buckets[i] := -1;
  END;
  <*ASSERT Sane()*>
END CharArray.

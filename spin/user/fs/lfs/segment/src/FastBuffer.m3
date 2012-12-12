(*
 *
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 23-Nov-97  Tsutomu Owa (owa) at the University of Washington
 *	Added DEBUG code.
 *
 * 05-Mar-97  Tsutomu Owa (owa) at the University of Washington
 *      Borrowed from Gun's httpd/src. Changed interface name.
 *
 * 06-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	Added buckets for differently sized buffers.
 *
 * 02-Dec-95  Emin Gun Sirer (egs) at the University of Washington
 *	tcpclient client 128 95 2 14 1234 32000 100Created. Buffer manager and recycler.
 *)
MODULE FastBuffer;
IMPORT FastList, IO, Fmt;

CONST
  DEBUG = FALSE;
  MAXBucket = 16;
  BucketDivider = 1024;

TYPE Bucket = [0..MAXBucket];

VAR 
  freelist: ARRAY Bucket OF REF FastList.T;

<* INLINE *>
EPHEMERAL
PROCEDURE FindBucket(size: INTEGER := DefaultSize) : Bucket =
  VAR
    bucket: INTEGER;
  BEGIN
    bucket := size DIV BucketDivider;
    IF bucket > MAXBucket THEN
      bucket := MAXBucket;
    END;
    RETURN bucket;
  END FindBucket;

PROCEDURE Allocate(size: CARDINAL := DefaultSize) : T =
  VAR
    b: Bucket;
    buf: T;
  BEGIN
    b := FindBucket(size);
    buf := FastList.Dequeue(freelist[b]);
    IF DEBUG THEN
      IF (size MOD 512) # 0 THEN IO.Put("!!! weird size requested\n"); END;
      IO.Put("Allocate. bkt ");
      IO.Put(Fmt.Int(b));
      IO.Put(" size ");
      IO.Put(Fmt.Int(size));
    END;
    IF buf = NIL THEN
      buf := NEW(T);
      buf.data := NEW(REF ARRAY OF CHAR, size);
    ELSIF NUMBER(buf.data^) < size THEN
      FastList.Enqueue(buf, freelist[b]);
      buf := NEW(T);
      buf.data := NEW(REF ARRAY OF CHAR, size);
    END;
    RETURN buf;
  END Allocate;

(* EPHEMERAL *)
PROCEDURE Deallocate(buffer: T) =
  VAR
    b: Bucket;
    size: CARDINAL;	(* XXX *)
  BEGIN
    b := FindBucket(NUMBER(buffer.data^));
    IF DEBUG THEN
      size := NUMBER(buffer.data^);
      IF (size MOD 512) # 0 THEN IO.Put("!!! weird size requested\n"); END;
      IO.Put(". Deallocate. bkt ");
      IO.Put(Fmt.Int(b));
      IO.Put(" size ");
      IO.Put(Fmt.Int(size));
      IO.Put("\n");
    END;
    FastList.Enqueue(buffer, freelist[b]);
  END Deallocate;

BEGIN
  FOR i := FIRST(freelist) TO LAST(freelist) DO
    freelist[i] := NEW(REF FastList.T);
  END;
  IF DEBUG THEN IO.Put("FastBuffer free lists are initialized.\n"); END;
END FastBuffer.

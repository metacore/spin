(*
 *
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 20-May-96  David Dion (ddion) at the University of Washington
 *	Borrowed from httpd code for copyin/copyout buffer management
 *
 * 06-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	Added buckets for differently sized buffers.
 *
 * 02-Dec-95  Emin Gun Sirer (egs) at the University of Washington
 *	tcpclient client 128 95 2 14 1234 32000 100Created. Buffer manager and recycler.
 *)
MODULE Buffer;
IMPORT FastList, HandlerUtils;

CONST
  MAXBucket = 16;
  BucketDivider = 1024;

TYPE Bucket = [0..MAXBucket];

VAR 
  freelist: ARRAY Bucket OF REF FastList.T;

<* INLINE *>
(* EPHEMERAL *)
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
  BEGIN
    b := FindBucket(NUMBER(buffer.data^));
    FastList.Enqueue(buffer, freelist[b]);
  END Deallocate;

(*
 * Function that is called by the device when a buffer being sent
 * can be reused by the application.
 *)
(* EPHEMERAL *)
PROCEDURE FreeBuffer(<*UNUSED*>buf: REF ARRAY OF CHAR;
                     <*UNUSED*>len: CARDINAL;
                     arg: REFANY) =
  VAR
    buffer: T;
  BEGIN
    buffer := NARROW(arg, T);
    Deallocate(buffer);
  END FreeBuffer;

BEGIN
  FOR i := FIRST(freelist) TO LAST(freelist) DO
    freelist[i] := NEW(REF FastList.T);
  END;
  HandlerUtils.Print("Buffer free lists are initialized.\n");
END Buffer.

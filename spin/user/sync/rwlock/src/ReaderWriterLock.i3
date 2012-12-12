(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 10-Dec-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Reaer/writer locks. Multiple readers, single writer.
 *
 *)
INTERFACE ReaderWriterLock;

TYPE T <: REFANY;

PROCEDURE Allocate() : T;

PROCEDURE ReaderLock(rwl: T);

PROCEDURE ReaderTryLock(rwl: T) : BOOLEAN;

PROCEDURE ReaderUnlock(rwl: T);

PROCEDURE WriterLock(rwl: T);

PROCEDURE WriterTryLock(rwl: T) : BOOLEAN;

PROCEDURE WriterUnlock(rwl: T);

END ReaderWriterLock.

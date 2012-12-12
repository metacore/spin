(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 31-Oct-95  Charlie Garrett (garrett) at the University of Washington
 *      Created.
 *)

UNSAFE MODULE Cerrno;
IMPORT CerrnoExtern;

FROM Ctypes IMPORT int;

PROCEDURE GetErrno(): int =
  BEGIN
    RETURN CerrnoExtern.errno;
  END GetErrno;

PROCEDURE SetErrno(i: int) =
  BEGIN
    CerrnoExtern.errno := i;
  END SetErrno;

BEGIN
END Cerrno.

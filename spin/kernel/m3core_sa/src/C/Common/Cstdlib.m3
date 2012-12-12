(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 30-Oct-95  Charlie Garrett (garrett) at the University of Washington
 *      Created.
 *)

UNSAFE MODULE Cstdlib;
IMPORT CstdlibExtern;

FROM Ctypes IMPORT void_star;
FROM Cstddef IMPORT size_t;

PROCEDURE malloc (size: size_t): void_star =
  BEGIN
    RETURN CstdlibExtern.spin_malloc(size);
  END malloc;

PROCEDURE free (ptr: void_star) =
  BEGIN
    CstdlibExtern.spin_free(ptr);
  END free;

BEGIN
END Cstdlib.

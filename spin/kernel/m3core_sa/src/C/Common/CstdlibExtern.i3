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

UNSAFE INTERFACE CstdlibExtern;

FROM Ctypes IMPORT void_star;
FROM Cstddef IMPORT size_t;

<*EXTERNAL*> PROCEDURE spin_malloc (size: size_t): void_star;

<*EXTERNAL*> PROCEDURE spin_free (ptr: void_star);

END CstdlibExtern.

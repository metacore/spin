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

UNSAFE MODULE Csetjmp;
IMPORT CsetjmpExtern;
FROM Ctypes IMPORT int;

PROCEDURE setjmp (VAR env: jmp_buf): int =
  BEGIN
    RETURN CsetjmpExtern.setjmp(env);
  END setjmp;

PROCEDURE longjmp (VAR env: jmp_buf; val: int) =
  BEGIN
    CsetjmpExtern.longjmp(env, val);
  END longjmp;


PROCEDURE usetjmp (VAR env: jmp_buf): int =
  BEGIN
    RETURN CsetjmpExtern.usetjmp(env);
  END usetjmp;

PROCEDURE ulongjmp (VAR env: jmp_buf; val: int) =
  BEGIN
    CsetjmpExtern.ulongjmp(env, val);
  END ulongjmp;

BEGIN
END Csetjmp.

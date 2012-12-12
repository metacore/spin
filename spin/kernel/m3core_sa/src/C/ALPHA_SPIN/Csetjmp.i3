(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Wed Apr  7 15:07:38 PDT 1993 by muller         *)

INTERFACE Csetjmp;		(* for ALPHA_OSF *)

FROM Ctypes IMPORT int, long;

TYPE jmp_buf = ARRAY [0..83] OF long;

PROCEDURE setjmp (VAR env: jmp_buf): int;
PROCEDURE longjmp (VAR env: jmp_buf; val: int);

PROCEDURE usetjmp (VAR env: jmp_buf): int;
PROCEDURE ulongjmp (VAR env: jmp_buf; val: int);

END Csetjmp.

(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Fri Apr  9 18:51:54 PDT 1993 by muller        *)

INTERFACE Cstdio;

FROM Ctypes IMPORT int, short, void_star, unsigned_char_star, unsigned_int;

CONST 
  NIOBRW = 8;
  IOEOF  = 8_20;

TYPE
  FILE = RECORD 
            cnt: int;
            ptr: unsigned_char_star;
            base: unsigned_char_star;
            bufsiz: int;
            flag: short;
            file: short;
            unused: ARRAY [0..1] OF int;
            lock: void_star;
            bufendp: unsigned_char_star; END;
  FILE_star = UNTRACED REF FILE;

PROCEDURE GetIobElem(i: [0..NIOBRW-1]): FILE;
PROCEDURE SetIobElem(i: [0..NIOBRW-1]; f: FILE);

PROCEDURE flsbuf (c: unsigned_int; f: FILE_star): int;
PROCEDURE filbuf (f: FILE_star): int;

PROCEDURE feof (f: FILE_star): int;
PROCEDURE getc (f: FILE_star): int;
PROCEDURE ungetc (c: int; f: FILE_star): int;
PROCEDURE putc (c: int; f: FILE_star): int;
PROCEDURE fflush (f: FILE_star): int;

END Cstdio.

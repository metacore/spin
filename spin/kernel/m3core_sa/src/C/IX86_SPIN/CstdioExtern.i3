(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 13-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Created from Alpha version.
 *
 *)

UNSAFE INTERFACE CstdioExtern;

FROM Cstdio IMPORT NIOBRW, FILE, FILE_star;
FROM Ctypes IMPORT int, unsigned_int;

<*EXTERNAL "_iob"*> VAR iob: ARRAY [0..NIOBRW-1] OF FILE;
<*EXTERNAL "_flsbuf"*> PROCEDURE flsbuf (c: unsigned_int; f: FILE_star): int;
<*EXTERNAL "_filbuf"*> PROCEDURE filbuf (f: FILE_star): int;

<*EXTERNAL feof*>      PROCEDURE feof (f: FILE_star): int;
<*EXTERNAL getc*>      PROCEDURE getc (f: FILE_star): int;
<*EXTERNAL ungetc*>    PROCEDURE ungetc (c: int; f: FILE_star): int;
<*EXTERNAL putc*>      PROCEDURE putc (c: int; f: FILE_star): int;
<*EXTERNAL fflush*>    PROCEDURE fflush (f: FILE_star): int;

END CstdioExtern.

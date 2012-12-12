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

UNSAFE MODULE Cstdio;
IMPORT CstdioExtern;

FROM Ctypes IMPORT int, unsigned_int;

PROCEDURE GetIobElem(i: [0..NIOBRW-1]): FILE =
  BEGIN
    RETURN CstdioExtern.iob[i];
  END GetIobElem;

PROCEDURE SetIobElem(i: [0..NIOBRW-1]; f: FILE) =
  BEGIN
    CstdioExtern.iob[i] := f;
  END SetIobElem;

PROCEDURE flsbuf (c: unsigned_int; f: FILE_star): int =
  BEGIN
    RETURN CstdioExtern.flsbuf(c, f);
  END flsbuf;

PROCEDURE filbuf (f: FILE_star): int =
  BEGIN
    RETURN CstdioExtern.filbuf(f);
  END filbuf; 

 PROCEDURE feof (f: FILE_star): int =
  BEGIN
    RETURN CstdioExtern.feof(f);
  END feof;
 
 PROCEDURE getc (f: FILE_star): int =
  BEGIN
    RETURN CstdioExtern.getc(f);
  END getc;

 PROCEDURE ungetc (c: int; f: FILE_star): int =
  BEGIN
    RETURN CstdioExtern.ungetc(c, f);
  END ungetc;

 PROCEDURE putc (c: int; f: FILE_star): int =
  BEGIN
    RETURN CstdioExtern.putc(c, f);
  END putc;

 PROCEDURE fflush (f: FILE_star): int =
  BEGIN
    RETURN CstdioExtern.fflush(f);
  END fflush;

BEGIN
END Cstdio.

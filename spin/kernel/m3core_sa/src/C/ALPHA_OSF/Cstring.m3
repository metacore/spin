(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 30-Oct-95  Charlie Garrett (garrett) at the University of Washington
 *	Created. 
 *)

UNSAFE MODULE Cstring;
IMPORT CstringExtern;

FROM Ctypes IMPORT char_star, const_char_star, const_void_star,
                   int, void_star;

PROCEDURE memcpy (s1: void_star; s2: const_void_star; n: size_t): void_star =
  BEGIN
    RETURN CstringExtern.memcpy (s1, s2, n);
  END memcpy;

PROCEDURE memset (s: void_star; c: int; n: size_t): void_star =
  BEGIN
    RETURN CstringExtern.memset(s, c, n);
  END memset;

PROCEDURE memmove (s1: void_star; s2: const_void_star; n: size_t): void_star =
  BEGIN
    RETURN CstringExtern.memmove(s1, s2, n);
  END memmove;

PROCEDURE strcpy (s1: char_star; s2: const_char_star): char_star =
  BEGIN
    RETURN CstringExtern.strcpy(s1, s2);
  END strcpy;

PROCEDURE strncpy (s1: char_star; s2: const_char_star; n: size_t): char_star =
  BEGIN
    RETURN CstringExtern.strncpy(s1, s2, n);
  END strncpy;

PROCEDURE strcat (s1: char_star; s2: const_char_star): char_star =
  BEGIN
    RETURN CstringExtern.strcat(s1, s2);
  END strcat;

PROCEDURE strcmp (s1: const_char_star; s2: const_char_star): int =
  BEGIN
    RETURN CstringExtern.strcmp(s1, s2);
  END strcmp;

PROCEDURE strncmp (s1: const_char_star; s2: const_char_star; n: size_t): int =
  BEGIN
    RETURN CstringExtern.strncmp(s1, s2, n);
  END strncmp;

PROCEDURE strlen (s: const_char_star): size_t =
  BEGIN
    RETURN CstringExtern.strlen(s);
  END strlen;

BEGIN
END Cstring.


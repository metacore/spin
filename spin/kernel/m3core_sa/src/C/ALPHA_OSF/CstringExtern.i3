(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 30-Oct-95  Charlie Garrett (garrett) at the University of Washington
 *      Created for wrapping C functions called by safe interface
 *      Cstring.i3.
 *)

UNSAFE INTERFACE CstringExtern;

FROM Ctypes IMPORT char_star, const_char_star, const_void_star,
                   int, void_star;
FROM Cstring IMPORT size_t;

<*EXTERNAL*>
PROCEDURE memcpy (s1: void_star; s2: const_void_star; n: size_t): void_star;

<*EXTERNAL*> 
PROCEDURE memset (s: void_star; c: int; n: size_t): void_star;

<*EXTERNAL*>
PROCEDURE memmove (s1: void_star; s2: const_void_star; n: size_t): void_star;

<*EXTERNAL*>
PROCEDURE strcpy (s1: char_star; s2: const_char_star): char_star;

<*EXTERNAL*>
PROCEDURE strncpy (s1: char_star; s2: const_char_star; n: size_t): char_star;

<*EXTERNAL*>
PROCEDURE strcat (s1: char_star; s2: const_char_star): char_star;

<*EXTERNAL*>
PROCEDURE strcmp (s1: const_char_star; s2: const_char_star): int;

<*EXTERNAL*>
PROCEDURE strncmp (s1: const_char_star; s2: const_char_star; n: size_t): int;

<*EXTERNAL*>
PROCEDURE strlen (s: const_char_star): size_t;

END CstringExtern.


(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Cstring.i3                                            *)
(* Last modified on Tue Apr 20 20:16:18 PDT 1993 by muller         *)
(*      modified on Sat Jan 20 22:31:44 1990 by jerome         *)

(* HISTORY
 * 13-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Created from Alpha version.
 *
 *)

INTERFACE Cstring;

FROM Ctypes IMPORT char_star, const_char_star, const_void_star,
                   int, void_star, unsigned_long;

TYPE
  size_t            =  unsigned_long;


PROCEDURE memcpy (s1: void_star; s2: const_void_star; n: size_t): void_star;
PROCEDURE memset (s: void_star; c: int; n: size_t): void_star;
PROCEDURE memmove (s1: void_star; s2: const_void_star; n: size_t): void_star;
PROCEDURE strcpy (s1: char_star; s2: const_char_star): char_star;
PROCEDURE strncpy (s1: char_star; s2: const_char_star; n: size_t): char_star;
PROCEDURE strcat (s1: char_star; s2: const_char_star): char_star;
PROCEDURE strcmp (s1: const_char_star; s2: const_char_star): int;
PROCEDURE strncmp (s1: const_char_star; s2: const_char_star; n: size_t): int;
PROCEDURE strlen (s: const_char_star): size_t;

(*
 * PROCEDURE memchr (s: const_void_star; c: int; n: size_t): void_star;
 * 
 * PROCEDURE memcmp (s1: const_void_star; s2: const_void_star; n: size_t): int;
 * 
 * PROCEDURE strncat  (s1: char_star; s2: const_char_star; n: size_t): char_star;
 * 
 * PROCEDURE strchr (s: const_char_star; c: int): char_star;
 * 
 * PROCEDURE strrchr (s: const_char_star; c: int): char_star;
 * 
 * PROCEDURE strpbrk (s1: const_char_star; s2: const_char_star): char_star;
 * 
 * PROCEDURE strtok (s1: char_star; s2: const_char_star): char_star;
 * 
 * PROCEDURE strspn (s1: const_char_star; s2: const_char_star): int;
 * 
 * PROCEDURE strcspn (s1: const_char_star; s2: const_char_star): size_t;
 * 
 * PROCEDURE strcoll (s1: const_char_star; s2: const_char_star): int;
 * 
 * PROCEDURE strxrfm (s1: char_star; s2: const_char_star; n: size_t): size_t;
 * 
 * PROCEDURE strstr (s1: const_char_star; s2: const_char_star): char_star;
 * 
 * PROCEDURE strerror (errnum: int): char_star;
 *)
END Cstring.


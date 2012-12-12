(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
   Console.i3

   The console tty.
 *)
INTERFACE Console;

CONST Brand = "Console";

CONST	
  BASE_ERROR      = 1000;  (* start high to not overlap unix errno codes *)
  NO_CONSOLE      = BASE_ERROR+0;  (* "console" device not in name server *)
  NO_READSEEK     = BASE_ERROR+1;  (* cannot use non-zero read offset *)
  NO_SELECT       = BASE_ERROR+2;  (* cannot use non-zero read offset *)
  LAST_ERROR      = BASE_ERROR+2;  (* cannot use non-zero read offset *)

END Console.

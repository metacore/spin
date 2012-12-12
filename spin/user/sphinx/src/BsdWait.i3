(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 24-May-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

(* This module defines consts and procs for sys/wait.h *)
INTERFACE BsdWait;
IMPORT Ctypes;

CONST
  WNOHANG = 16_1;
  WUNTRACED = 16_2;
  WLOGINDEV = 16_0;
  WCOREFLAG = 8_200;
  WCONTINUED = 16_4;
  WNOWAIT = 16_8;
  WTRAPPED = 16_0;
  WEXITED = 16_0;
  WSIGINFO = 16_0;
  WAIT_MYPGRP = 0;

CONST
  WSTOPPED_= 8_177;
  WCONTINUED_ = 8_377;
  
PROCEDURE StatusCode(x : Ctypes.unsigned_int) : INTEGER;
  (* StatusCode == _WCONTINUED => proc is cont'ed.
     StatusCode == _WSTOPPED => proc is stopped'ed.
     StatusCode == 0 => proc exited.
     otherwise  => signal number*)
  
PROCEDURE ExitCode(x : Ctypes.unsigned_int) : INTEGER;

PROCEDURE Compose(stat, exit : INTEGER) : Ctypes.unsigned_int;
  
END BsdWait.

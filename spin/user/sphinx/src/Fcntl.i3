(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *  HISTORY
 * 11-Jun-96 oystr at the University of Washington
 *	Added O_NONBLOCK.
 *
 * 15-May-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *	
 *)

INTERFACE Fcntl;

CONST
  O_RDONLY = 8_0;
  O_WRONLY = 8_1;
  O_RDWR = 8_2;
  O_NONBLOCK = 8_4;
  O_APPEND = 8_10;
  O_CREAT = 8_1000;
  O_TRUNC = 8_2000;
  O_EXCL = 8_4000;
  FD_CLOEXEC = 1;
  SEEK_SET = 0;
  SEEK_CUR = 1;
  SEEK_END = 2;
  F_DUPFD = 0;
  F_GETFD = 1;
  F_SETFD = 2;
  F_GETFL = 3;
  F_SETFL = 4;
  F_GETOWN = 5;
  F_SETOWN = 6;
  F_GETLK = 7;
  F_SETLK = 8;
  F_SETLKW = 9;
  
END Fcntl.

(*
 * Copyright 1994-1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 31-May-97  David Becker at the University of Washington
 *      Added ttselect for new select support
 *
 * 23-Mar-97  Yasushi Saito (yasushi) at the University of Washington
 *	Cleaned up Error interface.
 *)
(*
   ConsoleExtern.i3

   Externs to console tty ops.
 *)
UNSAFE (* for externals *)
INTERFACE ConsoleExtern;
IMPORT BSDtty;

<*EXTERNAL*>PROCEDURE console_open(): INTEGER;
<*EXTERNAL*>PROCEDURE console_close(): INTEGER;
<*EXTERNAL*>PROCEDURE console_nread(VAR bytes: CARDINAL): INTEGER;
<*EXTERNAL*>PROCEDURE console_read(VAR data: ARRAY OF CHAR;
                VAR bytes: CARDINAL): INTEGER;
<*EXTERNAL*>PROCEDURE console_write(READONLY data: ARRAY OF CHAR;
                VAR bytes: CARDINAL): INTEGER;
<*EXTERNAL*>PROCEDURE console_ioctl(cmd: INTEGER; VAR data: ARRAY OF CHAR;
		flags: INTEGER): INTEGER;
<*EXTERNAL*>PROCEDURE console_spgrp(handler: BSDtty.SignalHandler): INTEGER;

END ConsoleExtern.

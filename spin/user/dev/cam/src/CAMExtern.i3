(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
   CAMExtern.m3

   Convenient types for talking to CAM in the  layer.

 *)
(*
 *
 * HISTORY
 * 22-Jan-96  Yasushi Saito (yasushi) at the University of Washington
 *	Removed readForFileIO
 * 28-Oct-95  Brian Bershad (bershad) at the University of Washington
 *	Made unsafe.
 * 07-Aug-95  Brian Bershad (bershad) at the University of Washington
 *	created.
 *
 *)
UNSAFE (* This interface is unsafe because it defines externs.*)
INTERFACE CAMExtern;

TYPE dev_t = INTEGER;

<* EXTERNAL *>PROCEDURE cam_attach();

<* EXTERNAL *>PROCEDURE cdisk_open(dev, zero, blk: INTEGER): INTEGER;
<* EXTERNAL *>PROCEDURE cdisk_close(dev, zero, blk: INTEGER): INTEGER;

<* EXTERNAL *>PROCEDURE cam_read(dev: dev_t; VAR data : ARRAY OF CHAR;
		offset: CARDINAL; VAR bytes: CARDINAL): INTEGER;

<* EXTERNAL *>PROCEDURE cam_write(dev: dev_t; READONLY data: ARRAY OF CHAR;
		offset: CARDINAL; VAR bytes: CARDINAL): INTEGER;

END CAMExtern.

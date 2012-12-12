(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 21-Feb-97  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
UNSAFE INTERFACE PFMExtern;
IMPORT Word;
(* Unsafe because it defines C interface. *)

  
<*EXTERNAL pfmopen*>
PROCEDURE Open(dev, mode: Word.T): Word.T;
  (* Open the perfmon device. "dev" is not used. *)
  
<*EXTERNAL pfmclose*>
PROCEDURE Close();

<*EXTERNAL pfmioctl*>
PROCEDURE Ioctl(dev, cmd, data, flag: Word.T): Word.T;

<*EXTERNAL pfmintr*>
PROCEDURE Interrupt(pc, ps, cntr, rpcc, sp: Word.T);

<*EXTERNAL pfmread*>
PROCEDURE Read(p: ADDRESS; n: Word.T): Word.T;
  
END PFMExtern.

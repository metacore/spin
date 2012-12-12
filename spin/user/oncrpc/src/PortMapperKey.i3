(*
 * Copyright 1996, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 23-Apr-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *)

INTERFACE PortMapperKey;
IMPORT Word;
IMPORT Ctypes;
CONST Brand = "PortMapperKey";
TYPE T = RECORD
        prog: Ctypes.unsigned_int;
        prot: Ctypes.unsigned_int;
END;

PROCEDURE Equal(a, b: T): BOOLEAN;
(* Return "a = b". *)

PROCEDURE Hash(a: T): Word.T;
(* Return "a". *)

PROCEDURE Compare(a, b: T): [-1..1];
(* Return "-1" if "a < b", "0" if "a = b", or "+1" if "a > b". *)

END PortMapperKey.

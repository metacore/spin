(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
 * HISTORY
 * 03-Nov-95  Yasushi Saito (yasushi) at the University of Washington
 *	created.
 *)


(*
 Defines machine dependent procedures for packing and unpacking types.
 These procedures are actually used as a methods of Type children.
 *)


INTERFACE Target;
IMPORT IWr;

PROCEDURE GetReturnReg() : TEXT;
PROCEDURE GetSyscallIDReg() : TEXT;

(* Given the bit size of a type, return the corresponding C type.
 For example if bitsize=8, then it will return "char".*)
PROCEDURE ToCTextOrdinal(bitsize : CARDINAL) : TEXT;

PROCEDURE ClearErrorReg(wr: IWr.T);
PROCEDURE SetErrorReg(code: TEXT; wr: IWr.T);
  
PROCEDURE Init();

VAR
  maxRegAddr : CARDINAL; (* CONST : Max # of bytes storeable in
			  registers when passing parameters. *)
  maxSavedRegAddr : CARDINAL; (* last register saved across syscalls *)
  
  regSize : INTEGER; (* CONST : bytesize of a register *)
END Target.

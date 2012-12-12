(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
 * HISTORY
 * 26-Nov-95  Yasushi Saito (yasushi) at the University of Washington
 *	created.
 *)
INTERFACE ParamAddr;
IMPORT IWr;

(* ParamAddr is a uniform notation for a parameter location.
 * Itself is a pure virtual class, and it has three children,
 * UserAddr, KernelAddr and RegAddr. UserAddr is used to read or write the 
 * value stored in the user space. RegAddr is the value stored in
 * the registers.
 *)
TYPE
  T = OBJECT
  METHODS
    (* Return the textual representation for the address this object
     *  denotes. XXX: semantics is different for reg and mem.
     *  reg returns the dereferenced value, while mem returns the
     *  address itself.
     *)
    adr(): TEXT;
    
    (* Increase the address by VAL. Returns a new addr.
     * This object is not modified. *)
    inc(val: INTEGER): T;
    
    (* Output the code to read data into VAR of the type TYPE. *)
    (* XXX The type of TYPE is actually a Type.T, but I can't declare
     *  it as Type.T because of it will cause circular reference.
     *  This is a quite a dirty hack. Someone teach me how to deal with
     *  this problem! -yasushi
     *)
    read(var: TEXT; type: REFANY; wr: IWr.T);

    (* Similar to read, but the bytesize is provided by text. *)
    readDyn(var: TEXT; size: TEXT := NIL; wr: IWr.T);
    
    write(var: TEXT; type: REFANY; wr: IWr.T);
    writeDyn(var: TEXT; size: TEXT := NIL; wr: IWr.T);
  END;

PROCEDURE User(virtAddr: TEXT): T;
PROCEDURE Kernel(kernAddr: TEXT): T;
PROCEDURE Reg(reg: INTEGER): T;

END ParamAddr.

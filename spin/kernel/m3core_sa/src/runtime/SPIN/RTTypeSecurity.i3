(*
 * HISTORY
 * 14-Oct-97  Tian Fung Lim (tian) at the University of Washington
 *	Created.  
 *
 *)

(* This module implements the runtime support for type-based security.
 * The implementation relies on SecurityContext, and is in
 * spincore/src/auth *)

INTERFACE RTTypeSecurity;
IMPORT RT0;

TYPE
  Typecode = RT0.Typecode;
  (* reference to security record *)
  SR = UNTRACED REF SC;
  (* security cell/cookie *)
  SC = RECORD
    sid : INTEGER;
  END;


(* For efficiency, expose this vector to the allocator without 
   an accessor function. For extra nastiness, consider EXTERNing 
   this. *)
VAR
  typeSecurityVector : UNTRACED REF ARRAY OF BOOLEAN;
  RecordSize(* CONST *) : INTEGER;


(* Lookup typecode in type security vector to see if security is enabled
   for that type *)
PROCEDURE IsSecure(tc : Typecode) : BOOLEAN;

(* enable or disable type security for tc *)
PROCEDURE SetTypeSecurity(tc : Typecode; sec : BOOLEAN);

(* given a pointer to an object, locate and return a pointer to the
   security record *)
PROCEDURE Get(r : REFANY) : SR;

(* Called during allocation to initialize the new record, based on
   current security context 
 *)
PROCEDURE InitRecord(sr : SR);

(* grow the type security vector at link time *)
PROCEDURE Reinitialize(ntypes : INTEGER);

(* set up type security vector *)
PROCEDURE Init();

END RTTypeSecurity.


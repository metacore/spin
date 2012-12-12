(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)


(* HISTORY 
 * 27-Jan-97  Wilson Hsieh (whsieh) at the University of Washington
 *	added ref header fields for reference counting collector
 *
 * 19-Nov-95  Przemek Pardyak (pardy) at the University of Washington
 *	Added procedure type information.  A type descriptor is a series of
 *	INTEGER-s representing: #args, #results (0 or 1), global UIDs of
 *	argument types and result type. Added argument passing mode
 *	descriptors. 
 *
 * 13-Nov-95  Przemek Pardyak (pardy) at the University of Washington
 *	Changed representation of type information to enable dynamic
 *	linking of subtypes.  Before, a type was identified by typecode
 *	(a number between 0 and the number of existing types assigned
 *	during startup of M3 program. The set of its subtypes was represented 
 *	by a range between typecode and lastSubTypeTC.  Now, type is identified
 *	by typecode which is 60 bits of a pointer to its typecell.  Its 
 *	index (what used to be typecode) is kept in typeidx, both assigned 
 *	when the type is first loaded. The set of its subtypes is a range 
 *	between subTypeCode and lastSubTypeCode, both reassigned during 
 *	each reinitialization of the runtime.
 *
 *	Also, the type Typecode was split into two types: Typecode taking
 *	60 bits and containing a pointer to a typecell for that type
 *	and Typeidx taking 20 bits and containing a short integer 
 *	identifying the type (exacyly like old Typecode used to).
 *
 * 27-Oct-95  Brian Bershad (bershad) at the University of Washington
 *	Removed externs added by early releases of SPIN.
 *
 *)

(* Last modified on Fri Jul 15 13:25:46 PDT 1994 by kalsow     *)
(*      modified on Fri May 28 14:34:04 PDT 1993 by muller     *)
(*      modified on Tue Sep  8 11:51:46 PDT 1992 by jdd        *)

(* RT0 is "the bottom of the world".  It contains
   types and constants that are shared by multiple modules of
   the runtime and/or the compiler and linker.

   If you're using this interface, you're a wizard!

   This interface and its implemenation MUST NOT import any
   other interface.
*)

INTERFACE RT0;

(* These types CANNOT be changed without a synchronized change to the
   output of the compiler and prelinker *)

(*------------------------------------ compiler generated data structures ---*)

TYPE
  Typecode    = [0 .. 16_FFFFF];                      (* can fit in 20 bits *)
  Fingerprint = ARRAY [0..1] OF [-16_7fffffff-1 .. 16_7fffffff]; (* 64 bits *)
  String      = UNTRACED REF CHAR;       (* a '\000' terminated string *)
  ModulePtr   = UNTRACED REF ModuleInfo;
  ProcPtr     = UNTRACED REF ProcInfo;

TYPE (* allocated at offset 0 of each compilation unit's global data *)
  ModuleInfo = RECORD
    file           : String;
    type_cells     : ADDRESS;  (* initially a ref to a Typecell *)
    type_cell_ptrs : ADDRESS;  (* initially a ref to a TypeLink *)
    full_rev       : ADDRESS;  (* initially a ref to a Revelation *)
    partial_rev    : ADDRESS;  (* initially a ref to a Revelation *)
    proc_info      : ProcPtr;
    proc_types     : UNTRACED REF INTEGER; (* procedure type information *)
    try_scopes     : ADDRESS;  (* RTExRep.Scope *)
    var_map        : ADDRESS;  (* RTTypeMap.T *)
    gc_map         : ADDRESS;  (* reduced RTTypeMap.T *)
    link           : PROCEDURE ();
    main           : PROCEDURE ();
  END;

TYPE (* one of these is generated for each top-level procedure *)
  ProcInfo = RECORD
    proc   : PROCANY;
    name   : String;
    export : ADDRESS;
    (* type   : ADDRESS; (* SPIN *)*)
  END;

TYPE
  TypeLink = RECORD
    next : ADDRESS;  (* init code patches this to be a TypeDefn *)
    type : INTEGER;  (* init code patches this to be a Typecode *)
  END;

TYPE
  RevPtr     = UNTRACED REF Revelation;
  Revelation = RECORD lhs_id, rhs_id: INTEGER; END;

(*------------------------------------------- linker generated type cells ---*)

TYPE
  MethodSuite = UNTRACED REF RECORD
    typecode : INTEGER;  (* Typecode *)
    methods  : (* ARRAY [0..n] OF *) ADDRESS;
  END;

TYPE
  TypeDefn = UNTRACED REF Typecell;
  Typecell = RECORD
    typecode         : INTEGER; (* typecode *)
    subTypeCode      : INTEGER; (* subtype code of this type *)
    lastSubTypeCode  : INTEGER; (* last subtype code of this type's subtype *)
    selfID           : INTEGER;
    fp               : Fingerprint;
    traced           : INTEGER; (* 0=>untraced, 1=>traced *)
    dataOffset       : INTEGER;
    dataSize         : INTEGER;
    dataAlignment    : INTEGER;
    methodOffset     : INTEGER;
    methodSize       : INTEGER;
    nDimensions      : INTEGER;       (* > 0 iff open array *)
    elementSize      : INTEGER;
    defaultMethods   : ADDRESS;       (* # NIL iff object type *)
    type_map         : ADDRESS;       (* RTTypeMap.T *)
    gc_map           : ADDRESS;       (* reduced RTTypeMap.T for collector *)
    type_desc        : ADDRESS;
    initProc         : TypeInitProc;  (* called by NEW *)
    linkProc         : TypeSetupProc; (* called during initialization *)
    parentID         : INTEGER;
    parent           : TypeDefn;
    children         : TypeDefn;
    sibling          : TypeDefn;
    brand            : String;
    name             : String;
    next             : TypeDefn;
  END;
(*
  dataOffset:
     for object types, the quantity to add to the address of the object
             to get to the fields that belong to this object type;
     for refs, unused;
     for open arrays, the quantity to add to the address of the array
             to get to the elements
  dataSize:
     for object types, the size of the fields that belong to this type;
     for refs, the size of the referent;
     for open array types, the size of the "open overhead", including
             padding to align the elements; i.e. ADR (a[0]) - ADR (a)
  dataAlignment:
     for object types, the alignment of the referent;
     for refs, the alignment of the referent;
     for open arrays, the alignment of the full array, including the header
*)

TYPE
  TypeInitProc  = PROCEDURE (ref: ADDRESS);
  TypeSetupProc = PROCEDURE (def: TypeDefn);

(*----------------------------------------- compiler generated references ---*)

CONST
  NilTypecode  : Typecode = 0;
  TextTypecode : Typecode = 1;

CONST
  CCB = 4;                                 (* bits for copying collector   *)
  TCB = 20;                                (* bits for typecode            *)
  RCB = 8;                                 (* bits for ref count collector *)
  SB = BITSIZE (ADDRESS) - (CCB+TCB+RCB);  (* # spare bits in a ref header *)

TYPE
  RefHeader = RECORD
    (* copying collector fields *)
    forwarded : BITS   1 FOR BOOLEAN  := FALSE; (* used during collection *)
    weak      : BITS   1 FOR BOOLEAN  := FALSE; (* any weakrefs? *)
    marka     : BITS   1 FOR BOOLEAN  := FALSE; (* used during collection *)
    markb     : BITS   1 FOR BOOLEAN  := FALSE; (* used during collection *)
    (* typecode *)
    typecode  : BITS TCB FOR Typecode := 0;     (* the typecode *)
    (* reference counting fields *)
    (* see SRC Report 64 for documentation *)
    Z         : BITS   1 FOR BOOLEAN  := FALSE; (* on Zero Count List *)
    V         : BITS   1 FOR BOOLEAN  := FALSE; (* refcount overflow  *)
    refcount  : BITS   6 FOR [0 .. 63] := 0;
    (* spare fields *)
    spare     : BITS  SB FOR [0 .. 0] := 0;     (* for future expansion *)
  END;

TYPE  (* header for compiler generated TEXT literals (1-D open array of char)*)
  TextHeader = RECORD
    chars  : String;
    length : INTEGER;
  END;

(*---------------------- compiler generated argument passing description  ---*)

CONST
  FormalModeVALUE = 0;
  FormalModeVAR   = 1;
  FormalModeCONST = 2;

(*--------------------------------- compiler generated procedure closures ---*)

CONST
  ClosureMarker = -1;

TYPE
  ProcedureClosure = RECORD
    marker : INTEGER; (* == -1 *)
    proc   : PROCANY; (* address of a nested procedure's body *)
    frame  : ADDRESS; (* its parent's frame pointer *)
  END;

END RT0.

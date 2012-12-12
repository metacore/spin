(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Thu May  4 10:54:10 PDT 1995 by kalsow     *)
(*      modified on Fri May 28 14:34:04 PDT 1993 by muller     *)
(*      modified on Tue Sep  8 11:51:46 PDT 1992 by jdd        *)

(*
 * HISTORY 
 * 16-Jul-97  Tian Fung Lim (tian) at the University of Washington
 *	Set MaxHash to 0 because we need 32 bits for the size field.
 *
 * 27-Jan-97  Wilson Hsieh (whsieh) at the University of Washington
 *	added ref header fields for reference counting collector
 *
 * 02-Nov-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added refcount field to the object header type.
 *
 * 27-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	encode INLINE possibilities in procedure types
 *
 * 27-Aug-96  Wilson Hsieh (whsieh) at the University of Washington
 *	encode FUNCTIONAL and EPHEMERAL in procedure types, by using
 *      a bit each in the result count
 *
 * 14-Jun-96  Przemek Pardyak (pardy) at the University of Washington
 *	Made Typecode be 20 bit index again.  NARROW code accesses
 *	RT0u.types to get subtype information.
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
 *	during startup of M3 program). The set of its subtypes was represented 
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

(* RT0 is "the bottom of the world".  It contains
   types and constants that are shared by multiple modules of
   the runtime and/or the compiler and linker.

   If you're using this interface, you're a wizard!

   This interface and its implemenation MUST NOT import any
   other interface.
*)

INTERFACE RT0;
IMPORT RTMachine; (* FIXME: FORMALLY IT'S NOT OK TO HAVE THIS IMPORT HERE *)

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
    (* type   : ADDRESS; (* SPIN *)*) (* FIXME: DO THIS *)
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
    (* FIXME 
    class            : INTEGER;
    *)
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

TYPE GcStatus = { Unused, Untraced, NotMarked, Marked, RecursivelyMarked };

CONST
  MaxReferentSize = RTMachine.MaxReferentSize;
(*  MaxHash = RTMachine.MaxHash;*)
  MaxHash = 0;
  NB = 1 + RTMachine.LogMaxReferentSize + RTMachine.LogMaxHash;
  (* 4 bits for copying collector
     20 bits for typecode
     NB for size/hash -- is 34
     7 for reference counting collector
   *)
  SB = BITSIZE (ADDRESS) - 4 - 20 - NB - 7; (* # spare bits in a ref header *)
  LowRef = 0;
  HighRef = 31;
  MidRef = 16;

TYPE
  RefHeader = RECORD
    forwarded : BITS  1 FOR BOOLEAN  := FALSE; (* used during collection *)
    gcStatus  : BITS  3 FOR GcStatus := GcStatus.NotMarked;
    typecode  : BITS 20 FOR Typecode := 0;     (* the typecode *)
    immobile  : BITS  1 FOR BOOLEAN  := FALSE; (* used during collection *)
    (* reference counting fields *)
    (* see SRC Report 64 for documentation, we are using the same meanings *)
    Z         : BITS   1 FOR BOOLEAN  := FALSE; (* on Zero Count List *)
    V         : BITS   1 FOR BOOLEAN  := FALSE; (* refcount overflow  *)
    refcount  : BITS   5 FOR [LowRef .. HighRef] := 0;
    spare     : BITS SB FOR [0 .. 0] := 0;     (* for future expansion *)
    hash      : BITS  0 FOR [0..0] := 0;
    size      : BITS 32 FOR [0 .. 16_ffffffff] := 0;

(*
    hash      : BITS RTMachine.LogMaxHash FOR [0..MaxHash] := 0;
    size      : BITS RTMachine.LogMaxReferentSize FOR [0..MaxReferentSize] := 0;
    *)
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
  FormalModeCVAR  = 3;  (* should never appear in M3 code *)

  (* procedure type modifiers are stored in result count *)
  ProcHasResult    = 16_1;
  ProcIsEPHEMERAL  = 16_2;
  ProcIsFUNCTIONAL = 16_4;
  ProcIsINLINE     = 16_8;
  ProcCallsINLINE  = 16_10;

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

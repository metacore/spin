(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
 * HISTORY
 * 03-Nov-95  Yasushi Saito (yasushi) at the University of Washington
 *	created
 *)

INTERFACE Type;

(*
 
  Type.T represents a Modula-3 type.
 
 *)

IMPORT TypeTable;
IMPORT IWr;
IMPORT M3AST_SM_F;
IMPORT Msg;
IMPORT ParamAddr;
IMPORT TextRefTbl;

TYPE 
  T <: TPublic;
  TPublic = OBJECT
    (* 
      If the type is assigned a name using TYPE declaration, then
      ALIAS holds that name. Otherwise, it's NIL.
     *)
    alias: TEXT := NIL;
    
    (* Size and required alignement of this type, both in bits. *)
    bitsize, align: CARDINAL;
    
    (* If true, pack and unpack codes are expanded inline even if
     the type has an alias. Otherwise, do procedure call.
     Only trivial data types has this field TRUE.

     XXX THIS FIELD ISN'T USED NOW.
     *)
    allowInlinePackingP: BOOLEAN;
    (* Incremented each time this type appear in the parse tree.
     XXX You don't have to know about the use of this field. *)
    nUsed: CARDINAL;

    (* If TRUE, then this type is not visible to the client.
       This is true for only predefined special types, ex. Strand.T.
     
     *)
    extensionOnly: BOOLEAN;
    
    (* The source code location where this type appeared.
     Used when reporting an error. *)
    srcPos: Msg.Pos;
    
  METHODS
    instanceName(userName: TEXT): TEXT;
    (* "instanceName" returns the real name of the
     variable defined as "userName" in the interface definition.

     This procedure mostly returns "userName" itself, but there are
     a few exceptions.

     One is an extensiononly variable whose instance
     is unique. For example, an extension only
     variable of type "CPU.SavedState" always has the name
     "s_" in the extension code, regardless of name user specifies.

     Another example is an open array converted from user side "char*".
     In this case, we have to return "SUBARRAY(u_array_, 0, u_len_)"
     where "u" is the value of "userName".
     
     *)
    
    toText(useAlias := TRUE; extensionCode := TRUE): TEXT;
    (* Return the textual representation of the text.
      This is actually a reverse compilation from abstract syntax 
      into the (almost) original textual notation.
     
      If USEALIASP is TRUE and if the type has an alias(i.e., assigned
      a name using TYPE decl.), that name is returned instead of
      structual representation. If USE_ALIAS_P is FALSE, then structual
      representation is returned always.
     
      If EXTENSIONCODE is TRUE, then the code is to be used in the
      extension code. Else the code is used in the user code.
      This parameter is used only when you want to change the represen
      tation of the type between user and extension. Typical example of 
      such type is externalized references.
     *)
    
    declareVar(name: TEXT; useAlias := TRUE; extensionCode := TRUE): TEXT;
    (* Declare the variable "name". *)
    
    (* Return the C representation of the type. NAME is the
     *  variable or typename that should be declared to be of this type.
     * Return NIL if it can't be expressed in C.
     *)
    toCText(name: TEXT; useAliasP := TRUE): TEXT;
    
    pack(var: TEXT; addr: ParamAddr.T; wr: IWr.T);
    
    (* Emit the code into WR that fetches the the value
     * located at ADDR.
     * and stores to VARIABLE. The code is supposed to run as a
     * spindle.
     *)
    unpack(var: TEXT; addr: ParamAddr.T; wr: IWr.T);

    destruct(var: TEXT; wr: IWr.T);
    (* Called after the last use of "var". *)
    
    (* Return TRUE if we can use VIEW on the whole structure.
     *  REF Foo returns FALSE trivially, and any records or arrays
     *  that contains a REF as a member will return FALSE.
     *
     *)
    viewable(): BOOLEAN;

    (* See if we need caller_ or space_ to unpack the value.
     
     *)
    needSpace(): BOOLEAN;

    exceptionOnUnpack(): REF ARRAY OF TEXT;
    exceptionOnPack(): REF ARRAY OF TEXT;
    
    (* Notification to the type object that this type is used as an output
       variable. Typically, what this method does is that if the type is a reference,
       or if it contains a reference as its member, set it's capabilityP TRUE.
     *)
    externalize();
    
    (* Fill in align and bitsize from TS. *)
    init(ts: M3AST_SM_F.TYPE_SPEC; READONLY pos: Msg.Pos);
  END;

TYPE
  Ordinal <: OrdinalPublic;
  OrdinalPublic = T BRANDED OBJECT END;

  Enum <: EnumPublic;
  EnumPublic = Ordinal OBJECT
    values: REF ARRAY OF TEXT;
  END;

  Char = Enum BRANDED OBJECT END;

  (* Integer is conceptually a subtype of Subrange, but it is
     treated differently because it is viewable, hence we can
     emit efficient code for packing/unpacking.. *)
  ViewableOrd <: Ordinal;
  
  Subrange <: SubrangePublic;
  SubrangePublic = Ordinal OBJECT
    base: Ordinal; (* ?? *)
    min, max : INTEGER;
  END;

  (* INTEGER is represented by a Subrange p with p.base = p,
   p.min = FIRST(INTEGER) and p.max = LAST(INTEGER). *)
  
  Real = T BRANDED OBJECT END;

  LongReal = T BRANDED OBJECT END;

  Extended = T BRANDED OBJECT END;

  Ref <: RefPublic;
  RefPublic = T OBJECT
    traced: BOOLEAN;
    branded: BOOLEAN;  
    brand : TEXT;  (* IF branded AND brand=NIL then the type was
		     declared as branded, with no brand given *)
    
    (* CAPABILITY_P is TRUE if this type is used as a return value by
     * some extention. In such case, we have to use different
     * representation between user and spindle codes.
     *)
    capabilityP: BOOLEAN;
    
    (* Target type. May be NULL, if it is REFANY *)
    target: T;
    targetName: TEXT;
  END;

  (* Note that a type declared as opaque is represented by an object
   of type Object if it is fully revealed in an interface
   given to the stub generator *)

  Opaque <: OpaquePublic;
  OpaquePublic = Ref OBJECT
    revealedPart: T;
    
    (* If this type was declared opaquely but revealed in
     a module included in stub generation, revIntf is the
     name of the interface containing the revelation *)
    (* XXX I don't know how it works -- yasushi *)
    revIntf: TEXT;
  END;

  Object <: ObjectPublic;
  ObjectPublic = Ref OBJECT
    ancestor: TEXT;
    methods: REF ARRAY OF Field;
    overrides: REF ARRAY OF Field;
  END;

  (* REF ARRAY OF FOO, and VAR x: ARRAY OF FOO are very similar. In fact, they
   *  are implemented in the same way.
   *)
  RefArray <: OpenArrayPublic;
  OpenArray <: OpenArrayPublic;
  OpenArrayPublic = RefPublic OBJECT
    nDims: CARDINAL;
  END;
  
  Text = Opaque BRANDED OBJECT END;

  Array <: ArrayPublic;
  ArrayPublic = T OBJECT
    index: REF ARRAY OF Ordinal;  (* May be a multi dimensional array.
				   Therefore we have array of Ordinals. *)
    element: T
  END;
  
  (* index = NIL for open array types. *)
  
  Packed <: PackedPublic;
  PackedPublic = T OBJECT
    size: INTEGER;
    base: T;
  END;

  Record <: RecordPublic;
  RecordPublic = T OBJECT
    fields: REF ARRAY OF Field;
  END;
	
  Set <: SetPublic;
  SetPublic = T OBJECT
    range: Ordinal;
  END;

  (* <*AS WORD*> type. *)
  RawWord <: RawWordPublic;
  RawWordPublic = Ordinal OBJECT
    m3ArgTemplate, cArgTemplate: TEXT;
  END;
  
  Field = RECORD
    name: TEXT;
    type: T;
    default: TEXT; (* NIL if default value is not supplied *)
  END;
  
  Mode = {Value,(* VALUE *)
	  In, 	(* READONLY *)
	  InOut,(* VAR *)
	  Out 	(* <*OUT*>VAR *)
	  };
  
  (* Param is a pseudo-type for formal params for procedures. *)
  Param = RECORD
    name: TEXT; (* param name *)
    type: T;
    default: TEXT; (* default value *)
    mode: Mode; (* in, out or both *)
    inReg, outReg: CARDINAL; (* this is filled in by register scheduler.
			       inReg is the register where you pass in
			       this parameter, and outReg is opposite.
			       outReg is meaningful only when you do
			       call by value-result. *)
  END;
    
  (* Proc is used not only when procedure type is used, but also to carry
   * information about usual procedure definitions.
   * PROC_NAME is the name of the procedure for usual proc definition.
   * It is NIL when proc is used as a type. XXX Anyway, we don't support
   *  procedure type now. 
   *)  
  Proc = T OBJECT
    retType: T; (* may be NIL *)
    params: REF ARRAY OF Param;
    needTrampolineCode: BOOLEAN; (* do we have to create intermediate
				   m3 procedure? *)
    needSpace, needArgs: BOOLEAN;
    raises: TextRefTbl.T;
  END;
  
VAR (* READONLY *)
  integer      : ViewableOrd;
  cardinal     : Subrange;
  boolean      : Enum;
  char         : Char;
  real         : Real;
  longreal     : LongReal;
  extended     : Extended;
  refany       : Ref;
  root         : Ref;
  null         : Ref;
  text         : Text;
  ctext	  	: Text;
  carrayOfChar : OpenArray;
  nsname : Text;
  
  (* extensiononly types *)
  strand       : T;
  state        : T;
  space        : T;
  
(* Add most of the standard types into the table astTable.

 The point here is that most of the identifiers that have same
 structually equivalent type shares same structure for its
 type field. I would say "most of" because I found that
 some very basic types such as INTEGER or REAL are not shared.
 I don't know why.
 *)
PROCEDURE RegisterStandardTypes(t: TypeTable.T);

(* Given the # of bits occupied by a type, this procedure returns the
   # of bytes used for that type.
 *)
PROCEDURE BitsToBytes(v: INTEGER): INTEGER;
PROCEDURE Align(t: T; VAR addr: INTEGER);

PROCEDURE OutputProcDecl (name: TEXT; p: Proc; extensionCode: BOOLEAN)
 : TEXT;
(* Output prototype decl for the proc.
 *) 

PROCEDURE GetAliasForRepresentationIndependentType(bits: INTEGER): T;
(* Given a bitsize of some ordinal type,
   return a canonical type name that has same size as that type, and
   is viewable.
 *)

PROCEDURE True(t: T): BOOLEAN;
PROCEDURE False(t: T): BOOLEAN;
(* They are the trivial implementations of predicate methods *)
  
END Type.


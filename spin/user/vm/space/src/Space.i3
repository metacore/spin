(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 19-Jul-97  Yasushi Saito (yasushi) at the University of Washington
 *	Moved externref into vmcore.
 * 26-Jun-97  Yasushi Saito (yasushi) at the University of Washington
 *	Made Space.T an alias of AddressSpace.T
 * 02-Jun-97  Yasushi Saito (yasushi) at the University of Washington
 *	Marked many of the procedures OBSOLETE. I'll phase out this module
 *	soon.
 * 19-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	made GetCurrent FUNCTIONAL
 *
 * 18-Jun-96  Yasushi Saito (yasushi) at the University of Washington
 *	Changed types of faults.
 *
 * 6-21-96  becker at the University of Washington
 *	Added Duplicate proc which returns a space identical to its arg.
 *	Added Clear to Deallocate entire space.
 *
 * 29-Apr-96  Stefan Savage (savage) at the University of Washington
 *	Moved GetKernel to SpacePrivate (bershad)
 *
 * 07-Apr-96 Jan Sanislo (oystr) at the University of Washington
 *    Add MapPhysToVirt for mapping frame buffer.
 *
 * 11-Mar-96  David Dion (ddion) at the University of Washington
 *	Added support for external refs
 *
 * 05-Oct-95  Emin Gun Sirer (egs) at the University of Washington
 *	Changed the interface to raise specific exceptions instead of ANY.
 *
 * 01-Aug-95  Stefan Savage (savage) at the University of Washington
 *	Added protection constants
 *
 * 26-Jul-95  Stefan Savage (savage) at the University of Washington
 *	No longer external functions, space opaque
 *
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *	Changed the implementation to call the external functions directly.
 *
 * 02-Dec-94  Stefan Savage (savage) at the University of Washington
 *      Created
 *)

(*

 Space is a subtype of <a href="../../vmcore/doc/AddressSpace.html">
 AddressSpace.T</a>. In addition to all the services in
 AddressSpace, it provides convenient shortcut services.
 Also, address space provides an access to <!--href="externref.html">
 external ref table</a>.

 <strong>Note:</strong>
 Space is basically a legacy module. It was once an abstraction provided
 directly in the spincore, so many modules depends on its interface.
 However, I see no strong reason this should not be merged into
 AddressSpace module.

 Another caveat is that all the arguments in this module
 are expressed by bytes, while all the procs in AddressSpace
 take page size unit
 arguments. They have to be unified in the future.

 *)

INTERFACE Space;

IMPORT VirtAddr, PhysAddr, ExternalRef, Word;
IMPORT AddressSpace;
IMPORT VMError;
IMPORT Protection;

TYPE
  T = AddressSpace.T;
  
CONST
  ReadOnlyProtection = Protection.T{TRUE,FALSE,FALSE,0};
  ReadWriteProtection = Protection.T{TRUE,TRUE,FALSE,0};
  ExecuteOnlyProtection  = Protection.T{FALSE,FALSE,TRUE,0};
  ExecuteProtection = Protection.T{TRUE,FALSE,TRUE,0};

<*OBSOLETE*>  
PROCEDURE Create(): T;
  (* Creates an empty address space. Note that Create() is now equivalent
     to "NEW(AddressSpace.T).init(NIL)". Use the latter syntax
     if you are writing new code. Create() is here only for
     a backward compatibility reason. *)

PROCEDURE Duplicate(newSpace, space: T) RAISES {VMError.E};
  (* Create a copy of the address space. Everything is eagerly copied now.
     XXX we have to iron out the semantics of this procedure wrt vm_inherit,
     etc. *)

<*OBSOLETE*>    
PROCEDURE Destroy(space: T);
  (* Destroy the "space" and free all the resources used by it.
     This is obsolete. Use "space.destroy()" instead. *)
  
PROCEDURE Allocate(space: T; VAR addr: Word.T; size: Word.T;
		   anywhere := FALSE) RAISES {VMError.E};
  (* Combination of allocate and memobject creation and map. *)
  
PROCEDURE Deallocate(space:T; addr, size: Word.T) RAISES {VMError.E};
  (* Combination of addressspace.unmap and deallocate. *)


<*OBSOLETE*>
PROCEDURE Read(space: T; fromaddr: VirtAddr.Address;
	       VAR to: ARRAY OF CHAR;
	       size := -1) RAISES {VMError.E};
  (* Read the region that starts from "fromaddr" into "to".
     If "size" is omitted, the size of the region is "NUMBER(to)".
     This may cause page fault as a side effect.

     Note: this is superceded by Translation.Read. *)

<*OBSOLETE*>  
PROCEDURE Write(space: T; READONLY from: ARRAY OF CHAR;
		to : VirtAddr.Address;
		size := -1) RAISES {VMError.E};
  (* Write to the region that starts from "to" from "from".
     If "size" is omitted, the size of the region is "NUMBER(from)".
     This may cause page fault as a side effect.
     Note: this is superceded by Translation.Write. *)
  
PROCEDURE Zero(space: T; addr, size: Word.T) RAISES {VMError.E};
  (* Zero clears the region.
  XXX this should be moved either to AddressSpace or Translation.
  I'm not sure which, so I leave it here -- yaz *)

<*OBSOLETE*>  
PROCEDURE Protect(space: T; addr, size: Word.T; prot: Protection.T)
  RAISES {VMError.E};
  (* Change the proction of the region. "v" is not used now.
  Note: use Translation.ChangeMappings instead.*)

<*OBSOLETE*>  
PROCEDURE Activate(space: T);
  (* Sets the current space. This is not needed any more. If you really,
     really, really need to switch the space(you must be as strong
     as Taz to do this), use Translation.Activate *)
  
<*OBSOLETE*>
FUNCTIONAL PROCEDURE GetCurrent(): T;
(* Get the current space. Use Translation.GetCurrent instead *)

<*OBSOLETE*>  
PROCEDURE GetExternalRefTable(space : T) : ExternalRef.T;
<*OBSOLETE*>
PROCEDURE DeleteExtRef(space: T; extptr : Word.T);
<*OBSOLETE*>
PROCEDURE Externalize(space: T; intptr: REFANY; 
		      pos : Word.T := ExternalRef.DontCare): Word.T;
<*OBSOLETE*>
PROCEDURE Internalize(space: T; extptr: Word.T) : REFANY;
<*OBSOLETE*>
PROCEDURE CopyExterns(src: T; dest: T);
(* Above procedures are all obsolete. Use
   self.getExternRefTable().xxx() instead. *)

PROCEDURE MapPhysToVirt(space: T; phys: PhysAddr.Address;
			addr, size: Word.T): INTEGER;
(*
 XXX This is UNSAFE.
 See implementation comments before using this routine.
 *)

(*
 * For List generic
 *)
CONST Brand: TEXT = "Space";
PROCEDURE Equal(space1, space2: T): BOOLEAN RAISES ANY; 

END Space.

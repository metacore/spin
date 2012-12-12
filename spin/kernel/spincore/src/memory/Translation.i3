(*
 * Copyright 1994, 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 15-Apr-97  Yasushi Saito (yasushi) at the University of Washington
 *	virt addr is now a page number, not byte address.
 *	
 * 14-Nov-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added brand for hash tables.
 *
 * 29-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Changed the structure of Proctection; "pad" is changed to "others".
 *
 * 28-Apr-96  Stefan Savage (savage) at the University of Washington
 *	Moved ProtectionFault to Trap.i3
 *
 * 1-Dec-95 Przemek Pardyak (pardy) at the University of Washington
 *	Made ProtectionFault raise exceptions.
 *
 * 21-Oct-95  Brian Bershad (bershad) at the University of Washington
 *	Removed unused IMPORT Dispatcher.
 *
 * 05-Oct-95  Emin Gun Sirer (egs) at the University of Washington
 *	Changed the interface to raise specific exceptions instead of ANY.
 *
 * 02-Nov-94  Stefan Savage (savage) at the University of Washington
 *	Created
 * html
 *)

(*
   
   Translation is an abstraction of an virtual address space.
   [PhysAddr].T can be mapped onto it.

   *)

INTERFACE Translation;

IMPORT PhysAddr;
IMPORT VirtAddr;
IMPORT Protection;
IMPORT VMError;

TYPE
  T <: ROOT;

PROCEDURE Initialize(map: T);
(* Constructor. *)
  
PROCEDURE Destroy(map: T);
(* Destroys a translation context. *)
  
<*OBSOLETE*>PROCEDURE Activate(map: T);
(* Sets the specified translation context as the <em>active</em> context.
   All TLB accesses after this call will refer 
   to the new Translation.T.  This interface is problematic because it is
   currently difficult to make other operations atomic with respect it.
   Therefore, <b>Activate</b> is only used (indirectly) by extensions in
   <i>Resume</i> handlers (in which interrupts are disabled).  The
   <b>Activate</b> call will probably disappear in favor of an interface
   for binding thread activations with translations. *)
  
FUNCTIONAL PROCEDURE GetCurrent(): T;
  (* Get the translation that the currently executing strand is
     bound to. Returns NIL if the current thread is not the user
     space thread(or its surrogate syscall handler). *)
  
PROCEDURE AddMapping(map: T; p: PhysAddr.T; begin, end: VirtAddr.Page;
		     prot: Protection.T)
  RAISES {VMError.E};
(* Map "p" onto the address "begin". If "p" is an usual page frame,
   it the page is mapped onto "begin" ond "end" is not used. If "p" is an
     an IO frame created by [PhysAddrPrivate].AllocateIOSpace, then
   "end" describes the last address on which the IO space is mapped.
   "begin" and "end" are both specified in MMU page unit.
   "prot" describes the protection mode.
   
   If the virtual address range is invalid (eg. refers
   to kernel text or data) or the frame is invalid, 
   then an exception is raised.

   Note: The current implementation does not keep track of previous
   operations and will let you overwrite previous mappings. Also, we do
   not currently use superpage mappings when they are possible.
  *)
  
PROCEDURE RemoveMapping(map: T; begin, end: VirtAddr.Page);
  (* "begin" and "end" are both specified in MMU page unit. *)
  
PROCEDURE ExamineMapping(map: T; begin: VirtAddr.Page): Protection.T;
  (* "begin" is specified in MMU page unit. *)
  
PROCEDURE ChangeMapping(map: T; begin, end: VirtAddr.Page; prot: Protection.T)
  RAISES {VMError.E};
  (* "begin" and "end" are specified in MMU page unit.

     XXX this is not implemented yet. *)

<*OBSOLETE*>  
PROCEDURE AccessRW(map: T; begin, end: VirtAddr.Page;
		   proc: PROCEDURE (page: VirtAddr.Address;
				    VAR content: PhysAddr.Content))
    RAISES {VMError.E};
(* "AccessRW" walks over the region ["begin" .. "end"] ("end" excluded),
   and for each page in the region, it calls the procedure "proc".
   "begin" and "end" are specified in MMU page unit. This procedure may
   cause page fault events. *)

<*OBSOLETE*>  
PROCEDURE AccessRO(map: T; begin, end: VirtAddr.Page;
		   proc: PROCEDURE (page: VirtAddr.Address;
				    READONLY content: PhysAddr.Content))
  RAISES {VMError.E};
(* Readonly version of "AccessRW". *)

PROCEDURE Access(map: T; addr, len: VirtAddr.Address;
		 proc: PROCEDURE (VAR content: ARRAY OF CHAR))
  RAISES {VMError.E};
PROCEDURE Read(map: T; addr: VirtAddr.Address; VAR buf: ARRAY OF CHAR)
  RAISES {VMError.E};
PROCEDURE Write(map: T; READONLY buf: ARRAY OF CHAR; addr: VirtAddr.Address)
  RAISES {VMError.E};
  
END Translation.




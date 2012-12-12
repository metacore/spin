(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 19-Jul-97  Yasushi Saito (yasushi) at the University of Washington
 *	Moved into vmcore. Made T an object.
 * 20-May-96  Yasushi Saito (yasushi) at the University of Washington
 *	Added NilRef.
 *	
 * 11-Mar-96  David Dion (ddion) at the University of Washington
 *	Added CopyTranslationTable
 *
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created. Externalized references.
 *)
INTERFACE ExternalRef;
IMPORT Word;

(*
   This interface can be used to allow user-level programs
   to obtain references to in-kernel objects (most importantly
   domains) and to pass them back in again.
   
   Externalize takes a reference and gives back a bitstring
   that can be placed into a register and passed back to user
   space. Internalize takes the same bitstring and returns the
   associated reference. The implementation of this interface 
   does not need to be trusted in any way.
   
   A typical client that wishes to Externalize a pointer will
   simply call externalize and shove the return result in a
   register (or some user memory address). To internalize a
   a reference, a client will take a value, call Internalize,
   and subsequently Narrow the pointer to the desired type.
  
   This interface guards against security violation of the user
   "guessing" addresses within the kernel and then checking to
   see if they have hit a kernel structure. This scheme, which
   is prevented in the kernel by Modula-3's safe addressing,
   becomes feasible at the kernel-user boundary and must be
   guarded against.
  
   One obvious shortcoming of this interface is that it is not
   possible to pass structures to user space that are UNTRACED.
   This is because an UNTRACED types derives from ADDRESS, and
   narrowing from an ADDRESS to an more specific UNTRACED type 
   can only be done in unsafe modules.

   Programmers are welcome to subtype the external ref table and
   create its own semantics. Note that a buggy externref table cannot
   harm anyone except the user of the table. 

   Notes for the default implementation:
   
   The Default implementation of ExternalRef.T do not allow duplicates
   references in the table. That is, externalizing some reference X
   twice returns the
   same value Y. Thus, externalize and delete are not symmetric.
   (ie, delete(externalize(X)) may change the state of table).
   The reference NIL always has a constant externalized ref
   value "NilRef", i.e.,
 
 | Externalize(any, NIL, any) = NilRef
 | Internalize(t, NilRef) = NIL

 If you call "DeleteReference(any, NilRef)", this is a nop.
   
 *)


EXCEPTION Conflict;
CONST
  NilRef = LAST(Word.T);	(* Nil Refs *)
  DontCare = -1;                (* Don't specify the slot in externalize *)

(* XXX we should create the Default type just like in Table.ig. *)
TYPE T <: Public;
  Public = OBJECT
  METHODS
    init(): T;
    destroy();
    externalize(ref: REFANY; slot := DontCare): INTEGER RAISES {Conflict};
    (* Create an externalized reference of "ref". If "slot" isn't
       "DontCare" and the externalized ref "slot" is already occupied
       by some other ref, this procedure raises the "Conflict" exception.
       Returns the newly created externalized reference. *)
    
    internalize(extref: INTEGER): REFANY;
    delete(extref: Word.T);
    copy(dest: T);
  END;
  
PROCEDURE GetCurrent(): T;
  (* Get the extern ref table attached to the current address space. *)
END ExternalRef.

(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 18-Feb-97  Charles Garrett (garrett) at the University of Washington
 *	AddAuthorizer lets you install an authorization procedure which
 *	 deny linking based on subtyping.
 *
 * 22-Mar-96  Marc Fiuczynski (mef) at the University of Washington
 *	Moved CreateFromSymbolsArray from DomainPrivate to here. 
 *
 * 09-Feb-96  Emin Gun Sirer (egs) at the University of Washington
 *	Added IsModula3.
 *
 * 06-Nov-95  Emin Gun Sirer (egs) at the University of Washington
 *      Added FullyResolved.
 *
 * 28-Sep-95  Brian Bershad (bershad) at the University of Washington
 *	Changed Apply and Domain Closure to work with functions returning
 *	BOOLEANS rather than REFANYs.
 *
 * 25-Jul-95  Brian Bershad (bershad) at the University of Washington
 *	Removed core interfaces
 *
 * 12-Jul-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created DomainInternal and moved the trusted component of the
 *	interface there.
 *
 * 07-Jul-95  Emin Gun Sirer (egs) at the University of Washington
 *	Added a special debug domain that identifies unresolved symbols
 *      when linked against.
 *
 * 05-Jul-95  Emin Gun Sirer (egs) at the University of Washington
 *	Made clients of create safe by making it use open arrays.
 *
 * 27-Nov-94  Emin Gun Sirer (egs) at the University of Washington
 *	Created.
 *)
INTERFACE Domain;
IMPORT SymbolEntry, Auth, RT0;

TYPE
  T <: REFANY;

CONST
  debugName = "Debug";
  debugIE = FALSE;     (* debug flag for checking imports/exports *)

VAR debug: T;	(* Link against me to find out what is wrong with you *)


(* 
 * Create: Create a new domain with the given code in it. Default 
 * is to mark the domain as untrusted. Array must contain a legitimate,
 * "certified" coff array. If code = NIL then we just create an empty
 * "container" domain with the specified name.
 *)
PROCEDURE Create(name: TEXT; code: REF ARRAY OF CHAR;
                          collectable: BOOLEAN := TRUE) : T;
PROCEDURE NewCreate(name: TEXT;
                 READONLY code: ARRAY OF BITS 8 FOR [0..255];
                 collectable: BOOLEAN := TRUE) : T ;

(* called through Interface.mg and UnsafeInterface.mg *)
PROCEDURE CreateFromSymbolsArray((*module: RTCode.Module;*)
                                 module: ADDRESS;
                                 name: TEXT; 
                                 READONLY symbols: ARRAY OF SymbolEntry.E): T;

(* Dynamic Linker *)
PROCEDURE Resolve(mapper, mappee: T);
PROCEDURE Unresolve(mapper, mappee: T);
PROCEDURE GetSymbolValue(domain: T; 
                         name: TEXT;
                         VAR found: BOOLEAN) : ADDRESS;  (* FIXME: unsafe!! *)

(* Create and manipulate nested sets of domains *)
PROCEDURE Add(container, element: T);
PROCEDURE Delete(container, element: T);

(* create a new alias for a given domain *)
PROCEDURE Dup(domain: T) : T;

(* manipulate domain capabilities *)
PROCEDURE GetRights(domain: T; VAR modify: BOOLEAN; VAR import: BOOLEAN);
PROCEDURE SetRights(domain: T; modify: BOOLEAN; import: BOOLEAN);

(* install an authorizer to guard against subtyping *)
PROCEDURE AddAuthorizer(domain: T; auth: Auth.T);
PROCEDURE AddTypeAuthorizer(domain: T; typename: TEXT; auth: Auth.T);

(* turn the ability to implement interfaces off *)
PROCEDURE PreventExportOfInterfaces (domain: T);

(* call immediately after done building static domains *)
PROCEDURE StaticDomainsInitialized ();

(* See if a domain is completely resolved *)
PROCEDURE FullyResolved(domain: T) : BOOLEAN;

(* See if a domain contains any Modula-3 code *)
PROCEDURE IsModula3(domain: T) : BOOLEAN;

(* Initialize a domain *)
PROCEDURE Initialize(x: T) : BOOLEAN;

(* Destroy a domain when outstanding references are lost *)
PROCEDURE Destroy(domain: T): BOOLEAN;

PROCEDURE Stats(VAR subdomains:INTEGER; VAR bytes:INTEGER);
(* Stats returns how many subdomains have been created and how many
	bytes were in the original object files *)

(*
 * Apply calls the apply method on the given domain (or unit or procedure)
 * until the apply method returns FALSE.  At that point, it returns
 * with the value returned by the application. 
 *)

TYPE
  Closure = OBJECT
  METHODS
    apply(d: T): BOOLEAN;
  END;

(* Call the closure for the domain and all of its subdomains *)
PROCEDURE ApplyNoLock (domain: T; cl: Closure): BOOLEAN;

(* Call the closure for all the domains in the system *)
PROCEDURE ApplyToAllNoLock(cl: Closure): BOOLEAN;

TYPE
  UnitClosure = OBJECT
  METHODS
    apply(unit: RT0.ModulePtr): BOOLEAN;
  END;

(* Call the closure for all units within the domain *)  
PROCEDURE ApplyToUnitsNoLock (domain: T; cl: UnitClosure): BOOLEAN;

TYPE
  ProcClosure = OBJECT
  METHODS
    apply(proc: PROCANY): BOOLEAN;
  END;
  
(* Call the closure for each procedure within the unit *)
PROCEDURE ApplyToProcedures (unit: RT0.ModulePtr; cl: ProcClosure): BOOLEAN;

(*
 * Stripping
 *)

PROCEDURE Strip(d: T);
PROCEDURE StripAllDomains();

(* 
 * Generic LIST Support 
 *)
CONST Brand = "Domain";
PROCEDURE Equal(k1, k2: T): BOOLEAN;

END Domain.


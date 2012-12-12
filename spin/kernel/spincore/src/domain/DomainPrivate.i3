(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 18-Feb-97  Charles Garrett (garrett) at the University of Washington
 *	AddressToDomain and TypeDefnToDomain allow you to find out what
 *	 domain contains an address or type definition. Currently
 *	 addresses in the static part of the kernel all map to
 *	 SpinPublic. Type definitions in the static kernel will return
 *	 the domain which exports that type.
 *
 * 11-Feb-97  Przemek Pardyak (pardy) at the University of Washington
 *	Exported CollectM3Modules and variables describing static kernel.
 *
 * 14-Nov-96  Przemek Pardyak (pardy) at the University of Washington
 *	Moved nextID from Domain.m3 to DomainPrivate.i3.
 *
 * 18-Apr-96  Przemek Pardyak (pardy) at the University of Washington
 *      Added verbose mode to print information about progress of domain
 *	creation, linking, checking and initialization.  Added a flag
 *	to turn on/off interface checking.
 *
 * 25-Jul-95  Brian Bershad (bershad) at the University of Washington
 *	removed unsafeCore declaration.
 *
 * 12-Jul-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Domain interface for trusted clients.
 *
 *)
INTERFACE DomainPrivate;
IMPORT Domain, MachineLinker, Word, Mx, RTLinker;

TYPE
  ModulePtrs   = RTLinker.ModulePtrs;
  LinkInfoPtrs = UNTRACED REF ARRAY OF UNTRACED REF CHAR;

TYPE 
  RangeT = REF RECORD 
    start, stop: ADDRESS; 
    next: RangeT;
  END;

VAR
  checkInterfaces: BOOLEAN := TRUE;
  verbose: BOOLEAN := FALSE;
  nextID: INTEGER := 0;
  staticLinkBase  : Mx.LinkSet  := NIL;
  staticModuleCnt : INTEGER := 0;

(* The exception indicates an attempt to look up an address that is 
   outside of any known domain. *)
EXCEPTION NonDomainAddress;
EXCEPTION NonDomainTypeDefn;

(* Generate new domain using code at any address *)
PROCEDURE CreateFromAddress(name: TEXT; code: ADDRESS;
                            collectable: BOOLEAN := TRUE) : Domain.T;


(*
 * Generate a new domain with only the specified symbols in it.  Domain
 * is marked as TRUSTED if you come from this pathm, since the domain
 * specifier information is coming from statically linked code.   syms and
 * size describe the contents of the array in an already extracted form.
 *)
PROCEDURE CreateFromSymbols(name: TEXT; syms: ADDRESS; size: INTEGER):Domain.T;
PROCEDURE CreateFromModule(name: TEXT; m: MachineLinker.T) : Domain.T;

PROCEDURE SetState(d: Domain.T; name: TEXT; trusted: BOOLEAN);
PROCEDURE GetState(d: Domain.T; VAR name: TEXT;
                   VAR trusted: BOOLEAN;
                   VAR dynamic: BOOLEAN);

(*
 * Extract information about underlying representation
 *)
PROCEDURE ShowModules(d: Domain.T);

(* Get start and size info for dl module.  Return FALSE if none available. *)
PROCEDURE TextInfo (d: Domain.T; VAR start: Word.T; VAR size: INTEGER):
  BOOLEAN;

PROCEDURE Nlist(d: Domain.T);

(*
 * Unsafe iterators.  They are unsafe because the take the main domain lock.
 * This restriction will be lifted when we move onto finer grain locks.
 *)
PROCEDURE Apply(domain: Domain.T; cl: Domain.Closure): BOOLEAN;
PROCEDURE ApplyToAll(cl: Domain.Closure): BOOLEAN;
PROCEDURE ApplyToUnits (domain: Domain.T; cl: Domain.UnitClosure): BOOLEAN;

(* Return the smallest domain containing a given address. *)
PROCEDURE AddressToDomain(addr: ADDRESS;
                          inInterface: BOOLEAN := FALSE): Domain.T
                          RAISES {NonDomainAddress};
PROCEDURE TypeDefnToDomain(addr: ADDRESS): Domain.T RAISES {NonDomainTypeDefn};

(*
 * For M3 dynamic linking
 *)
PROCEDURE CollectM3Modules(domain: Domain.T; 
                           VAR totalmods: ModulePtrs;
                           VAR totalinfo: LinkInfoPtrs);

PROCEDURE Init(verbose: BOOLEAN);
PROCEDURE InitM3(verbose: BOOLEAN);

END DomainPrivate.


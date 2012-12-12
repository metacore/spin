(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 * 
 * Implementation of SPIN protection domains.
 *
 * HISTORY
 * 18-Feb-97  Charles Garrett (garrett) at the University of Washington
 *	Added an authorizer field to control linking, and an
 *	 exported_intf field which tells static domains what interfaces
 *	 they export.
 *
 * 11-Feb-97  Przemek Pardyak (pardy) at the University of Washington
 *	Added the destroyed flag.
 *
 * 06-Nov-96  Wilson Hsieh (whsieh) at the University of Washington
 *	allow subtyping restriction
 *
 * 22-Nov-96  becker at the University of Washington
 *	Added TrackDomain
 *
 * 02-Feb-96  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Representation of a SPIN protection domain.
 *
 *)
INTERFACE DomainRep;
IMPORT Domain, MachineLinker, Mx, Auth;
IMPORT SymbolEntry;
IMPORT TrackDomain;
(*
 * A Domain.T is a reference to some code and some symbol to value
 * translations.
 *
 * Domains nest, hence form trees. The children of a domain, i.e. its 
 * subdomains, hang off of subdlist, and can in turn contain other subdomains.
 *)
REVEAL 
  Domain.T = BRANDED REF RECORD
    name: TEXT := "anon";
    module: MachineLinker.T := NIL;(* coff module that this domain points to *)
    subdlist: DList := NIL;    (* Subdomain (children) of this tree node *)
    collectable: BOOLEAN;      (* support for per domain heaps *)
    modify: BOOLEAN := TRUE;   (* right to change code in this domain *)
    import: BOOLEAN := TRUE;   (* right to import symbols from this domain *)
    trusted: BOOLEAN := FALSE;
    dynamic: BOOLEAN;	       (* dynamically linked?? *)

    (* permission to EXPORT interfaces in domain *)
    interfacesCanBeImplemented := TRUE;

    parent: Domain.T;
    id: INTEGER;
    next: Domain.T;
    linked: DList := NIL;
    rev_linked: DList := NIL;
    linkBase: Mx.LinkSet := NIL; (* result of M3 linking of the domain *)
    imports: DList := NIL;    (* list of domains containing imported M3 code *)
    track: TrackDomain.T;

    (* exported interfaces for static domains *)
    exported_intf : REF ARRAY OF SymbolEntry.E := NIL; 

    authorizer: Auth.T := NIL;
    destroyed: BOOLEAN := FALSE;
  END;

(*
 * The List generic doesn't allow O(N) pass over a list that is N long.
 * Further, deletions require O(N-1) allocations instead of 0.
 * That's why we use DLists here instead of DomainList.T.
 *)
TYPE
  DList = REF RECORD
    domain: Domain.T;
    next : DList := NIL;
  END;

PROCEDURE PrintDList (dl: DList);

END DomainRep.

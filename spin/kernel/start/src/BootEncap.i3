(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
 * HISTORY
 * 07-Aug-96  Marc Fiuczynski (mef) at the University of Washington
 *	Cleanup.
 *
 *)

(* "BootEncap" interface provided to static encapsulated domains *)
INTERFACE BootEncap;
IMPORT Domain;
PROCEDURE Lookup(name: TEXT; VAR d: Domain.T):BOOLEAN;
(* "Lookup" a domain in the nameserver. *)

PROCEDURE Init(verbose:BOOLEAN);
(* "Init" is exported by the generated BootEncapInit module, and
   invokes the Init routines of the various Encap_* modules. *)

TYPE Image = UNTRACED REF ARRAY OF BITS 8 FOR [0..255];
TYPE T = RECORD
  objectName  : TEXT;
  objectImage : Image;
END;

END BootEncap.

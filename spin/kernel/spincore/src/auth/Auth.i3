(* 
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * Basic authorizer definitions.  Modules needing work through trusted
 * intermediaries, but unwilling to defer all authorization predicates
 * should build an authorization protocol from these building blocks.
 *
 * HISTORY
 * 26-Apr-95  Przemek Pardyak (pardy) at the University of Washington
 *	Added an argument to authorization. 
 *
 * 24-Jan-96  Brian Bershad (bershad) at the University of Washington
 *	Created
 *
 *
 *
 *)


INTERFACE Auth;

EXCEPTION Retry;

TYPE
  Key = REFANY;
 

  T = OBJECT
	  METHODS
	    authorize(key: Key; arg: REFANY := NIL): BOOLEAN 
                      RAISES {Retry};
	  END;

  AuthNever <: T;
  AuthAlways <: T;
  AuthAuthKey <: T;	(* TRUE IFF key = self *)

CONST Brand = "Authorizer";        (* Used by Table generic. *)

END Auth.

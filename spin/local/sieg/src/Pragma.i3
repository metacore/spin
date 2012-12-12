(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 13-May-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)


INTERFACE Pragma;
IMPORT Module, AST;

PROCEDURE Find (READONLY m: Module.T; p: AST.NODE; directive: TEXT) : TEXT;
(* Find a pragma "directive" that appears just before the ast node "p".
 "p" can be NIL. In that case, this procedure looks for all the
 nodes in the interface file to find the pragma. Returns the pragma body.
 For example, in case of a pragma <*FOO bar buz*> and call to this
 proc with directive := "FOO" will return "bar buz".  *)

END Pragma.

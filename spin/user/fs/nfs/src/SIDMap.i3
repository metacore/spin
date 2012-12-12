(* 
 * Copyright 1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * Support for mapping security identifiers onto legacy file systems
 *
 * HISTORY
 * 
 * 07-Aug-97  Robert Grimm (rgrimm) at the University of Washington
 *      Changed over to new security manager.
 *      Removed DTE references and replaced them with abstract SIDs.
 *
 * 14-Mar-97  Robert Grimm (rgrimm) at the University of Washington
 *      Created
 *
 *
 *)

INTERFACE SIDMap;

IMPORT NameServer, SecurityManager;

  (*
   *  The principal types of the SID mapping
   * ----------------------------------------
   *)

TYPE
  ToT   = SecurityManager.SID; (* What we are mapping into.      *)
  BaseT = MUTEX OBJECT         (* Base type for mapping nodes.   *)
    sid : ToT;
  END;
  T <: BaseT;                  (* Real mapping nodes are opaque. *)

CONST
  Brand = "SIDMap";
  UseMe = FALSE;

(*
 *  Path delimiter, as well as self and parent components
 * -------------------------------------------------------
 *)

VAR
  pathDelimiter := '/';  (* Path delimiter    *)
  nameSelf      := ".";  (* Directory itself  *)
  nameParent    := ".."; (* Parent directory  *)

(*
 *  Mapping set up
 * ----------------
 *)

<* INLINE *>
PROCEDURE GetRoot() : T;
  (* Get root of mapping which is supplied by this interface *)

PROCEDURE AddMapping( start: T; path: TEXT; sid : ToT )
  RAISES { NameServer.Error };
  (* Add new mapping *)

(*
 *  Query the mapping 
 * -------------------
 *)

PROCEDURE GetComponent(
    delimiter               : CHAR;
    VAR (* INOUT *) path    : TEXT;
    VAR (* OUT *) component : TEXT; );
  (* Componentize path *)

PROCEDURE WalkTree( delimiter : CHAR; path : TEXT; node : T ) : T
  RAISES { NameServer.Error };
  (*
   * Walk the mapping tree,
   * given the current node and the path (relative or absolute),
   * expand the tree as necessary.
   *)

PROCEDURE WalkNode( name : TEXT; node : T; expand : BOOLEAN ) : T
  RAISES { NameServer.Error };
  (* Walk exactly one node in the tree, expand only if requested. *)

END SIDMap.

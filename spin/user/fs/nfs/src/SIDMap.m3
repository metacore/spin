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
 *)

MODULE SIDMap;

IMPORT Text, TextRefTbl, NameServer, SecurityManager;

REVEAL
  T = BaseT BRANDED Brand OBJECT
    name     : TEXT;            (* File/directory name    *)
    implicit : BOOLEAN;         (* Is the type implicit ? *)
    parent   : T;               (* Parent                 *)
    children : TextRefTbl.T;    (* Table of children      *)
  END;

VAR
  root          : T;     (* Root of namespace *)

<* INLINE *>
PROCEDURE GetRoot() : T =
  (* Get root of mapping *)
  BEGIN
    RETURN root;
  END GetRoot;

PROCEDURE AddMapping( start: T; path: TEXT; sid : ToT )
  RAISES { NameServer.Error } =
  (* Add new mapping *)
  VAR
    node : T;
  BEGIN
    node := WalkTree( pathDelimiter, path, start );
    LOCK node DO
      node.sid      := sid;
      node.implicit := FALSE;
      FixChildren( node );
    END;
  END AddMapping;

PROCEDURE GetComponent(
    delimiter               : CHAR;
    VAR (* INOUT *) path    : TEXT;
    VAR (* OUT *) component : TEXT; ) =
  (* Get first component off path, inspired by NameServer.GetComponent *)
  VAR
    stop : INTEGER;
    pos  : INTEGER;
    len  : CARDINAL;
  BEGIN
    IF path # NIL THEN
      pos := Text.FindChar( path, delimiter );
      IF pos = -1 THEN
        component := path;
        path      := NIL;
      ELSE
        len := Text.Length( path );
        
        (* Handle trailing '/' *)
        IF pos = len THEN
          DEC(len);
        END;

        (* Handle several '/' in a row *)
        stop := pos;
        INC(pos);
        WHILE pos < len AND Text.GetChar( path, pos ) = delimiter DO
          INC(pos);
        END;

        IF stop = 0 THEN
          (* Absolute path *)
          component := Text.FromChar(delimiter);
        ELSE
          (* Relative path *)
          component := Text.Sub( path, 0, stop );
        END;

        (* Remaining path *)
        IF pos >= len THEN
          path := NIL;
        ELSE
          path := Text.Sub( path, pos );
        END;
      END;
    ELSE
      component := NIL;
    END;
  END GetComponent;

PROCEDURE WalkTree( delimiter : CHAR; path : TEXT; node : T ) : T
  RAISES { NameServer.Error } =
  (*
   * Walk the mapping tree,
   * given the current node and the path (relative or absolute),
   * expand the tree as necessary.
   *)
  VAR
    current  : T;
    next     : REFANY;
    part1    : TEXT;
    part2    : TEXT;
  BEGIN
    IF path = NIL OR Text.Empty( path ) THEN RETURN node; END;
    part2 := path;
    
    IF node = NIL THEN
      current := root;
    ELSE
      current := node;
    END;

    REPEAT
      (* Get something to work on *)
      GetComponent( delimiter, part2, part1 );

      IF Text.Equal( part1, Text.FromChar(delimiter) ) THEN
        (* Start absolute path *)
        current := root;
      ELSIF Text.Equal( part1, nameSelf ) THEN
        (* Self *)
        (* Do nothing *)
      ELSIF Text.Equal( part1, nameParent ) THEN
        (* Parent *)
        (* Only need to lock when accessing children,
         * parent never changes after initialization.
         *)
        IF current.parent = NIL THEN
          RAISE NameServer.Error(NameServer.EC.InvalidName);
        END;
        current := current.parent;
      ELSE
        (* Relative path *)
        LOCK current DO
          IF current.children = NIL THEN
            (* Create table for children on demand *)
            current.children := NEW(TextRefTbl.Default).init();
          END;

          IF NOT current.children.get( part1, next ) THEN
            (* Expand tree on demand *)
            next := NEW( T,
                         sid       := current.sid,
                         name      := part1,
                         implicit  := TRUE,
                         parent    := current );
            EVAL current.children.put( part1, next );
          END;
        END; (* lock *)

        current := next;
      END;
    UNTIL part2 = NIL;

    RETURN current;
  END WalkTree;

PROCEDURE WalkNode( name : TEXT; node : T; expand : BOOLEAN ) : T
  RAISES { NameServer.Error } =
  (* Walk exactly one node in the tree, expand only if requested. *)
  VAR
    current : T;
    next    : REFANY := NIL;
  BEGIN
    IF name = NIL OR Text.Empty( name ) OR node = NIL THEN
      RETURN node;
    END;

    current := node;
    IF Text.Equal( name, nameSelf ) THEN
      (* Self *)
      next := current;
    ELSIF Text.Equal( name, nameParent ) THEN
      (* Parent *)
      IF current.parent = NIL THEN
        RAISE NameServer.Error(NameServer.EC.InvalidName);
      END;
      next := current.parent;
    ELSE
      (* Child *)
      LOCK current DO
        IF current.children = NIL AND expand THEN
          current.children := NEW(TextRefTbl.Default).init();
        END;

        IF current.children # NIL
          AND NOT current.children.get( name, next )
          AND expand THEN
          (* Expand tree on demand *)
          next := NEW( T,
                       sid      := current.sid,
                       name     := name,
                       implicit := TRUE,
                       parent   := current );
          EVAL current.children.put( name, next );
        END;
      END; (* lock *)
    END;

    RETURN NARROW(next, T);
  END WalkNode;

PROCEDURE FixChildren( node : T ) =
  (* Fix implicit types of children recursively up to first explicit type.
   * ACHTUNG: node must be locked when calling FixChildren.
   *)
  VAR
    iter  : TextRefTbl.Iterator;
    name  : TEXT;
    thing : REFANY;
  BEGIN
    IF node.children # NIL THEN
      iter := node.children.iterate();
      WHILE iter.next( name, thing ) DO
        WITH child = NARROW( thing, T ) DO
          LOCK child DO
            IF child.implicit THEN
              child.sid   := node.sid;
              FixChildren( child );
            END;
          END;
        END;
      END;
    END;
  END FixChildren;

(* Initialize the root node *)
BEGIN
  root := NEW( T,
               sid      := SecurityManager.TazDataSid,
               name     := "/",
               parent   := NIL,
               implicit := FALSE );
END SIDMap.

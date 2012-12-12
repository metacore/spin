(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 04-Nov-95  Yasushi Saito (yasushi) at the University of Washington
 *	created.
 *)

(* This modules converts m3tk parse tree into a proprietary format.
 Information is accumulated into Handle. *)
INTERFACE Pass1;

IMPORT ASTWalk, M3Context;
IMPORT Module, TypeTable;

TYPE
    Handle <: HandlePublic;
    HandlePublic = ASTWalk.Closure OBJECT
      typeTable: TypeTable.T;
      m: Module.T;
    END;

(* Creates a new handle suitable for passing to "ASTWalk.VisitNodes",
 or directly to "Node", from another walk. The "callback" method
 is set to "Node". *)
PROCEDURE NewHandle(i3Name, domName : TEXT; c: M3Context.T) : Handle;


END Pass1.

(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
 * HISTORY
 * 07-Nov-95  Yasushi Saito (yasushi) at the University of Washington
 *	created.
 *)
INTERFACE Msg;
IMPORT M3CSrcPos, M3AST_AS;


TYPE Pos = RECORD
  pos : M3CSrcPos.T;
  cu : M3AST_AS.Compilation_Unit;
END;

CONST POS_VOID = Pos{pos:=-1, cu:=NIL};
  
(*
 *  Display a message, and die
 *)
PROCEDURE Fatal(READONLY pos : Pos; a,b,c,d,e : TEXT := NIL);

(* Display an error message always *)
PROCEDURE Error(READONLY pos : Pos; a,b,c,d,e : TEXT := NIL);

(* Display an warning message always *)
PROCEDURE Warn(READONLY pos : Pos; a,b,c,d,e : TEXT := NIL);

(* Display a message only when debuggingP := TRUE. It is true when
 *  -Debug flag is set.
 *)
PROCEDURE Debug(READONLY pos : Pos; a,b,c,d,e : TEXT := NIL);

(* Same as debug *)
PROCEDURE Verbose(READONLY pos : Pos; a,b,c,d,e : TEXT := NIL);

VAR debuggingP : BOOLEAN;
    errorCode : INTEGER;
END Msg.

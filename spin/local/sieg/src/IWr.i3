(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 03-Nov-95  Yasushi Saito (yasushi) at the University of Washington
 *	created
 *)

(* Indented write. Some nice amount of spaces are added to the beginning
   of each line when they are Output. calling "indent()" increases the
   amount of the indentation, and "unindent()" does the opposite. 
 
   Otherwise, T behaves pretty like Wr.T.
 *)


INTERFACE IWr;

TYPE T <: Public;
    Public = OBJECT
    METHODS
      put(a,b,c,d,e,f,g,h : TEXT := NIL);
      f(fmt: TEXT; a, b, c, d, e: TEXT := NIL);
      (* Fmt.F, then put *)
      indent();
      unindent();
      close();
    END;
    
PROCEDURE OpenWrite(f : TEXT) : T;

END IWr.

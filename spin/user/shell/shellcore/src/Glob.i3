(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 08-May-96  Yasushi Saito (yasushi) at the University of Washington
 *	Added GetVariableList.
 * 05-May-96  Charles Garrett (garrett) at the University of Washington
 *	Aded the target variable which is set to either ALPHA_SPIN or
 *	 ALPHA_SPIN_PROF to indicate what target was used to build the
 *	 kernel. This variable is used by scripts to load extensions that
 *	 were built for the same target.
 *
 * 22-Feb-96  Charles Garrett (garrett) at the University of Washington
 *	Added variable "loadpath" which specifies the root directory to
 *	 search when loading extensions.
 *
 * 21-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	Created.
 *
 *)


(*
 *
 * Globbing support
 *
 *)

INTERFACE Glob;

IMPORT ParseParams, ASCII;

EXCEPTION Error;



CONST SubChar = '$';
      HomeChar = '~';		(* expands to tftphome *)
      MagicCharacters = ASCII.Set{SubChar,HomeChar};


TYPE T <: REFANY;			
	(* All globbing is done in a context. If the context is NIL, then
	   the globbing is done in the global context*)


PROCEDURE New(parent: T): T;	
	(* Create a new T, with parent T. Names in parent are inherited 
	   by child.. *)

PROCEDURE Freeze(ctxt: T): BOOLEAN;
	(* Freeze the named context. Disallow further writes.  No Unfreeze
	 * is supported.
         *)



PROCEDURE Substitute(ctxt: T  ; pp: ParseParams.T): ParseParams.T RAISES {Error};
   (*
    * Walk the param list replacing all occurrences of (MagicChar)
    * MC-prefixed strings
    * with their globbed values.
    *
    * An MC-prefixed string is
    *	any TEXT starting with $ or ~
    *   and ending with either the end of the string or a non ascii-numeric.
    *
    * For example, if $foo = bar, then
    *	$foo      --> bar
    *   $foo/wiz  --> bar
    *
    * presently, we don't handle wiz/$foo/bar, but we'll add that shortly.
    *
    * Raises Error if from is not defined.
    *)


PROCEDURE Lookup(ctxt: T  ; from: TEXT): TEXT RAISES {Error};
  (* 
   * Map variable named by from to its value.  The $ is assumed to have been
   * stripped.  Will walk parent, grandparent, etc.
   *
   * Raises Error if t is not defined.
   *)

PROCEDURE Translate (ctxt: T  ; t: TEXT): TEXT RAISES {Error};
  (* Convert text parsing MagicCharacters.  Error if magic --> failure *)

PROCEDURE GetVariable (ctxt: T  ; from: TEXT): TEXT;
  (* Like Lookup, only returns NIL if from not found. Will walk parent. *)

PROCEDURE SetVariable(ctxt: T  ; from, to: TEXT);
  (* 
   * Create mapping from 'from' to 'to'.  No $ should be present in from.
   * Installs mapping in specified context only. Will shadow parent if 
   * necessary. Does nothing if ctxt is not writable. (should fail more loudly)
   *)

PROCEDURE GetVariableList(ctxt: T  ) : REF ARRAY OF TEXT;
  (* Return the list of variable names defined in the shell. Will iterate
   * over parents. *)

PROCEDURE IsTrue(ctxt: T  ; t: TEXT): BOOLEAN;
  (* Returns TRUE iff t is defined and is defined as true. *)
  
END Glob.



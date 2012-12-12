(*
 * Copyright 1997, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 09-Apr-97  Tsutomu Owa (owa) at the University of Washington
 *	Change interface names so that these don't conflict w/
 *	those used by user/net.
 *
 * 18-Mar-97  Tsutomu Owa (owa) at the University of Washington
 *	Reverted If structure defined as urt/urtcore/src.
 *
 * 03-Feb-97  Tsutomu Owa (owa) at the University of Washington
 *	Created.
 *)

INTERFACE StcpUtils;
IMPORT StcpIf;			(* to print StcpIf.ifdevea *)
IMPORT StcpMbuf;		(* to print StcpMbuf header *)

(* ---------------------------------------------------------------- *)
(* Print ARARRAY OF CHAR according to passed length and base type *) 
(* len: how many chars to write *)
(* base: base of Fmt.Int() *)
(* needCR: flag to put CR at the end *)
PROCEDURE PrintArrayOfChar(READONLY data : ARRAY OF CHAR;
	len, base : CARDINAL ;
	needCR : BOOLEAN);

(* ---------------------------------------------------------------- *)
(* Print StcpIf.ifdevea *)
PROCEDURE PrintStcpIfdevea(ifdevea : StcpIf.ifdevea; needCR : BOOLEAN);

(* ---------------------------------------------------------------- *)
(* Print StcpMbuf *)
PROCEDURE PrintStcpMbuf(m : StcpMbuf.T);

END StcpUtils.

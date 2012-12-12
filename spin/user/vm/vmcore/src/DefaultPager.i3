(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 22-Oct-96  Yasushi Saito (yasushi) at the University of Washington
 *	Whisted, and added Stat. 
 *)

(* 
 The default implementation is bogus; it doesn't swap.
 The RealDefaultPager in vm/swap is the real one.
 *)

INTERFACE DefaultPager;
IMPORT Wr;
IMPORT PagerObject;

PROCEDURE Create(size : CARDINAL) : PagerObject.T;
(* "size" is the page number *)

PROCEDURE SwapOn(dev : REFANY);
(* "dev" is the device to swap on *)

PROCEDURE Stat(wr: Wr.T);
(* Print out some statistits information. *)
   
END DefaultPager.

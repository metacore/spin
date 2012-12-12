(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 3-Mar-98  David Becker at the University of Washington
 *	Added Equal and Hash for StrandSema tables
 *
 * 25-Mar-97  Yasushi Saito (yasushi) at the University of Washington
 *	Eliminated cpustate.
 *
 * 22-Nov-95  Emin Gun Sirer (egs) at the University of Washington
 *      Made strands partially opaque.
 *
 * 28-Sep-95  Brian Bershad (bershad) at the University of Washington
 *	Added RaiseException call.
 *
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created. The strands interface.
 *)
INTERFACE Strand;
IMPORT CPU, FastList, SpinException;

TYPE PublicT = FastList.T BRANDED OBJECT
   pri: PriorityT := defPriority;    (* priority *)
END;

TYPE T <: PublicT;
CONST Brand = "Strands";

TYPE PriorityT = [-2..10000];

CONST defPriority = 1000; (* default priority *)

PROCEDURE Stop(s: T) : BOOLEAN;
PROCEDURE Run(s: T);
PROCEDURE Block(s: T);
PROCEDURE Unblock(s: T);
PROCEDURE Yield();
FUNCTIONAL PROCEDURE GetCurrent(): T;
FUNCTIONAL PROCEDURE GetCurrentUserStrand(): T;
PROCEDURE AccessTrapFrame(s: T;
			  callback: PROCEDURE(VAR ss: CPU.SavedState));
(* Call the procedure "callback", passing the register contents of the
   user space thread "s" when it entered the kernel. *)
  
PROCEDURE RaiseException(s: T; reason: SpinException.ExceptionInfo);

(* for use with tables *)
PROCEDURE Equal(a, b: T) : BOOLEAN;
PROCEDURE Hash(a: T) : INTEGER;

END Strand.

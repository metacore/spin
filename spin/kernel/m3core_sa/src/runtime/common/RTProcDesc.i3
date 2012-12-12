(*
 * Safe interface to procedure information
 *)

INTERFACE RTProcDesc;

IMPORT RTIO;

TYPE
  ProcType <: ADDRESS;

PROCEDURE GetType (p: PROCANY): ProcType;
(* Find the type descriptor for a given procedure. *)

PROCEDURE NumberOfArgs (type: ProcType): INTEGER;
(* Return the number of arguments of a procedure. *)

PROCEDURE HasResult (type: ProcType): BOOLEAN;
(* Return the indication if a procedure returns result. *)

PROCEDURE ArgType (type: ProcType; n: INTEGER): INTEGER;
(* Return the type of n-th argument *)

PROCEDURE ArgMode (type: ProcType; n: INTEGER): INTEGER;
(* Return the type of n-th argument *)

PROCEDURE ResultType (type: ProcType): INTEGER;
(* Return the type of the result. *)

PROCEDURE IsFunctional (type: ProcType): BOOLEAN;
(* *)

PROCEDURE IsEphemeral (type: ProcType): BOOLEAN;
(* *)

PROCEDURE PrintProcType(type: ProcType; p: RTIO.SimplePutter := NIL);
(* Print the procedure type *)

END RTProcDesc.

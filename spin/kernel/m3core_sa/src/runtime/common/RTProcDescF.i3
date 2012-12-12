(*
 * Unsafe interface to procedure information
 *)

INTERFACE RTProcDescF;

IMPORT RTProcDesc;

TYPE
  T = UNTRACED REF RECORD
    proc    : PROCANY;              (* the procedure *)
    jtblPtr : UNTRACED REF PROCANY; (* pointer to the entry in jtbl *)
    type    : RTProcDesc.ProcType;  (* pointer to the descriptor of types *)
  END;    

PROCEDURE GetDesc(p: PROCANY): T;
(* Find the procedure descriptor. *)

PROCEDURE Legalize (p: ADDRESS);
(* Declare the argument to be a legal procedure with no known type *)

PROCEDURE Reset ();
(* Force reinitialization after the runtime has changed. *)

CONST
  Brand = "RTProcDescF";
  (* Duh! For the table generic. *)

END RTProcDescF.

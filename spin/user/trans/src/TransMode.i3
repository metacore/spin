(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 08-Nov-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 * html
 *)

(* This module defines various bits that controls the
   dehavior of a transaction. *)
INTERFACE TransMode;
IMPORT Word;

TYPE
  T = {Void, NoLogging, NoLocks, PageGrainLogging};
  (*
     <dl>

     <dd><strong>NonLogging</strong>
     <dt>When this flag is set, the atomicity of the transaction is
     not guaranteed. The transaction will not generate any log, and
     will not lock regions accessed.
     This should be used only when you want sweeping modification to
     the database, and you can redo the change manually in case of
     disaster. NoLogging assumes NoLocks.
     
     <dd><strong>NonLocks</strong>
     <dt>When this flag is set, the isolation of the transaction is
     not guaranteed. Isolation is not guaranteed.
     
     </dl>
     *)
  
  Set = SET OF T;

CONST Default = Set{};

PROCEDURE WordToT(v: Word.T): Set;
  (* Convert between the word representation and "Set" *)
  
END TransMode.


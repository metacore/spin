(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 12-Apr-97  Yasushi Saito (yasushi) at the University of Washington
 *	Eliminated all redundant fields.
 * 02-Nov-94  Stefan Savage (savage) at the University of Washington
 *	Created
 *)

INTERFACE TranslationRep;

IMPORT Translation, Word;

TYPE
  REVEAL Translation.T = BRANDED (* RECLAIMABLE *) OBJECT
    pagetableBase: Word.T; (* ptr to pmap. *)
  END;


END TranslationRep.




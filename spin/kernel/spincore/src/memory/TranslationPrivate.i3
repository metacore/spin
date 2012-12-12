(*
 * Copyright 1994, 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 29-May-96  Stefan Savage (savage) at the University of Washington
 *	Added GetKernel for Debugger
 *
 * 03-Aug-95  Stefan Savage (savage) at the University of Washington
 *	Created
 *
 *)

INTERFACE TranslationPrivate;

IMPORT Translation;

PROCEDURE Init(verbose: BOOLEAN);

PROCEDURE GetKernel(): Translation.T;
  
END TranslationPrivate.

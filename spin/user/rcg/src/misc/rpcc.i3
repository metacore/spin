(*
 *
 * Copyright 1996, 1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 *)

(*
 * HISTORY
 *)

UNSAFE INTERFACE rpcc;

IMPORT Word;

<* EXTERNAL ReadCycles *> PROCEDURE rpcc(): Word.T;

END rpcc.

(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)

(* Outputs spindle side i3 codes.
 *  
 *)

(*
 * HISTORY
 * 07-Nov-95  Yasushi Saito (yasushi) at the University of Washington
 *	created.
 *)
INTERFACE ExtensionI3;
IMPORT Module, IWr, Rd;

PROCEDURE Output(READONLY m : Module.T);

(* Copy contents of SRC into DEST until '@' is found at the
 *  beginning of line.
 * XXX we should move this thing into separate module.*)
PROCEDURE CopyTemplateTillMarker(src : Rd.T; dest : IWr.T);

(* Skeleton files. The real file name is templateFile & "i3" (or & "m3").
 *)
VAR templateFile := "/afs/cs/project/spin/build/merge/spin/user/sieg/extension_tmpl";

END ExtensionI3.

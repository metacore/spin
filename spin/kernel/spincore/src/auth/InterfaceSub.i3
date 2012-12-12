(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *)
INTERFACE InterfaceSub;
IMPORT Auth;
IMPORT SymbolEntry;

PROCEDURE Export(auth: Auth.T;
		 module: ADDRESS;
		 name: TEXT;
		 READONLY symbols: ARRAY OF SymbolEntry.E);

END InterfaceSub.

(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 18-Feb-97  Charles Garrett (garrett) at the University of Washington
 *	GetNextStaticRange returns range of addresses of the static
 *	 kernel.
 *
 * 02-Apr-95  Przemek Pardyak (pardy) at the University of Washington
 *	Find_unit_descriptors returns additionally unit link information
 *	and checks that there is such information for each Modula-3 
 *	unit descriptor.
 *
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Interface to coff modules.
 *
 *)
UNSAFE INTERFACE MachineLinkerExtern;
(* This module is unsafe because it declares externals *)
IMPORT Ctypes, RT0;
IMPORT SymbolEntry;
IMPORT Word;

FROM MachineLinker IMPORT T, SymEntry;

<* EXTERNAL *> PROCEDURE MCreate(bytes: ADDRESS) : T;

<* EXTERNAL *> PROCEDURE MDestroy(m: T);

<* EXTERNAL *> PROCEDURE Link(mapper, mappee: T);

<* EXTERNAL *> PROCEDURE Unlink(mapper, mappee: T);

<* EXTERNAL *> PROCEDURE FullyResolved(m: T) : BOOLEAN;

<* EXTERNAL *> PROCEDURE Generate(VAR syms: SymbolEntry.T; size: INTEGER): T;
<* EXTERNAL *> PROCEDURE M3SafeGenerate(VAR syms: ARRAY OF SymbolEntry.T): T;

<* EXTERNAL *> PROCEDURE ExtractModuleName(addr: SymEntry) : Ctypes.char_star;

<* EXTERNAL *> PROCEDURE ExtractValue(sym: SymEntry) : ADDRESS;

<* EXTERNAL *> PROCEDURE Find_unit_descriptors(ms: T;
                            VAR mods: UNTRACED REF ARRAY OF RT0.ModulePtr;
                            VAR info: UNTRACED REF ARRAY OF UNTRACED REF CHAR);

<* EXTERNAL *> PROCEDURE Free_unit_descriptors(
                            VAR mods: UNTRACED REF ARRAY OF RT0.ModulePtr;
                            VAR info: UNTRACED REF ARRAY OF UNTRACED REF CHAR);

<* EXTERNAL *> PROCEDURE Add_Fake_Symbol(m: T; s: Ctypes.char_star;
                                    a: ADDRESS; text: BOOLEAN := TRUE);

<* EXTERNAL *> PROCEDURE Symbol_findvalue(d: T; name:Ctypes.char_star;
                                          VAR found: BOOLEAN) : ADDRESS;

<* EXTERNAL *> PROCEDURE IsModula3(d: T) : BOOLEAN;

<* EXTERNAL *> PROCEDURE ShowSectionInformation (m: T);

<* EXTERNAL *> PROCEDURE TextInfo(m: T;
			          VAR start: Word.T;
				  VAR size: INTEGER): BOOLEAN;

<* EXTERNAL *> PROCEDURE Nlist(m: T);

(*
 * Debug is statically allocated and initialized in an exporting C module
 *)
<* EXTERNAL *> VAR debugModule: T;

<* EXTERNAL *> PROCEDURE Inside(m: T; addr: ADDRESS) : BOOLEAN;

<* EXTERNAL Module_GetNextRange *> 
PROCEDURE GetNextRange(m: T; VAR idx: INTEGER; 
                       VAR start: ADDRESS; VAR stop: ADDRESS) : BOOLEAN;

<* EXTERNAL Module_GetNextText *> 
PROCEDURE GetNextText(m: T; VAR idx: INTEGER; 
                      VAR start: ADDRESS; VAR stop: ADDRESS) : BOOLEAN;

<* EXTERNAL Module_GetNextStaticRange *> 
PROCEDURE GetNextStaticRange(VAR idx: INTEGER; 
                             VAR start: ADDRESS; VAR stop: ADDRESS) : BOOLEAN;

<* EXTERNAL *> PROCEDURE Strip(ms: T);

<* EXTERNAL *> PROCEDURE UnregisterClean(m: T);

END MachineLinkerExtern.

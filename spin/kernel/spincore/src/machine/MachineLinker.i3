(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * M3->C passthroughs for manipulating runtime Module structures.
 *
 * HISTORY
 * 18-Feb-97  Charles Garrett (garrett) at the University of Washington
 *	GetNextStaticRange returns range of addresses of the static
 *	 kernel.
 *
 * 10-Jul-96  Tian Fung Lim (tian) at the University of Washington
 *	 Added Strip to remove internal resolved relocations and internal
 *	 symbols.
 *
 * 02-Apr-95  Przemek Pardyak (pardy) at the University of Washington
 *	Find_unit_descriptors returns additionally unit link information
 *	and checks that there is such information for each Modula-3 
 *	unit descriptor.
 *
 * 09-Feb-96  Emin Gun Sirer (egs) at the University of Washington
 *	Added IsModula3, which allows clients to mark non modula-3 code
 *	as a non-preemptible region.
 *
 * 06-Nov-95  Emin Gun Sirer (egs) at the University of Washington
 *      Cleaned up Find_unit_descriptors.
 *
 * 31-Oct-95  Charlie Garrett (garrett) at the University of Washington
 *      It gets uglier as we move externals to the ModuleExtern.i3 file.
 *
 * 16-Aug-95  Przemek Pardyak (pardy) at the University of Washington
 *	Changed Find_unit_descriptors to find multiple M3 units 
 *	in one object file.
 *
 * 25-Jul-95  Brian Bershad (bershad) at the University of Washington
 *	removed Core services.
 *
 * 07-Jul-95  Emin Gun Sirer (egs) at the University of Washington
 *	Added a special debug domain that identifies unresolved symbols
 *      when linked against.
 *
 *
 * 30-Jun-95 Przemyslaw Pardyak (pardy) at the University of Washington
 *      Changed Symbol_findlinkinfo to Symbol_find_unit_descriptor,
 *      it returns the symbol for the Modula-3 module/interface descriptor.
 * 
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Interface to coff modules.
 *
 *)
INTERFACE MachineLinker;

IMPORT Ctypes, RT0;
IMPORT SymbolEntry;
IMPORT Word;

(* Module.T is a pointer to the code and symbols defined in the coff file. *)
(* It is allocated and maintained by the C component *)
TYPE T = UNTRACED REF RECORD END;
TYPE SymEntry = UNTRACED REF SymbolEntry.T;

PROCEDURE MCreate(bytes: ADDRESS) : T;
PROCEDURE MDestroy(m: T);

PROCEDURE Link(mapper, mappee: T);
PROCEDURE Unlink(mapper, mappee: T);

PROCEDURE FullyResolved(m: T) : BOOLEAN;

PROCEDURE Generate(VAR syms: SymbolEntry.T; size: INTEGER): T;
PROCEDURE M3SafeGenerate(VAR syms: ARRAY OF SymbolEntry.T): T;

PROCEDURE ExtractModuleName(addr: SymEntry) : Ctypes.char_star;

PROCEDURE ExtractValue(sym: SymEntry) : ADDRESS;

PROCEDURE Find_unit_descriptors(ms: T;
                           VAR mods: UNTRACED REF ARRAY OF RT0.ModulePtr;
                           VAR info: UNTRACED REF ARRAY OF UNTRACED REF CHAR);

PROCEDURE Free_unit_descriptors(
                           VAR mods: UNTRACED REF ARRAY OF RT0.ModulePtr;
                           VAR info: UNTRACED REF ARRAY OF UNTRACED REF CHAR);

PROCEDURE Add_Fake_Symbol(m: T; s: Ctypes.char_star;
                          a: ADDRESS; text: BOOLEAN := TRUE);

PROCEDURE Symbol_findvalue(d: T; name:Ctypes.char_star;
                           VAR found: BOOLEAN) : ADDRESS;

PROCEDURE IsModula3(d: T) : BOOLEAN;

PROCEDURE ShowSectionInformation (m: T);

PROCEDURE TextInfo(m: T; VAR start: Word.T; VAR size: INTEGER): BOOLEAN;

PROCEDURE Nlist(m: T);

PROCEDURE GetDebugModule(): T;
PROCEDURE SetDebugModule(m: T);

PROCEDURE GetNextRange(m: T; VAR idx: INTEGER; 
                       VAR start: ADDRESS; VAR stop: ADDRESS) : BOOLEAN;

PROCEDURE GetNextText(m: T; VAR idx: INTEGER; 
                      VAR start: ADDRESS; VAR stop: ADDRESS) : BOOLEAN;

PROCEDURE GetNextStaticRange(VAR idx: INTEGER; 
                       VAR start: ADDRESS; VAR stop: ADDRESS) : BOOLEAN;

PROCEDURE UnregisterClean(m: T);

PROCEDURE Inside(m: T; addr: ADDRESS) : BOOLEAN;
PROCEDURE Strip(m: T);

END MachineLinker.

(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * Pass throughs to the real implementation.
 *
 * HISTORY
 * 18-Feb-97  Charles Garrett (garrett) at the University of Washington
 *	GetNextStaticRange returns range of addresses of the static
 *	 kernel.
 *
 * 10-Jul-96  Tian Fung Lim (tian) at the University of Washington
 *	Added Strip to remove internal resolved relocations and internal
 *	symbols.
 *
 * 02-Apr-95  Przemek Pardyak (pardy) at the University of Washington
 *	Find_unit_descriptors returns additionally unit link information
 *	and checks that there is such information for each Modula-3 
 *	unit descriptor.
 *
 * 31-Oct-95  Charlie Garrett (garrett) at the University of Washington
 *      Created. Pass throughs to the real implementation.
 *
 *)
UNSAFE MODULE MachineLinker;
IMPORT MachineLinkerExtern;
IMPORT Ctypes, RT0;
IMPORT SymbolEntry;
IMPORT Word;

PROCEDURE MCreate(bytes: ADDRESS) : T =
  BEGIN
    RETURN MachineLinkerExtern.MCreate(bytes);
  END MCreate;

PROCEDURE MDestroy(m: T) =
  BEGIN
    MachineLinkerExtern.MDestroy(m);
  END MDestroy; 

PROCEDURE Link(mapper, mappee: T) =
  BEGIN
    MachineLinkerExtern.Link(mapper, mappee);
  END Link;

PROCEDURE Unlink(mapper, mappee: T) =
  BEGIN
    MachineLinkerExtern.Unlink(mapper, mappee);
  END Unlink;

PROCEDURE FullyResolved(m: T) : BOOLEAN =
  BEGIN
    RETURN MachineLinkerExtern.FullyResolved(m);
  END FullyResolved; 

PROCEDURE Generate(VAR syms: SymbolEntry.T; size: INTEGER): T =
  BEGIN
    RETURN MachineLinkerExtern.Generate(syms, size);
  END Generate;

PROCEDURE M3SafeGenerate(VAR syms: ARRAY OF SymbolEntry.T): T =
  BEGIN
    RETURN MachineLinkerExtern.M3SafeGenerate(syms);
  END M3SafeGenerate;

PROCEDURE ExtractModuleName(addr: SymEntry) : Ctypes.char_star =
  BEGIN
    RETURN MachineLinkerExtern.ExtractModuleName(addr);
  END ExtractModuleName;
    
PROCEDURE ExtractValue(sym: SymEntry) : ADDRESS =
  BEGIN
    RETURN MachineLinkerExtern.ExtractValue(sym);
  END ExtractValue;
 
PROCEDURE IsModula3(d: T) : BOOLEAN =
  BEGIN
    RETURN MachineLinkerExtern.IsModula3(d);
  END IsModula3;

PROCEDURE Find_unit_descriptors(ms: T;
                           VAR mods: UNTRACED REF ARRAY OF RT0.ModulePtr;
                           VAR info: UNTRACED REF ARRAY OF UNTRACED REF CHAR) =
  BEGIN
    MachineLinkerExtern.Find_unit_descriptors(ms, mods, info);
  END Find_unit_descriptors;

PROCEDURE Free_unit_descriptors(
                           VAR mods: UNTRACED REF ARRAY OF RT0.ModulePtr;
                           VAR info: UNTRACED REF ARRAY OF UNTRACED REF CHAR) =
  BEGIN
    MachineLinkerExtern.Free_unit_descriptors(mods, info);
  END Free_unit_descriptors;

PROCEDURE Add_Fake_Symbol(m: T; s: Ctypes.char_star;
  a: ADDRESS; text: BOOLEAN := TRUE) =
  BEGIN
    MachineLinkerExtern.Add_Fake_Symbol(m, s, a, text);
  END Add_Fake_Symbol; 

PROCEDURE Symbol_findvalue(d: T; name:Ctypes.char_star; VAR found: BOOLEAN) : ADDRESS =
  BEGIN
    RETURN MachineLinkerExtern.Symbol_findvalue(d, name, found);
  END Symbol_findvalue;

PROCEDURE ShowSectionInformation (m: T) =
  BEGIN
    MachineLinkerExtern.ShowSectionInformation(m);
  END ShowSectionInformation; 

PROCEDURE TextInfo(m: T; VAR start: Word.T; VAR size: INTEGER): BOOLEAN =
  BEGIN
    RETURN MachineLinkerExtern.TextInfo(m, start, size);
  END TextInfo;

PROCEDURE Nlist(m: T) =
  BEGIN
    MachineLinkerExtern.Nlist(m);
  END Nlist;

PROCEDURE GetDebugModule(): T =
  BEGIN
    RETURN MachineLinkerExtern.debugModule;
  END GetDebugModule;

PROCEDURE SetDebugModule(t: T) =
  BEGIN
    MachineLinkerExtern.debugModule := t;
  END SetDebugModule;

PROCEDURE Inside(m: T; addr: ADDRESS) : BOOLEAN =
  BEGIN
    RETURN MachineLinkerExtern.Inside(m, addr);
  END Inside;

PROCEDURE GetNextRange(m: T; VAR idx: INTEGER; 
                       VAR start: ADDRESS; VAR stop: ADDRESS) : BOOLEAN =
  BEGIN
    RETURN MachineLinkerExtern.GetNextRange(m, idx, start, stop);
  END GetNextRange;

PROCEDURE GetNextText(m: T; VAR idx: INTEGER; 
                      VAR start: ADDRESS; VAR stop: ADDRESS) : BOOLEAN =
  BEGIN
    RETURN MachineLinkerExtern.GetNextText(m, idx, start, stop);
  END GetNextText;

PROCEDURE GetNextStaticRange(VAR idx: INTEGER; 
                             VAR start: ADDRESS; VAR stop: ADDRESS) : BOOLEAN =
  BEGIN
    RETURN MachineLinkerExtern.GetNextStaticRange(idx, start, stop);
  END GetNextStaticRange;

PROCEDURE UnregisterClean(m: T) =
  BEGIN
    MachineLinkerExtern.UnregisterClean(m);
  END UnregisterClean; 

PROCEDURE Strip (m: T) =
  BEGIN
    MachineLinkerExtern.Strip(m);
  END Strip;

BEGIN
END MachineLinker.

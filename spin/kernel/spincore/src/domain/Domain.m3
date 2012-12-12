(*
 * Copyright 1995,1996,1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)

(* 
 * HISTORY
 * 29-Jul-97  Wilson Hsieh (whsieh) at the University of Washington
 *	change Strip to call MxCheck.Strip to strip away m3linker stuff
 *      put call to CheckImportExport back in
 *
 * 24-Jul-97  Tian Fung Lim (tian) at the University of Washington
 *	Commented out call to CheckImportExport.  This compromises safety
 *	and should be fixed (probably by increasing traced heap size or
 *	by fixing webserver to use appropriately-sized buffers)
 *
 * 10-Jun-97  Wilson Hsieh (whsieh) at the University of Washington
 *	check import/export
 *      add code to keep track of mappings from modules to M3 link units
 *      when static domains are created, rewrite the link units to point
 *        back to the domain
 *
 * 31-May-97  David Becker at the University of Washington
 *	Moved cycle counter functions to CPU interface
 *
 * 24-Feb-97  Marc Fiuczynski (mef) at the University of Washington
 *	Made TrackStrand "mostly" conditional.
 *
 * 18-Feb-97  Charles Garrett (garrett) at the University of Washington
 *	AddressToDomain and TypeDefnToDomain allow you to find out what
 *	 domain contains an address or type definition. Currently
 *	 addresses in the static part of the kernel all map to
 *	 SpinPublic. Type definitions in the static kernel will return
 *	 the domain which exports that type.
 *
 * 11-Feb-97  Przemek Pardyak (pardy) at the University of Washington
 *	Added some code for domain destruction (still disabled). 
 *
 * 22-Nov-96  becker at the University of Washington
 *	Added TrackDomain
 *
 * 14-Nov-96  Przemek Pardyak (pardy) at the University of Washington
 *	Moved nextID from Domain.m3 to DomainPrivate.i3.
 *
 * 08-Aug-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed CreateFromSymbolsArray to convert a SymbolEntry.E into a
 *	SymbolEntry.T.  The only difference between a SymbolEntry.E and
 *	and SymbolEntry.T is that the SE_name field is a char_star in the
 *	latter and a TEXT in the former.
 *
 * 14-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	New interface to interrupt management.
 *
 * 22-Jul-96  Tian Fung Lim (tian) at the University of Washington
 *	Removed InitializeNotM3.
 *
 * 15-Jul-96  Przemek Pardyak (pardy) at the University of Washington
 *	Domain destruction, the "low-road" approach: destroy a domain
 *	when there are no outstanding pointers to it.  Remove some pointers
 *	in the process, e.g., uninstall event handlers.
 *
 * 12-Jul-96  Tian Fung Lim (tian) at the University of Washington
 *	Added Strip and StripAllRelocations.  Strip is currently called
 *      from Initialize. 
 *
 * 18-Apr-96  Przemek Pardyak (pardy) at the University of Washington
 *      Added verbose mode to print information about progress of domain
 *	creation, linking, checking and initialization. Added a flag
 *	to turn on/off interface checking.
 *
 * 07-Apr-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added support for checking Modula-3 consistency.  Added global
 *	list of all active domains. 
 *
 * 22-Mar-96  Marc Fiuczynski (mef) at the University of Washington
 *	Updated CreateFromSymbolsArray not to waste untraced memory.
 *	Also, user cannot specify size anymore.  It is not retrieved from
 *	the size of the passed in array.
 *
 * 09-Feb-96  Emin Gun Sirer (egs) at the University of Washington
 *	Added IsModula3, which allows clients to mark non modula-3 code
 *	as a non-preemptible region.
 *      Split off DomainRep.
 *
 * 06-Nov-95  Emin Gun Sirer (egs) at the University of Washington
 *      Defuncted Link and LinkOne, since resolve already did the same
 *	thing. Got rid of domain lists, use a container domain.
 *      Fixed M3 module collection to work with nested domains.
 *      Changed the representation of empty domains to have NIL 
 *	coff modules. Added FullyResolved.
 *
 * 18-Oct-95  Emin Gun Sirer (egs) at the University of Washington
 *	Added checks for NIL arguments to avoid a crash with locks held.
 *
 * 06-Oct-95  Marc Fiuczynski (mef) at the University of Washington
 *	Added CreateFromSymbolsArray() interface.
 *
 * 28-Sep-95  Brian Bershad (bershad) at the University of Washington
 *	Apply returns BOOLEAN and not REFANY.
 *
 * 16-Aug-95  Przemek Pardyak (pardy) at the University of Washington
 *	Multiple M3 units in one object file.
 *
 * 12-Aug-95  Przemek Pardyak (pardy) at the University of Washington
 *	Added locking to runtime update
 *
 * 10-Aug-95  Emin Gun Sirer (egs) at the University of Washington
 *	Cleaned up update and collect modules, added dlist, fixed apply.
 *
 * 25-Jul-95  Brian Bershad (bershad) at the University of Washington
 *	Removed core interfaces; install debug into nameserver. Added 
 *	broken apply.
 *
 * 15-Jul-95  Przemek Pardyak (pardy) at the University of Washington
 *      Dynamic linking of Modula-3 code
 *
 * 12-Jul-95  Emin Gun Sirer (egs) at the University of Washington
 *	Made a monitor.
 *
 * 07-Jul-95  Emin Gun Sirer (egs) at the University of Washington
 *	Added a special debug domain that identifies unresolved symbols
 *      when linked against.
 *
 * 05-Jul-95  Emin Gun Sirer (egs) at the University of Washington
 *	Made clients of create safe by making it use open arrays.
 *
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Implementation of SPIN protection domains.
 *
 *)

UNSAFE
MODULE Domain EXPORTS Domain, DomainRep, DomainPrivate;
(* The "Domain" module is unsafe because it must use casts to change
   view of the typespace for the runtime.  It also returns addresses and
   turns them into typed procedure entry points.  *)

IMPORT MachineLinker;
IMPORT SymbolEntry, ThreadExtra;
IMPORT CPU, CPUPrivate;
IMPORT RT0, RTLinker, RT0u, RTCode, RTType;
IMPORT M3ID, Mx, MxIn, M3FP, MxVS;
IMPORT Range, RangeDomainSplayTree;
IMPORT DomainChecks;
IMPORT Text, IO, Wr;
IMPORT Fmt, M3toC, Word, Auth, Ctypes;
IMPORT DebugOption, TrackDomain, TrackStrand, TrackExtension; <*NOWARN*>
IMPORT IntAuthTbl, AddrUnitTbl;
IMPORT RTProcedureSRC;
IMPORT MxCheck;

(* Types of M3 runtime data structures used for dynamic linking *)
TYPE
  RuntimeModulePtr = UNTRACED REF ARRAY OF RT0.ModulePtr;
  RuntimeLinkInfoPtr = UNTRACED REF ARRAY OF UNTRACED REF CHAR;

(* This module is arranged as a monitor. It is non-blocking.
   If lack of parallelism bites us, we can move to reader/writer
   locks on individual domains, even though they seem more expensive. *)
VAR
  DomainMonitor: MUTEX;
  activeDomains: T := NIL;             (* all domains in the system *)
  lastActive: T := NIL;
  rangeMap: RangeDomainSplayTree.T := NIL;
  RangeMapMonitor: MUTEX;
  subdomainTotal, bytesTotal: INTEGER; (* memory stats for domains *)

(* Temporary map that keeps track of unit-domain relationship for the static
   code, unnecessary and destroyed after the domains describing this code are
   initialized *) 
VAR
  moduleUnitMap : AddrUnitTbl.T;

(* Set to TRUE after the domains describing static code have been
   initialized *)
VAR
  staticsDone   := FALSE;

(*-------------------------------------------------------------- creation ---*)

PROCEDURE GetT(): T =
  VAR t: T;
  BEGIN
    INC(subdomainTotal);
    t := NEW(T);
    IF activeDomains = NIL THEN
      activeDomains := t;
      lastActive := t;
    ELSE
      lastActive.next := t;
      lastActive := t;
    END;
    t.id := nextID;
    INC(nextID);
    t.authorizer := NIL;
    TrackDomain.Create(t);
    RETURN t;
  END GetT;

(* Currently, we copy the contents of the buffer to space we allocate
   ourselves. When reclaimable works, we will just reclaim the buffer,
   thereby insuring that the user cannot change the code anymore and
   thus avoid having to copy its contents.  *)
PROCEDURE Create(name: TEXT;
                 code: (* RECLAIMABLE *) REF ARRAY OF CHAR;
                 collectable: BOOLEAN := TRUE) : T =
  VAR d: T;
  BEGIN
    IF code # NIL THEN
      INC(bytesTotal,NUMBER(code^));
      d := CreateFromAddress(name, ADR(code[FIRST(code^)]), collectable);
    ELSE
      (* Create an empty domain *)
      d := GetT();  
      IF name # NIL THEN d.name := (* "#" & *) name; END;
      (* IO.Put("> Create: " & Fmt.Int(d.id) & " " & d.name & "\n");*)
      d.collectable := collectable;
      d.dynamic := TRUE;
    END;
    RETURN d;
  END Create;

PROCEDURE NewCreate(name: TEXT;
                 READONLY code: ARRAY OF BITS 8 FOR [0..255];
                 collectable: BOOLEAN := TRUE) : T =
  BEGIN
    INC(bytesTotal,NUMBER(code));
    RETURN CreateFromAddress(name, ADR(code[FIRST(code)]), collectable);
  END NewCreate;

PROCEDURE CreateFromAddress(name: TEXT;
			    code: ADDRESS; collectable: BOOLEAN := TRUE) : T =
  VAR d: T;
  BEGIN
    d := GetT();
    d.collectable := collectable;
    IF name = NIL THEN d.name := "Static"; ELSE  d.name := name; END;
    d.trusted := TRUE;
    LOCK DomainMonitor DO 
      TrackDomain.SetNewborn(d);
      d.module := MachineLinker.MCreate(code);
      d.dynamic := TRUE;
      TrackDomain.UnsetNewborn(d);
    END;
    RETURN d;
  END CreateFromAddress;

PROCEDURE CreateFromSymbols(name: TEXT; syms: ADDRESS; size: INTEGER): T =
  VAR d: T;
  BEGIN
    d := CreateFromAddress(name, NIL, FALSE);
    WITH symbols = LOOPHOLE(syms,UNTRACED REF SymbolEntry.T) DO
      TrackDomain.SetNewborn(d);
      d.module := MachineLinker.Generate(symbols^, size);
      TrackDomain.UnsetNewborn(d);
    END;
    d.dynamic := FALSE;
    RETURN d;
  END CreateFromSymbols;

(* called through Interface.mg and UnsafeInterface.mg *)
PROCEDURE CreateFromSymbolsArray(module: ADDRESS;
                                 name: TEXT; 
                                 READONLY symbols: ARRAY OF SymbolEntry.E): T =
  VAR d: T;
  BEGIN
    d := CreateFromAddress(name, NIL, FALSE);

    (* FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME -mef *)
    VAR ptr: T := activeDomains;
        dom: T := NIL;
    BEGIN
      WHILE ptr # NIL DO
        IF MachineLinker.Inside(ptr.module,
                                LOOPHOLE(module, RTCode.Module)) THEN
          IF dom # NIL THEN
            IO.Put("ERROR >> Domain.CreateFromSymbolsArray: found twice\n");
          END;
          dom := ptr;
        END;
        ptr := ptr.next;
      END;
      IF dom = NIL THEN
        (* FIXME ???
        IO.Put("ERROR >> Domain.CreateFromSymbolsArray: not found\n");
        *)
      ELSE
        WHILE dom.parent # NIL DO
          dom := dom.parent;
        END;
        d.parent := dom;
        AddLinked(d, dom);
      END;
    END;
    (* FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME -mef *)

    (* We remember each of the exported interfaces so that we can
       determine which types are exported by SpinPublic and SpinTrusted.
       This copy could be eliminated if we change the signature to
       pass a REF to the array of symbols, instead of passing it READONLY *)
    d.exported_intf := NEW(REF ARRAY OF SymbolEntry.E, NUMBER(symbols));
    d.exported_intf^ := symbols;

    (* Static domains require special handling because they come in as
       a set of symbols and not a range in memory.  Therefore, for extensions
       we deal with ranges directly and for static domains we traverse the
       symbols and fix up the M3 link info *)
    IF staticsDone THEN
      LOCK RangeMapMonitor DO
        VAR
          idx := 0;
          start, stop: ADDRESS;
        BEGIN
          WHILE MachineLinker.GetNextStaticRange(idx, start, stop) DO
            EVAL rangeMap.insert(Range.T{start, stop}, d);
          END;
        END;
      END;
    ELSE
      FixStaticM3LinkInfo (d, symbols);
    END;

    (* XXX this goes away once machinelinker can just
       work with SymbolEntry.E types directly. (mef) *)
    VAR 
      syms : REF ARRAY OF SymbolEntry.T;
    BEGIN
      syms := NEW(REF ARRAY OF SymbolEntry.T,NUMBER(symbols));
      FOR i := FIRST(symbols) TO LAST(symbols) DO
        syms[i].SE_ptr     := symbols[i].SE_ptr;
        syms[i].SE_name    := M3toC.CopyTtoS(symbols[i].SE_name);
        syms[i].SE_nvalue  := symbols[i].SE_nvalue;
        syms[i].SE_nsclass := symbols[i].SE_nsclass;
        syms[i].SE_module  := symbols[i].SE_module;
      END;
      TrackDomain.SetNewborn(d);
      d.module := MachineLinker.M3SafeGenerate(syms^);
      TrackDomain.UnsetNewborn(d);
      FOR i := FIRST(symbols) TO LAST(symbols) DO
        syms[i].SE_ptr     := NIL;
        syms[i].SE_name    := NIL;
        syms[i].SE_nvalue  := NIL;
        syms[i].SE_nsclass := 0;
        syms[i].SE_module  := 0;
      END;
    END;
    d.dynamic := FALSE;
    RETURN d;
  END CreateFromSymbolsArray;

PROCEDURE CreateFromModule(name: TEXT; m: MachineLinker.T) : T =
  VAR d: T;
  BEGIN
    d := CreateFromAddress(name, NIL, FALSE);
    d.module := m;
    d.dynamic := FALSE;
    RETURN d;
  END CreateFromModule;

(*----------------------------------------------------------- collections ---*)

(* Create a new alias for a domain *)
PROCEDURE Dup(domain: T) : T = 
  VAR d: T;
  BEGIN
    d := GetT();
    d^ := domain^;
    RETURN d;
  END Dup;

(* Add a new subdomain to list of children *)
PROCEDURE Add(holder, subdomain: T) =
  VAR newsubdlist: DList;
  BEGIN
    IF holder = NIL OR subdomain = NIL THEN RETURN END;
    newsubdlist := NEW(DList);
    LOCK DomainMonitor DO
      newsubdlist.domain := subdomain;
      newsubdlist.next := holder.subdlist;
      holder.subdlist := newsubdlist;
      IF subdomain.subdlist # NIL THEN
        IO.PutError("Nested subdomains\n");
        (* FIXME: propagate parent information to subdomains *)
      END;
      IF subdomain.parent # NIL THEN
        IO.PutError("Domain has more than one parent\n");
      END;
      WHILE subdomain.parent # NIL DO
        subdomain := subdomain.parent;
      END;
      subdomain.parent := holder;
    END;
  END Add;

PROCEDURE Delete(holder, subdomain: T) =
  VAR d, p: DList;
  BEGIN
    IF holder = NIL OR subdomain = NIL THEN RETURN END;
    LOCK DomainMonitor DO
      IF holder.subdlist # NIL AND
        holder.subdlist.domain = subdomain THEN
        holder.subdlist := holder.subdlist.next;
        RETURN;
      END;
      d := holder.subdlist;
      p := d;
      WHILE d # NIL DO
        IF d.domain = subdomain THEN
          p.next := d.next;
          RETURN;
        END;
        p := d;
      END;
    END;
  END Delete;

(*--------------------------------------------------------------- linking ---*)

(* Do the resolution between modules *)
PROCEDURE Resolve_2(mapper, mappee: T) =
  VAR subd : DList;
  BEGIN
    AddLinked(mapper, mappee);
    MachineLinker.Link(mapper.module, mappee.module);
    subd := mappee.subdlist;
    WHILE subd # NIL DO
      Resolve_2(mapper, subd.domain);
      subd := subd.next;
    END;
  END Resolve_2;

(* Descend recursively down the domain tree *)
PROCEDURE Resolve_1(mapper, mappee: T) =
  VAR subd: DList;
  BEGIN
    IF NOT mapper.modify OR NOT mappee.import THEN
      RETURN;
    END;
    IF mapper # NIL THEN
      Resolve_2(mapper, mappee);
      subd := mapper.subdlist;
      WHILE subd # NIL DO
        Resolve_1(subd.domain, mappee);
        subd := subd.next;
      END;
    END;
  END Resolve_1;

(* Patch unresolved symbols in mapper to point to mapped module *)
PROCEDURE Resolve(mapper, mappee: T) =
  BEGIN
    IF mapper = NIL OR mappee = NIL THEN
      RETURN;
    END;
    LOCK DomainMonitor DO
      TrackDomain.SetNewborn(mapper);
      Resolve_1(mapper, mappee);
      TrackDomain.UnsetNewborn(mapper);
    END;
  END Resolve;

(* Do a module vs. module unresolve operation *)
PROCEDURE Unresolve_2(mapper, mappee: T) =
  VAR subd: DList;
  BEGIN
    IF mappee = NIL THEN RETURN END;
    IF NOT mapper.modify OR NOT mappee.import THEN
      RETURN;
    END;
    MachineLinker.Unlink(mapper.module, mappee.module);
    subd := mappee.subdlist;
    WHILE subd # NIL DO
      Unresolve_2(mapper, subd.domain);
      subd := subd.next;
    END;
  END Unresolve_2;

(* Recursively descend the domain tree *)
PROCEDURE Unresolve_1(mapper, mappee: T) =
  VAR subd: DList;
  BEGIN
    IF NOT mapper.modify OR NOT mappee.import THEN
      (* FIX raise some exception here *) 
      RETURN;
    END;
    IF mapper # NIL THEN
      Unresolve_2(mapper, mappee);
      subd := mapper.subdlist;
      WHILE subd # NIL DO
        Unresolve_1(subd.domain, mappee);
        subd := subd.next;
      END;
    END;
  END Unresolve_1;

(* Undo a previous resolution *)
PROCEDURE Unresolve(mapper, mappee: T) =
  BEGIN
    IF mapper = NIL OR mappee = NIL THEN
      RETURN;
    END;
    LOCK DomainMonitor DO
      Unresolve_1(mapper, mappee);
    END;
  END Unresolve;

PROCEDURE GetSymbolValue_1(domain: T; name: TEXT; VAR found: BOOLEAN) : ADDRESS =
  VAR res: ADDRESS;
      subd: DList;
  BEGIN
    res := MachineLinker.Symbol_findvalue(domain.module, 
                                          M3toC.CopyTtoS(name), found);
    IF found THEN
      RETURN res;
    END;
    subd := domain.subdlist;
    WHILE subd # NIL DO
      res := GetSymbolValue_1(subd.domain, name, found);
      IF found THEN
        RETURN res;
      END;
      subd := subd.next;
    END;
    RETURN NIL
  END GetSymbolValue_1;

PROCEDURE GetSymbolValue(domain: T; name: TEXT; VAR found: BOOLEAN) : ADDRESS =
  BEGIN
    found := FALSE;
    IF domain = NIL THEN RETURN NIL; END;
    LOCK DomainMonitor DO
      RETURN GetSymbolValue_1(domain, name, found);
    END;
  END GetSymbolValue;

PROCEDURE FullyResolved_1(domain: T) : BOOLEAN =
  VAR subd: DList;
      res : BOOLEAN;
  BEGIN
    res := MachineLinker.FullyResolved(domain.module);
    subd := domain.subdlist;
    WHILE subd # NIL DO
      res := res AND FullyResolved_1(subd.domain);
      subd := subd.next;
    END;
    RETURN res;
  END FullyResolved_1;

PROCEDURE FullyResolved(domain: T) : BOOLEAN =
  BEGIN
    LOCK DomainMonitor DO
       RETURN FullyResolved_1(domain);
    END;
  END FullyResolved;

(*---------------------------------------------------------------- rights ---*)

PROCEDURE SetRights(domain: T; modify: BOOLEAN; import: BOOLEAN) =
  BEGIN
    IF domain = NIL THEN RETURN END;
    LOCK DomainMonitor DO
      (* XXX this should be a strict restriction, unless an ownership *)
      (* capability is passed in. *)
      domain.modify := modify;
      domain.import := import;
    END;
  END SetRights; 

PROCEDURE GetRights(domain: T; VAR modify: BOOLEAN; VAR import: BOOLEAN) =
  BEGIN
    IF domain = NIL THEN RETURN; END;
    LOCK DomainMonitor DO
      modify := domain.modify;
      import := domain.import;
    END;
  END GetRights; 

(*--------------------------------------------------------- authorization ---*)

PROCEDURE PreventExportOfInterfaces (domain: T) =
  (* do not allow any of the interfaces in the domain to be
     implemented (EXPORTed) by any modules *)
  BEGIN
    IF domain = NIL THEN RETURN; END;
    LOCK DomainMonitor DO
      domain.interfacesCanBeImplemented := FALSE;
    END;
  END PreventExportOfInterfaces;

PROCEDURE AddAuthorizer(domain: T; auth: Auth.T) =
  BEGIN
    IF domain = NIL THEN RETURN END;
    LOCK DomainMonitor DO
      domain.authorizer := auth;
    END;
  END AddAuthorizer; 

PROCEDURE AddTypeAuthorizer(domain: T; typename: TEXT; auth: Auth.T) =
  (* Local procedure walks over domain hierarchy and looks for the 
     named type. *)
  PROCEDURE FindTypeCode(d: T; tn: TEXT): RT0.Typecode =
    VAR
      subd : DList := d.subdlist;
      modules : RuntimeModulePtr;
      linkInfo: RuntimeLinkInfoPtr;
      typeDefn : RT0.TypeDefn;
    BEGIN
      IF subd # NIL THEN
        WHILE subd # NIL DO
          WITH tc = FindTypeCode(subd.domain, tn) DO
            IF tc # RTType.NoSuchType THEN
              RETURN tc;
            END;
          END;
          subd := subd.next;
        END;
      ELSE
        IO.Put("Found leaf " & d.name & "\n");
        (* We have a leaf domain, iterate over its types. *)
        CollectM3Modules(d, modules, linkInfo);

        FOR i := FIRST(modules^) TO LAST(modules^) DO
          typeDefn := LOOPHOLE(modules[i].type_cells, RT0.TypeDefn);
          WHILE (typeDefn # NIL) DO 
            (* Get the string name of the type. The name will not
               be freed while we hold the DomainMonitor, so we 
               do not have to make a copy of it. *)
            IF Text.Equal(tn, 
                          M3toC.StoT(LOOPHOLE(typeDefn.name, 
                                              UNTRACED REF [-128..127]))) THEN
              RETURN typeDefn.typecode;
            END;
            typeDefn := typeDefn.next;
          END;
        END;
      END;

      RETURN RTType.NoSuchType;
    END FindTypeCode;

  BEGIN
    IF domain = NIL THEN RETURN END;
    LOCK DomainMonitor DO
      (* Verify that the named type exists in the domain and get its
         typecode. *)
      WITH tc = FindTypeCode(domain, typename) DO
        IF tc # RTType.NoSuchType THEN
          EVAL DomainChecks.TypeAuthTbl.put(tc, auth);
        END;
      END;
    END;
  END AddTypeAuthorizer;

(*-------------------------------------------------------- initialization ---*)

VAR
  RuntimeInitMonitor: MUTEX;

(* FIXME: this procedure only checks whether a symbol starting with "MI_"
   or MM_ is defined in the object file.  This is a safety loophole because
   someone could fake such a symbol and pretend it's safe Modula-3 code.
   We should rely on a trusted compiler to convey this information *)
PROCEDURE IsModula3(domain: T) : BOOLEAN =
  BEGIN
    RETURN MachineLinker.IsModula3(domain.module);
  END IsModula3; 

PROCEDURE Initialize (domain: T): BOOLEAN =
  VAR
    newModules, allModules: ModulePtrs;
    newLinkInfo: LinkInfoPtrs;
    spl: CPU.InterruptLevel;
    units: Mx.UnitList;
  BEGIN
    IF domain = NIL THEN
      RETURN TRUE;
    END;

    (* Enable a backwards mapping from any address in a domain to
       the domain handle itself. The monitor prevents you from seeing
       the splay tree in an inconsistent state. *)
    LOCK RangeMapMonitor DO
      AddRangesToTree(domain);
    END;

    TrackDomain.SetNewborn(domain);
    LOCK DomainMonitor DO
      (* do not initialize domains that are not fully resolved *)
      IF NOT FullyResolved_1(domain) THEN
        IO.PutError("Domain " & domain.name & " not fully resolved.\n");
        TrackDomain.UnsetNewborn(domain);
        RETURN FALSE;
      END;

      (* collect information about Modula-3 units in subdomains *)
      CollectM3Modules(domain, newModules, newLinkInfo);
      IF newLinkInfo = NIL OR NUMBER(newLinkInfo^) = 0 THEN
        IO.PutError ("missing link information\n");
      END;
    END;

    (* no M3 code in the domain *)
    IF newModules = NIL THEN
      Strip(domain);
      TrackDomain.UnsetNewborn(domain);
      RETURN TRUE;
    END;

    (* reinitialize runtime if the domain contains M3 code *)
    LOCK RuntimeInitMonitor DO
      (* check interface consistency *)
      units := ReadM3LinkInfo(domain, newLinkInfo);

      (* Check whether domain subtypes from unauthorized supertypes,
         and if import/export relationships are OK *)
      IF NOT DomainChecks.CheckSubtypes (domain) OR
         (* MxCheck.CleanNormal should be deleted when memory pressure
            is reduced, or if collector gets better *)
         (NOT MxCheck.CleanNormal AND
          NOT DomainChecks.CheckImportExport (units, domain))
       THEN
        TrackDomain.UnsetNewborn(domain);
        (* dispose of unnecessary memory *)
        DISPOSE(newModules);
        DISPOSE(newLinkInfo);
        RETURN FALSE;
      END;

      (* combine module information with exisiting modules *)
      allModules := CombineModules(newModules);
      
      (* recompute the state of the runtime with new modules,the changes*)
      (* are computed but not committed until RTLinker.Update is run *)
      IF NOT RTLinker.Reinitialize(allModules) THEN
        IO.PutError("Runtime reinitialization failed.\n");
        TrackDomain.UnsetNewborn(domain);
        RETURN FALSE;
      END;

      (* update the state of the runtime *)
      (* it is non-preemptible, no M3 code can run because *)
      (* runtime data structures in RT0u and RTLinker are being updated *)
      spl := CPUPrivate.SetInterruptMask(
                 CPUPrivate.InterruptClass.High);
      RTLinker.Update();
      CPUPrivate.RestoreInterruptMask(spl);

      (* resychronize caches *)
      CPU.FlushInstructionCache();
    END;

    (* dispose of unnecessary memory *)
    FOR i := FIRST(newModules^) TO LAST(newModules^) DO
      newModules[i] := NIL;
    END;
    DISPOSE(newModules);
    FOR i := FIRST(newLinkInfo^) TO LAST(newLinkInfo^) DO
      newLinkInfo[i] := NIL;
    END;
    DISPOSE(newLinkInfo);
    Strip(domain);
    TrackDomain.UnsetNewborn(domain);

    PreventExportOfInterfaces (domain);

    (* FIXME - NEED TO RUN MAIN BODIES USING A COPY OF THE MAIN BODY LIST *)
    (* synchronize with previous initialization *)
    (* clean-up the run-time *)
    (*
      RTLinker.CleanUp();
    *)

    VAR
      newExt := TrackExtension.Create(domain);
      curExt : TrackExtension.T;
    BEGIN
      TrackDomain.SetExtension(newExt,domain);
      IF DebugOption.DoTrackStrand THEN 
        curExt := TrackStrand.Extension();
        TrackStrand.SetExtension(newExt);
      END;

      (* execute code in the domain *)
      RTLinker.ExecuteMainBodies();

      IF DebugOption.DoTrackStrand THEN       
        TrackStrand.SetExtension(curExt);
      END;
    END;

    RETURN TRUE;
  END Initialize;

(*------------------------------------------------------ Modula-3 linking ---*)

(* find type info and create a list of domains containing Modula 3 code
   for all subdomains and then for the main domain *)
PROCEDURE CollectM3Modules(domain: T; 
                           VAR totalmods: ModulePtrs;
                           VAR totalinfo: LinkInfoPtrs) =
  VAR
    subd: DList;
    (* array of runtime module descriptors *)
    newmods, childmods: ModulePtrs := NIL;
    newinfo, childinfo: LinkInfoPtrs := NIL;
  BEGIN
    (* find symbol for unit descriptor, copy them out to M3 allocated
       arrays and make the linker free the structures it allocated *)
    MachineLinker.Find_unit_descriptors(domain.module, totalmods, totalinfo);
    IF totalmods # NIL THEN
      newmods := NEW(ModulePtrs, NUMBER(totalmods^));
      SUBARRAY(newmods^, 0, NUMBER(totalmods^)) := totalmods^;
    ELSE
    END;
    IF totalinfo # NIL THEN
      newinfo := NEW(LinkInfoPtrs, NUMBER(totalinfo^));
      SUBARRAY(newinfo^, 0, NUMBER(totalinfo^)) := totalinfo^;
    ELSE
    END;
    MachineLinker.Free_unit_descriptors(totalmods, totalinfo);
    totalmods := newmods;
    totalinfo := newinfo;

    subd := domain.subdlist;
    WHILE subd # NIL DO
      CollectM3Modules(subd.domain, childmods, childinfo);
      subd := subd.next;
      IF childmods # NIL THEN
        IF totalmods = NIL THEN
          totalmods := childmods;
          totalinfo := childinfo;
        ELSE
          (* The order of merging subarrays below is important. It matches
             the init ordering gernated by the compiler *)
          newmods := NEW(ModulePtrs, 
                         NUMBER(totalmods^) + NUMBER(childmods^));

          SUBARRAY(newmods^, 0, NUMBER(totalmods^)) := totalmods^;
          SUBARRAY(newmods^, NUMBER(totalmods^), NUMBER(childmods^)) :=
              childmods^;

          FOR i := FIRST(childmods^) TO LAST(childmods^) DO
            childmods[i] := NIL;
          END;
          DISPOSE(childmods);
          FOR i := FIRST(totalmods^) TO LAST(totalmods^) DO
            totalmods[i] := NIL;
          END;
          DISPOSE(totalmods);
          totalmods := newmods;
          
          newinfo := NEW(LinkInfoPtrs, 
                         NUMBER(totalinfo^) + NUMBER(childinfo^));
          SUBARRAY(newinfo^, 0, NUMBER(totalinfo^)) := totalinfo^;
          SUBARRAY(newinfo^, NUMBER(totalinfo^), NUMBER(childinfo^)) :=
              childinfo^;
          FOR i := FIRST(totalinfo^) TO LAST(totalinfo^) DO
            totalinfo[i] := NIL;
          END;
          DISPOSE(totalinfo);
          FOR i := FIRST(childinfo^) TO LAST(childinfo^) DO
            childinfo[i] := NIL;
          END;
          DISPOSE(childinfo);
          totalinfo := newinfo;
	END;
      END;
    END;

    (* the number of unit descriptors and link information strings should
       be the same or the later should be zero *)
    IF ((totalmods = NIL) # (totalinfo = NIL)) OR
      (((totalmods # NIL) AND (totalinfo # NIL)) AND
      (NUMBER(totalmods^) # NUMBER(totalinfo^) AND NUMBER(totalinfo^) # 0))
     THEN
      IO.PutError(
          "Number of unit and link info descriptors doesn't match: " );
      IF totalmods # NIL AND totalinfo # NIL THEN
        IO.Put(Fmt.Int(NUMBER(totalmods^)) & " " & 
          Fmt.Int(NUMBER(totalinfo^)) &"\n");
      ELSIF totalmods # NIL THEN
        IO.Put(Fmt.Int(NUMBER(totalmods^)) & " 0\n");
      ELSE
        IO.Put("0 " & Fmt.Int(NUMBER(totalinfo^)) & "\n");
      END;
      (* FIXME: should bzero those too *)
      totalmods := NIL;
      totalinfo := NIL;
    END;
  END CollectM3Modules;

(* return a new list of all modules in the system *)
PROCEDURE CombineModules(newModules: ModulePtrs) : ModulePtrs =
  VAR
    totModules: ModulePtrs;
    oldTotalModules := RT0u.nModules; (* old total count *)
    newTotalModules := oldTotalModules + NUMBER(newModules^);
  BEGIN
    IF verbose THEN
      IO.Put(">>> NEW extension loaded: " &
        Fmt.Int(NUMBER(newModules^)) & " new units, " & 
        Fmt.Int(newTotalModules) & " total units\n");
    END;

    (* create new total list of modules *)
    totModules := NEW(ModulePtrs, newTotalModules);

    (* copy pointers to old modules *)
    FOR i := 0 TO oldTotalModules - 1 DO
      totModules[i] := LOOPHOLE(RT0u.modules + i*ADRSIZE(ADDRESS),
                                 UNTRACED REF RT0.ModulePtr)^;
    END;
    SUBARRAY(totModules^, oldTotalModules, NUMBER(newModules^)) := newModules^;
    RETURN totModules;
  END CombineModules;

PROCEDURE ReadM3LinkInfo(domain: T; linkInfo: LinkInfoPtrs): Mx.UnitList =
  VAR
    units0, units: Mx.UnitList;
    wr: Wr.T := ThreadExtra.GetWrSelf();
    ok := TRUE;
    unitDomain: T;
  BEGIN
    IF NOT checkInterfaces THEN RETURN NIL; END;

    domain.linkBase := Mx.NewSet ();
    Mx.AddLinked(domain.linkBase, staticLinkBase);
    AddInfoFromLinked(domain, domain);

    IF verbose THEN
      IO.Put(">>> reading new Modula-3 link information\n");
    END;

    MxIn.StartReading();
    FOR i := FIRST(linkInfo^) TO LAST(linkInfo^) DO
      IF verbose THEN IO.Put("."); END;
      TRY
        unitDomain := AddressToDomain (linkInfo[i]);
      EXCEPT
      | NonDomainAddress =>
        <* ASSERT FALSE *>
      END;
      IF MxIn.ReadUnits(linkInfo[i], wr, unitDomain) = -1 THEN
        IO.PutError("Could not parse link information\n");
        ok := FALSE;
        EXIT;
      END;
    END;

    IF ok THEN
      units0 := MxIn.GetUnits();
      units := Mx.Copy (units0);
      IF NOT DomainChecks.CheckM3LinkInfo(domain.linkBase,
                                          units, 
                                          NUMBER(linkInfo^),
                                          wr,
                                          FALSE,
                                          FALSE) 
       THEN
        IO.PutError("extension is not a correct M3 program\n");
        ok := FALSE;
      END;
    END;

    IF ok AND verbose THEN
      IO.Put(">>> the extension is a correct Modula-3 program\n");
    END;

    (* FIXME
    MxIn.Done();
    *)
    RETURN units0;
  END ReadM3LinkInfo;

PROCEDURE ReadStaticM3LinkInfo() =
  VAR 
    units     : Mx.UnitList := NIL;
    nUnits    := RTLinker.info.n_modules;
    module    := RTLinker.info.modules;
    linkInfo  := RTLinker.info.link_info;  
    wr: Wr.T  := ThreadExtra.GetWrSelf();
    check     : BOOLEAN;
    unit      : Mx.Unit;
    ok        : BOOLEAN := TRUE;
  BEGIN
    IF verbose THEN
      IO.Put(">>> reading static Modula-3 link information\n");
    END;

    MxIn.StartReading();
    FOR i := 0 TO nUnits-1 DO
      IF verbose THEN IO.Put("."); END;
      IF MxIn.SpinReadUnits(linkInfo^, wr, NIL, unit) = -1 THEN
        IO.PutError("Could not parse link information\n");
        ok := FALSE;
        EXIT;
      END;
      check := moduleUnitMap.put (LOOPHOLE (module,
                                            UNTRACED REF RT0.ModulePtr)^,
                                  unit);
      <* ASSERT NOT check *>
      INC (module, ADRSIZE (RT0.ModulePtr));
      INC (linkInfo, ADRSIZE (UNTRACED REF CHAR));
    END;

    IF ok THEN
      units := MxIn.GetUnits();
      IF NOT DomainChecks.CheckM3LinkInfo (staticLinkBase, units, nUnits, 
                                           wr, FALSE, TRUE) 
       THEN
        IO.PutError("the boot image is not a correct M3 program\n");
      ELSE
        IO.Put("the kernel is a correct Modula-3 program\n");
      END;
      units := NIL;
    END;
  END ReadStaticM3LinkInfo;

(*-------------------------------------------------------- static domains ---*)

PROCEDURE StaticDomainsInitialized () =
  BEGIN
    <* ASSERT NOT staticsDone *>
    staticsDone := TRUE;
    moduleUnitMap := NIL;    (* allow garbage collection *)
  END StaticDomainsInitialized;

TYPE
  FixProcClosure = ProcClosure OBJECT
    domain: T;
  OVERRIDES
    apply := AddStaticRange;
  END;

(* the static domains have to have the M3 linkage units point back
   to them -- the linkage units were read in before the domains were
   created *)
PROCEDURE FixStaticM3LinkInfo (domain: T;
                               READONLY symbols: ARRAY OF SymbolEntry.E) =
  VAR
    unit      : Mx.Unit;
    interface : RT0.ModulePtr;
    check     : BOOLEAN;
  BEGIN
    <* ASSERT moduleUnitMap # NIL *>

    (* cannot export any interfaces in the static domains *)
    PreventExportOfInterfaces (domain);

    (* fix up M3 linkage info to point back to domain *)
    FOR i := FIRST (symbols) TO LAST (symbols) DO
      interface := LOOPHOLE (symbols[i].SE_ptr, RT0.ModulePtr);
      check := moduleUnitMap.get (interface, unit);

      IF debugIE AND NOT check THEN
        IO.Put ("Could not find " & Fmt.Addr (interface) & " " &
          symbols[i].SE_name & " " & StoT (interface.file) & "\n");
        PrintMap ();
      END;
      <* ASSERT check AND unit # NIL *>
      IF debugIE THEN
        IO.Put ("Assigning unit " & StoT (interface.file) & " domain " &
          domain.name & "\n");
      END;

      EVAL ApplyToProcedures (interface, 
                              NEW (FixProcClosure, domain := domain));

      unit.domain := domain;
    END;
  END FixStaticM3LinkInfo;

PROCEDURE AddStaticRange (self: FixProcClosure; proc: PROCANY): BOOLEAN =
  VAR
    end: ADDRESS;
    ignore1: PROCANY;
    ignore2: RTProcedureSRC.ProcInfo; 
    ignore3: RTProcedureSRC.UnitInfo; 
    ignore4: ADDRESS;
  BEGIN
    RTProcedureSRC.FromPC (proc, ignore1, end, ignore2,
                           ignore3, ignore3, ignore4, ignore4);
    LOCK RangeMapMonitor DO
      EVAL rangeMap.insert(Range.T{proc, end}, self.domain);
    END;
    RETURN TRUE;
  END AddStaticRange;

(*-------------------------------------------------------- ??? ---*)

PROCEDURE AddInfoFromLinked (top, current: T) =
  VAR
    subPtr: DList;
  BEGIN
    (* add info if this is not the domain being initialized *)
    IF current # top THEN
      AddInfoFromParent(top, current);
    END;

    (* add info for all linked *)
    subPtr := current.linked;
    WHILE subPtr # NIL DO
      AddInfoFromParent(top, subPtr.domain);
      subPtr := subPtr.next;
    END;

    (* add info for all reversely linked *)
    subPtr := current.rev_linked;
    WHILE subPtr # NIL DO
      AddInfoFromParent(top, subPtr.domain);
      subPtr := subPtr.next;
    END;

    (* recurse for all subdomains *)
    subPtr := current.subdlist;
    WHILE subPtr # NIL DO
      AddInfoFromLinked(top, subPtr.domain);
      subPtr := subPtr.next;
    END;
  END AddInfoFromLinked;

PROCEDURE AddInfoFromParent (top, d: T) =
  BEGIN
    WHILE d.parent # NIL DO
      d := d.parent;
    END;
    IF d.linkBase # NIL THEN
      Mx.AddLinked(top.linkBase, d.linkBase);
    END;
  END AddInfoFromParent; 

PROCEDURE AddLinked (mapper, mappee: T) =
  BEGIN
    WHILE mapper.parent # NIL DO mapper := mapper.parent; END;
    WHILE mappee.parent # NIL DO mappee := mappee.parent; END;
    AddToLinks(mapper.rev_linked, mappee);
    AddToLinks(mappee.linked, mapper);
  END AddLinked;

PROCEDURE AddToLinks (VAR list: DList; d: T) =
  VAR 
    ptr: DList := list;
    id: INTEGER := d.id;
    last: DList := NIL;
  BEGIN
    WHILE ptr # NIL DO
      IF ptr.domain.id = id THEN
        RETURN;
      END;
      last := ptr;
      ptr := ptr.next;
    END;

    ptr := NEW(DList);
    ptr.domain := d;

    IF last = NIL THEN
      list := ptr;
    ELSE
      last.next := ptr;
    END;
  END AddToLinks;

(*-------------------------------------------------------------- ??? ---*)

PROCEDURE TypeDefnToDomain(addr: ADDRESS): T RAISES {NonDomainTypeDefn} =
  VAR
    dom: T;
    symbols: REF ARRAY OF SymbolEntry.E;
    intf_ptr: RT0.ModulePtr;
    t : RT0.TypeDefn := NIL;
  BEGIN
    TRY
      dom := AddressToDomain(addr);
    EXCEPT
    | NonDomainAddress => RAISE NonDomainTypeDefn;
    END;

    IF dom.dynamic THEN
      RETURN dom;
    ELSE
      (* If the result of the address lookup was a static domain,
         then we need to check the individual interface descriptors
         to find out which one it was. *)
      dom := activeDomains;
      WHILE dom # NIL DO
        IF NOT dom.dynamic THEN
          symbols := dom.exported_intf;
          IF symbols # NIL THEN
            FOR i := FIRST(symbols^) TO LAST(symbols^) DO
              intf_ptr := LOOPHOLE(symbols[i].SE_ptr, RT0.ModulePtr);
              t := LOOPHOLE(intf_ptr.type_cells, RT0.TypeDefn);
              WHILE t # NIL DO
                IF t = addr THEN
                  RETURN dom;
                END;
                t := t.next;
              END;
            END;
          END;
        END;            
        dom := dom.next;
      END;
    END;

    RAISE NonDomainTypeDefn;
  END TypeDefnToDomain; 

PROCEDURE SetState(d: T; name: TEXT; trusted: BOOLEAN) =
  BEGIN
    IF name # NIL THEN d.name := (* "@" & *) name; END;
    d.trusted := trusted;
  END SetState;

PROCEDURE GetState (    d      : T;
                    VAR name   : TEXT;
                    VAR trusted: BOOLEAN;
                    VAR dynamic: BOOLEAN  ) =
  BEGIN
    name := d.name;
    trusted := d.trusted;
    dynamic := d.dynamic;
  END GetState;

PROCEDURE Nlist(d: T) =
  BEGIN
   IO.Put("DOMAIN -- Showing nlist for " & d.name & "\n");
   MachineLinker.Nlist(d.module);
  END Nlist;

PROCEDURE ShowModules(d: T) =
  BEGIN
    MachineLinker.ShowSectionInformation(d.module);
  END ShowModules;

PROCEDURE TextInfo (d: T; VAR start: Word.T; VAR size: INTEGER): BOOLEAN =
  BEGIN
    IF NOT d.dynamic THEN RETURN FALSE; END;
    RETURN MachineLinker.TextInfo(d.module, start, size);
  END TextInfo;

(* Stats returns how many subdomains have been created and how many
   bytes were in the original object files *)
PROCEDURE Stats(VAR subdomains:INTEGER; VAR bytes:INTEGER) =
  BEGIN
    subdomains := subdomainTotal;
    bytes := bytesTotal;
  END Stats;

(*----------------------------------------------------------- destruction ---*)

(* try to destroy a domain *)
PROCEDURE Destroy (<* UNUSED *>domain: T): BOOLEAN = 
  BEGIN
    IO.Put("WARNING >> Domain.Destroy not implemented\n");
    RETURN TRUE;
  END Destroy;

(*------------------------------------------------------------- stripping ---*)

PROCEDURE Strip (d:T) =
  VAR 
    subd: DList;
  BEGIN
    IF d = NIL THEN 
      RETURN;
    END;
    MachineLinker.Strip (d.module);

    (* m3 linker stuff *)
    MxCheck.Strip (d.linkBase);

    subd := d.subdlist;
    WHILE subd # NIL DO
      Strip(subd.domain);
      subd := subd.next;
    END;
  END Strip;

PROCEDURE StripAllDomains() =
  VAR
    ptr: T := activeDomains;
  BEGIN
    WHILE ptr # NIL DO
      Strip(ptr);
      ptr := ptr.next;
    END;
  END StripAllDomains;

(*--------------------------------- mapping between addresses and domains ---*)

(* Functions for the backwards mapping from addresses to domains. *)
PROCEDURE AddRangesToTree(domain: T) =
  VAR
    subds : DList;
    idx : INTEGER;
    start, stop : ADDRESS;
  BEGIN
    IF domain.subdlist # NIL THEN
      (* Traverse down to leaves *)
      subds := domain.subdlist;
      WHILE subds # NIL DO
        AddRangesToTree(subds.domain);
        subds := subds.next;
      END;
    ELSE
      (* Have a leaf domain *)
      idx := 0;
      WHILE MachineLinker.GetNextRange(domain.module, idx, start, stop) DO
        EVAL rangeMap.insert(Range.T{start, stop}, domain);
      END;
    END;
  END AddRangesToTree; 

PROCEDURE AddressToDomain(addr: ADDRESS;
                          inInterface: BOOLEAN := FALSE): T 
                          RAISES {NonDomainAddress} =
  VAR
    domain : T;
  BEGIN
    LOCK RangeMapMonitor DO
      (* Splay brings to the root an address range which contains addr. *)
      IF NOT rangeMap.find(Range.T{addr, addr + BYTESIZE(addr)}, domain) THEN
        RAISE NonDomainAddress;
      END;
    END;

    (* if we are interested in the domain through which a procedure
       is exported, then we have to find the interface and map it
       to a domain again *)
    IF inInterface AND domain.dynamic THEN
      VAR
        ignore1: PROCANY;
        ignore2: RTProcedureSRC.ProcInfo; 
        ignore3: RTProcedureSRC.UnitInfo; 
        interface: RTProcedureSRC.UnitInfo; 
        ignore4: ADDRESS;
        ignore5: ADDRESS;
      BEGIN
        RTProcedureSRC.FromPC (addr, ignore1, ignore5, ignore2,
                               ignore3, interface, ignore4, ignore4);
        IF interface # NIL THEN
          RETURN AddressToDomain(interface, FALSE);
        END;
      END;
    END;
    RETURN domain;
  END AddressToDomain;

(*-------------------------------------------------- generic list support ---*)

PROCEDURE Equal(d1, d2: T): BOOLEAN =
  BEGIN
    RETURN d1 = d2;
  END Equal;


(*---------------------------------------------------- debugging routines ---*)

PROCEDURE StoT (c: UNTRACED REF CHAR) : TEXT =
  BEGIN
    RETURN M3toC.StoT (LOOPHOLE (c, Ctypes.char_star));
  END StoT;

PROCEDURE PrintMap () =
  VAR
    iter: AddrUnitTbl.Iterator;
    m : ADDRESS;
    m2 : RT0.ModulePtr;
    u : Mx.Unit;
  BEGIN
    IF moduleUnitMap = NIL THEN RETURN; END;
    iter := moduleUnitMap.iterate ();
    
    IO.Put ("Printing map\n");
    WHILE iter.next (m, u) DO
      m2 := m;
      IO.Put (Fmt.Addr (m2) & " " &
              StoT (m2.file) & " : " & Fmt.Ref (u) & " " &
              M3ID.ToText (u.name) & "\n");
    END;
    IO.Put ("Done printing map\n");
  END PrintMap;

PROCEDURE PrintDList (dl: DList) =
  BEGIN
    IO.Put ("Printing domain list\n");
    WHILE dl # NIL DO
      IO.Put (dl.domain.name & "\n");
      dl := dl.next;
    END;
    IO.Put ("Done printing domain list\n");
  END PrintDList;

(*------------------------------------------------------------- iterators ---*)

(* A number of interators are provided for clients to be able to
   enumerate different entities contained in domains.  All procedures
   take closure arguments and the apply procedure is called for each
   element found.  If the procedure return FALSE, the iteration is
   terminated and FALSE is returned to the client.  TRUE is returned
   if the iteration completes successfully.  Each flavor of iterator
   procedure comes in two versions.  One of them takes a lock to
   guarantee atomicity wrt other domain operations and can be used
   only by trusted clients.  The other one does not take a lock and
   gives no atomicity guarantees are given but can be used by
   untrusted clients which must deal with synchronization themselves *)

(* 
 * Iterate over all domains contained by this domain.
 *)
  
PROCEDURE Apply (domain: T; cl: Closure): BOOLEAN =
  BEGIN
    LOCK DomainMonitor DO
      RETURN ApplyNoLock(domain, cl);
    END;
  END Apply;

PROCEDURE ApplyNoLock (domain: T; cl: Closure): BOOLEAN =
  VAR
    subd: DList;
  BEGIN
    (* no more domains *)
    IF domain = NIL THEN RETURN TRUE; END;

    (* if we are told we are done *)
    IF NOT cl.apply(domain) THEN RETURN FALSE; END;

    (* iterate over subdomains *)
    subd := domain.subdlist;
    WHILE subd # NIL DO
      IF NOT ApplyNoLock(subd.domain, cl) THEN RETURN FALSE; END;
      subd := subd.next;
    END;

    (* successful completion *)
    RETURN TRUE;
  END ApplyNoLock;

(*
 * Iterate over all domains in the system
 *)

PROCEDURE ApplyToAll(cl: Closure): BOOLEAN =
  BEGIN
    LOCK DomainMonitor DO
      RETURN ApplyToAllNoLock(cl);
    END;
  END ApplyToAll;

PROCEDURE ApplyToAllNoLock(cl: Closure): BOOLEAN =
  VAR
    domain: T;
  BEGIN
    domain := activeDomains;
    WHILE domain # NIL DO
      IF NOT cl.apply(domain) THEN
        RETURN FALSE;
      END;
      domain := domain.next;
    END;
    RETURN TRUE;
  END ApplyToAllNoLock;

(*
 * Iterate over all units within a domain
 *)

(* iterate over all units in the domain *)
PROCEDURE ApplyToUnits (domain: T; cl: UnitClosure): BOOLEAN =
  BEGIN
    LOCK DomainMonitor DO
      RETURN ApplyToUnitsNoLock(domain, cl);
    END;
  END ApplyToUnits;

PROCEDURE ApplyToUnitsNoLock (domain: T; cl: UnitClosure): BOOLEAN =
  VAR
    modules : RuntimeModulePtr;
    linkInfo: RuntimeLinkInfoPtr;
  BEGIN
    CollectM3Modules(domain, modules, linkInfo);
    IF modules # NIL THEN
      FOR i := FIRST(modules^) TO LAST(modules^) DO
        IF NOT cl.apply(modules[i]) THEN
          RETURN FALSE;
        END;
      END;
    END;

    RETURN TRUE;
  END ApplyToUnitsNoLock;

(*
 * Iterate over all procedures within a unit
 *)

PROCEDURE ApplyToProcedures (unit: RT0.ModulePtr; cl: ProcClosure): BOOLEAN =
  VAR
    procPtr: RT0.ProcPtr;
    list: REF ARRAY OF RTProcedureSRC.ProcDesc;
  BEGIN
    procPtr := unit.proc_info;
    (* FIXME
    IF procPtr # NIL THEN
      WHILE procPtr^.proc # NIL DO
        IF NOT cl.apply(procPtr^.proc) THEN
          RETURN FALSE;
        END;
      END;
    ELSE
    *)
      list := RTProcedureSRC.GetExported(unit);
      IF list # NIL THEN
        FOR i := FIRST(list^) TO LAST(list^) DO
          IF NOT cl.apply(list[i].proc) THEN
            RETURN FALSE;
          END;
        END;
      END;
    (*
    END;
    *)
    RETURN TRUE;
  END ApplyToProcedures; 

(*-------------------------------------------------------- initialization ---*)

PROCEDURE InitM3(<* UNUSED *>verbose: BOOLEAN) =
  BEGIN
    (* FIXME: a hack so that we don't have to explicitely initialize Wr *)
    IF Wr.EOL = NIL THEN
      Wr.EOL := "\n";
    END;

    MxIn.Init();
    M3FP.Init();
    M3ID.Init();
    MxVS.Init();

    IF NOT checkInterfaces THEN 
      IO.PutError("Modula-3 interface checking disabled\n");
      RETURN; 
    END;
    IO.Put("checking m3 link info .. ");

    (* capture the expected number of staticly linked units *)
    staticModuleCnt := RT0u.nModules;

    moduleUnitMap := NEW (AddrUnitTbl.Default).init(RTLinker.info.n_modules);


    (* create the link information for the static code *)
    (* FIXME
    staticCode  := Create("StaticCode");
    *)
    staticLinkBase  := Mx.NewSet ();
    ReadStaticM3LinkInfo();
  END InitM3;

PROCEDURE Init (<*UNUSED*> verbose: BOOLEAN) =
  BEGIN
    DomainMonitor      := NEW(MUTEX);
    RuntimeInitMonitor := NEW(MUTEX);
    debug := CreateFromModule(debugName, MachineLinker.GetDebugModule());
    rangeMap          := NEW(RangeDomainSplayTree.T);
    RangeMapMonitor   := NEW(MUTEX);
    DomainChecks.TypeAuthTbl := NEW(IntAuthTbl.Default).init(10);
  END Init;

BEGIN
END Domain.

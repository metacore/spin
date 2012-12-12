/*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 18-Jul-95  Emin Gun Sirer (egs) at the University of Washington
 *	Added unresolve support for BRADDR and GPREL32
 *
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Unresolve undoes external relocations.
 *
 */
#include "Interface.h"
#include "Resolve.h"
#include "Section.h"
#include "Symbol.h"
#undef PRINT
#define PRINT printf

static int Unresolve(Reloc *rel, Section *sec, Module *ms, Module *against);

extern LinkState
Unresolve_section(Section *ss, Module *ms, Module *against)
{
    Reloc *reloc;
    int    j, didit = 0;
    
    PRINT("Unfixing up section %s ",ss->SS_name);
    PRINT("vaddr 0x%lx size 0x%lx ", (long)ss->SS_ptr, ss->SS_size);
    PRINT("relocs %ld\n",ss->SS_nReloc);
    if(ms == NULL || against == NULL)
	return;
    for(j = 0; j < ss->SS_nReloc; ++j) {
	reloc = &ss->SS_relocPtr[j];
	if(reloc->RS_patched == 1) {
	    didit |= Unresolve(reloc, ss, ms, against);
	}
    }
    return ss->SS_flags = (didit ? Resolved_Partially: ss->SS_flags);
}

/*
 * Returns true for successfully unresolved relocations
 */
static int
Unresolve(Reloc *rel, Section *sec, Module *ms, Module *against)
{
    Section      *ss;
    SymbolEntry  *sym, *sym2;
    int          *relAddress;
    struct relocation_info *relinfo;

    ASSERT(rel->RS_patched == 1);
    relinfo = &rel->RS_relocInfo;

    /*
     * Determine the value for the relocation.
     */
    if(relinfo->r_extern) { 
	sym = &ms->MS_symPtr->ST_symBlkPtr[relinfo->r_symbolnum];
	if(sym == NULL) {
	    error("relocatable external symbol is NULL\n");
	    return 0;
	}
	if(sym->SE_nsclass == scUndefined || sym->SE_nsclass == scUndefinedInternal) {
	    sym2 = Symbol_find(against, sym->SE_name);
	    if(sym2 == NULL) {
		/*
		 * the imported symbol is not defined in the module that
		 * is getting unlinked. We don't have to do work.
		 */
		return 0;
	    }
	}
    } else {
	/*
	 * internal relocation.
	 */
	return 0;
    }
    relAddress = (int *) rel->RS_patchAddr;

    /* 
     * Real work begins here
     */
    switch(relinfo->r_length) {
      case 0:
	*((char *) relAddress) = (char) rel->RS_patchValue;
	PRINT("unpatched to 0x%lx\t", (char) rel->RS_patchValue);
	break;
      case 1:
	*((short *) relAddress) = (short) rel->RS_patchValue;
	PRINT("unpatched to 0x%lx\t", (short) rel->RS_patchValue);
	break;
      case 2:
	*((long *) relAddress) = (long) rel->RS_patchValue;
	PRINT("unpatched to 0x%lx\t", (long) rel->RS_patchValue);
	break;
      default:
	error("unknown relocation size");
	return 0;
    }

    rel->RS_patched = 0;
    return 1;
}


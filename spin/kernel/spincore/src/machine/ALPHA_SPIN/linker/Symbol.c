/*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 10-Jul-96  Tian Fung Lim (tian) at the University of Washington
 *	Changed mallocs to use appropriate pools.
 *
 * 12-Apr-96  Emin Gun Sirer (egs) at the University of Washington
 *	Changed accesses to filehdr to go though coffxtrct to avoid
 *	alignment problems.
 *
 * 07-Nov-95  Emin Gun Sirer (egs) at the University of Washington
 *	Changed to treat scNil as a nop, much like scUndefined.
 *      Treat SData the same way as Data.
 *      Enabled the exceptional case diagnostics to pinpoint problems.
 *
 * 17-Aug-95  Emin Gun Sirer (egs) at the University of Washington
 *	Changed to treat scSUndefined the same way as scUndefined.
 *
 * 07-Jul-95  Emin Gun Sirer (egs) at the University of Washington
 *	Added a special debug domain that identifies unresolved symbols
 *      when linked against.
 *
 * 19-Jun-95  Emin Gun Sirer (egs) at the University of Washington
 *	Allocate space for commons in bss segment.
 *
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Symbol table manipulation routines.
 *
 */
#include <symconst.h>
#include "Interface.h"
#include "Hash.h"
#include "Module.h"
#include "Symbol.h"
#include "Section.h"

/*
#undef PRINT
#define PRINT printf
*/

/*
 * Create two symbol table entries from a COFF file,
 * one for internal symbols, one for external.
 */
extern void
Symbol_create_table(Module *ms, char *source)
{
    StringList  *intstr = LALLOC(StringList);
    StringList  *extstr = LALLOC(StringList);
    SymbolEntry *se, *se2;
    struct fdr  *fdr, *p_fdr;
    FILHDR      *filehdr;
    HDRR        *coffsymhdr;
    SYMR        *symr, *s_symr;
    EXTR        *extr, *s_extr;
    long         int_symsize, ext_symsize;
    long         i, j;
    /* will be cleaned by StripInternalSymbols      */

    ms->MS_intStrPtr = intstr;
    ms->MS_extStrPtr = extstr;
  
    filehdr = (FILHDR *)coffxtrct(source, sizeof(FILHDR));
    
    coffsymhdr = (HDRR *)coffxtrct(source + filehdr->f_symptr,
				   sizeof(HDRR));

    ms->MS_intSymPtr = LALLOC(SymbolTable);
    ms->MS_extSymPtr = LALLOC(SymbolTable);
    bzero (ms->MS_intSymPtr, sizeof (struct SymbolTable));
    bzero (ms->MS_extSymPtr, sizeof (struct SymbolTable));    

    ms->MS_extSymPtr->ST_nSyms = ext_symsize = coffsymhdr->iextMax;
    ms->MS_intSymPtr->ST_nSyms = int_symsize = coffsymhdr->isymMax;

    int_symsize *= sizeof(struct SymbolEntry);
    ext_symsize *= sizeof(struct SymbolEntry);

    ms->MS_intSymPtr->ST_symBlkPtr = symlalloc(int_symsize);
    Hash_create(ms->MS_intSymPtr);

    ms->MS_extSymPtr->ST_symBlkPtr = symlalloc(ext_symsize);
    Hash_create(ms->MS_extSymPtr);

    intstr->SL_size      = coffsymhdr->issMax;
    extstr->SL_size      = coffsymhdr->issExtMax;

    extstr->SL_strBlkPtr = symlalloc(extstr->SL_size);
    intstr->SL_strBlkPtr = symlalloc(intstr->SL_size);

    /* 
     * Get the strings.
     */
    memcpy(intstr->SL_strBlkPtr, source + coffsymhdr->cbSsOffset,
	   intstr->SL_size);
    memcpy(extstr->SL_strBlkPtr, source + coffsymhdr->cbSsExtOffset, 
	   extstr->SL_size);

    /*
     * Fill the symbol tables in with interesting tidbits of information.
     * The info is smeared all over the place. The external symbols are
     * in a chunk. The others, which also include exported non-externals,
     * hang off of file descriptors.
     */
    extr = (EXTR *)(source + coffsymhdr->cbExtOffset);
    se = ms->MS_extSymPtr->ST_symBlkPtr;
    for(i = 0; i < coffsymhdr->iextMax; i++, extr++, se++) {
	/*
	 * Structures may not be 8-aligned in the coff-file
	 */
	s_extr = coffxtrct(extr, sizeof(EXTR));
	se->SE_ptr     = NULL;
	se->SE_name    = extstr->SL_strBlkPtr + s_extr->asym.iss;
	se->SE_nvalue  = s_extr->asym.value;
	se->SE_nsclass = s_extr->asym.sc;
	se->SE_module  = ms;
	Hash_add_symbol(se, ms->MS_extSymPtr);
	PRINT("Added external symbol %s\n", se->SE_name);
	coffdone(s_extr, extr);
	/*
	 * Allocate space for common unitialized symbols.
	 * The nvalue field contains the size of the symbol in bytes.
	 */
	switch(se->SE_nsclass) {
	case scCommon:
	case scSCommon:
	    PRINT("Symbol %s is a common\n", se->SE_name);
#if OLDDOMAINS
	    /*   
	     * Check if previously allocated space.
	     */
	    se2 = Domain_find_symbol(parent, se->SE_name);
	    if(se2 != NULL && se2->SE_nsclass == se->SE_nsclass) {
		/*
		 * If so, use it.
		 */
		se->SE_ptr = se2->SE_ptr;
	    } else {
#endif
		/*
		 * FIX We pad too conservatively,
		 *     where the f*ck is the pad info ?
		 */
		Module_pool_setspace(ms, POOL_BSS, se->SE_nvalue);
#if OLDDOMAINS
	    }
#endif
	}
    }

    PRINT("coffsymhdr says %d internal symbols\n", coffsymhdr->isymMax);
    fdr = (struct fdr *) (source + coffsymhdr->cbFdOffset);
    se = ms->MS_intSymPtr->ST_symBlkPtr;
    for(j = 0; j <  coffsymhdr->ifdMax; ++j, ++fdr) {
	p_fdr = (struct fdr *) coffxtrct(fdr, sizeof(struct fdr));
	PRINT("fdr adr: 0x%lx ", p_fdr->adr);
	PRINT("ss-offset:0x%x nsymbols: %d\n", p_fdr->issBase, p_fdr->csym);
	PRINT("file name: %s\n", intstr->SL_strBlkPtr + p_fdr->issBase + p_fdr->rss);
	OUTPUTFLUSH();
	symr = (SYMR *)(source + coffsymhdr->cbSymOffset + p_fdr->isymBase * sizeof(SYMR));
	for(i = 0; i < p_fdr->csym; i++, symr++, se++) {
	    s_symr = coffxtrct(symr, sizeof(SYMR));
	    se->SE_ptr     = NULL;
	    se->SE_name    = intstr->SL_strBlkPtr + p_fdr->issBase + s_symr->iss;
	    se->SE_nvalue  = s_symr->value;
	    se->SE_nsclass = s_symr->sc;
	    se->SE_module  = ms;
	    Hash_add_symbol(se, ms->MS_intSymPtr);
	    PRINT("Added internal symbol %s\n", se->SE_name);
	    coffdone(s_symr, symr);
	}
	coffdone(p_fdr, fdr);
    }
    coffdone(coffsymhdr, source + filehdr->f_symptr);
    coffdone(filehdr, source);


}

/*
 * Take a zero terminated array and create a symbol table based on it.
 */
extern void
Symbol_fake_table(Module *ms, SymbolEntry *syms, int symsize)
{
    long         i, oldsize, newsize;

    if(ms->MS_extSymPtr == NULL) {
	oldsize = 0;
	ms->MS_extSymPtr = LALLOC(SymbolTable);
	bzero (ms->MS_extSymPtr, sizeof(struct SymbolTable));
	ms->MS_extSymPtr->ST_nSyms = symsize;
    } else {
	oldsize = ms->MS_extSymPtr->ST_nSyms;
	ms->MS_extSymPtr->ST_nSyms += symsize;
    }
    
    newsize = ms->MS_extSymPtr->ST_nSyms * sizeof(struct SymbolEntry);
    ms->MS_extSymPtr->ST_symBlkPtr =
	(SymbolEntry *)	myrealloc(ms->MS_extSymPtr->ST_symBlkPtr,
				  oldsize * sizeof(struct SymbolEntry),
				  newsize);
    Hash_create(ms->MS_extSymPtr);

    for(i = 0; i < oldsize; i++)
	Hash_add_symbol(&ms->MS_extSymPtr->ST_symBlkPtr[i], ms->MS_extSymPtr);

    for(; i < ms->MS_extSymPtr->ST_nSyms; i++) {
	ms->MS_extSymPtr->ST_symBlkPtr[i] = syms[i - oldsize];
	ms->MS_extSymPtr->ST_symBlkPtr[i].SE_module = ms;
	Hash_add_symbol(&ms->MS_extSymPtr->ST_symBlkPtr[i], ms->MS_extSymPtr);
	PRINT("Added fake symbol %s 0x%lx\n", 
	       syms[i - oldsize].SE_name,
	       syms[i - oldsize].SE_nvalue);
    }
}

/*
 * Assign space to uninitialized variables.
 */
extern void
Symbol_assign_space(Module *ms) {
    SymbolEntry *se;
    int i;

    se = ms->MS_extSymPtr->ST_symBlkPtr;
    for(i = 0; i < ms->MS_extSymPtr->ST_nSyms; i++, se++) {
	switch(se->SE_nsclass) {
	case scCommon:
	case scSCommon:
	    /*
	     * allocate space only if not previously allocated.
	     */
	    if(se->SE_ptr == NULL)
		se->SE_ptr = Module_pool_allocate(ms, POOL_BSS, se->SE_nvalue);
	}
    }
}

/*
 * Given a symbol entry, return its address in memory.
 */
extern long
Symbol_get_value(SymbolEntry *se) {
    Module *ms;
    Section *ss;

    ms = se->SE_module;

    ASSERT(se);
    switch(se->SE_nsclass) {
    case scNil:           /* declared as an extern in a module, */
	                  /* but not used by that module. */
	                  /* why put it in the damn external syms section ? */
	return Invalid_Address;
    case scText:          /* text symbol */
	ss = Section_find_by_name(ms, ".text");
	return (long) ss->SS_ptr + se->SE_nvalue - ss->SS_baseValue;
    case scData:          /* initialized data symbol */
	ss = Section_find_by_name(ms, ".data");
	return (long) ss->SS_ptr + se->SE_nvalue - ss->SS_baseValue;
    case scAbs:           /* value of symbol is absolute */
	return se->SE_nvalue;
    case scBss:           /* un-initialized data symbol */
	break;
    case scRegister:      /* value of symbol is register number */
	break;
    case scUndefined:     /* who knows? */
    case scSUndefined:    /* small undefined(external) data */
	return Invalid_Address;
    case scSData:         /* load time only small data */
	ss = Section_find_by_name(ms, ".sdata");
	return (long) ss->SS_ptr + se->SE_nvalue - ss->SS_baseValue;
    case scCdbLocal:      /* variable's value is IN se->va.?? */
    case scBits:          /* this is a bit field */
    case scDbx:           /* dbx internal use */
    case scRegImage:      /* register value saved on stack */
    case scInfo:          /* symbol contains debugger information */
    case scUserStruct:    /* address in struct user for current process */
    case scSBss:          /* load time only small common */
    case scRData:         /* load time only read only data */
	break;
    case scVar:           /* Var parameter (fortran,pascal) */
	return se->SE_nvalue;
    case scCommon:        /* common variable */
    case scSCommon:       /* small common */
	return (long) se->SE_ptr;
    case scVarRegister:   /* Var parameter in a register */
    case scVariant:       /* Variant record */
    case scInit:          /* .init section symbol */
    case scBasedVar:      /* Fortran or PL/1 ptr based var */ 
    case scXData:         /* exception handling data */
    case scPData:         /* Procedure section */
    case scFini:          /* .fini section */
    case scRConst:        /* .rconst section */
    default:
	break;
    }
    printf("weird (unsupported) symbol class for symbol <%s> class %d\n",
	   se->SE_name,
	   se->SE_nsclass);
    return 0;
}

/*
 * Returns a pointer to the symbol entry describing the concrete
 * definition of the named symbol.
 */
void *
Symbol_find(Module *ms, char *name) 
{
    SymbolEntry *se;

    if(ms == NULL)
	return NULL;
    else
	se = Hash_find_symbol(name, ms->MS_extSymPtr);
    return se;
}



static void
Nlist_reloc(Module *ms, Reloc *rel)
{
	struct reloc *relinfo;
        SymbolEntry *sym;

	relinfo = &rel->RS_relocInfo;
	
	if(relinfo->r_type != R_GPDISP &&
	   relinfo->r_type != R_GPVALUE &&
	   relinfo->r_extern){
		if(relinfo->r_symndx > ms->MS_extSymPtr->ST_nSyms) {
			error("bad reloc - r_symndx[index]\n");
			return;
		}

		sym = &ms->MS_extSymPtr->ST_symBlkPtr[relinfo->r_symndx];

		if(sym == NULL) {
			error("relocatable external symbol is NULL\n");
		}
		printf("%s\t", sym->SE_name);
		switch (sym->SE_nsclass) {
		case scUndefined: printf("?"); break;
		case scSUndefined: printf("S?"); break;
		default: printf(" = 0x%lx\n", Symbol_get_value(sym));
		}
		printf("\n");
	}
}

				       
Nlist(Module *ms)
{
	SymbolTable *st;
	SymbolEntry *se;
	int i;

	/* EXTERNAL SYMBOLS ONLY */
	printf("EXTERNAL SYMBOLS\n");
	st = ms->MS_extSymPtr;
	for (i = 0; i < st->ST_nSyms; ++i)  {
		se = &st->ST_symBlkPtr[i];
		printf("%s\t", se->SE_name);
		
		switch (se->SE_nsclass)  {
		case scCommon:  printf("C"); break;
		case scSCommon: printf("c"); break;			
		case scText: printf("T"); break;
		case scData: printf("D"); break;
		case scSData: printf("d"); break;
		case scRData: printf("R"); break;			
		case scAbs:  printf("A"); break;
		case scBss:  printf("B"); break;
		case scSBss:  printf("b"); break;			
		case scRegister: printf("R"); break; /* XX */
		case scUndefined: printf("U"); break;
		case scSUndefined: printf("u"); break;
		default: printf("??"); break;
		}
		printf("\t0x%lx\n", Symbol_get_value(se));
	}
}
	
		
		

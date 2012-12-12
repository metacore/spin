/*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
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
 * Create two symbol table entries from an a.out file,
 * one for internal symbols, one for external.
 */
extern void
Symbol_create_table(Module *ms, char *source)
{
    StringList  *strlist = LALLOC(StringList);
    SymbolEntry *se;
    struct exec *filehdr;
    struct nlist *syms;
    long         symsize;
    char         *strings;
    long         i, j;

    filehdr = (struct exec *) source;
     
    ms->MS_symPtr = LALLOC(SymbolTable);

    symsize = filehdr->a_syms / sizeof(struct nlist);
    ms->MS_symPtr->ST_nSyms = symsize;

    symsize *= sizeof(struct SymbolEntry);
    ms->MS_symPtr->ST_symBlkPtr = lalloc(symsize);
    Hash_create(ms->MS_symPtr);

    syms = (struct nlist *) (source + sizeof(struct exec) + filehdr->a_text + 
			     filehdr->a_data + 	filehdr->a_trsize + 
			     filehdr->a_drsize);

    strings = source + sizeof(struct exec) + filehdr->a_text + 
	filehdr->a_data + filehdr->a_trsize +
	filehdr->a_drsize + filehdr->a_syms;

    strlist->SL_size  = *((unsigned int *) strings);
    strlist->SL_strBlkPtr = lalloc(strlist->SL_size);

    /* 
     * Get the strings.
     */
    memcpy(strlist->SL_strBlkPtr, strings, strlist->SL_size);

    /*
     * Fill the symbol tables in with interesting tidbits of information.
     */
    se = ms->MS_symPtr->ST_symBlkPtr;
    for(i = 0; i < ms->MS_symPtr->ST_nSyms; i++) {
	se[i].SE_ptr     = NULL;
	se[i].SE_name    = strlist->SL_strBlkPtr + syms[i].n_un.n_strx;
	se[i].SE_nvalue  = syms[i].n_value;
	se[i].SE_module  = ms;

	switch(syms[i].n_type) {
	  case N_TEXT | N_EXT:
	    se[i].SE_nsclass = scText;
	    break;

	  case N_DATA | N_EXT:
	    se[i].SE_nsclass = scData;
	    break;
	    
	  case N_BSS | N_EXT:
	    se[i].SE_nsclass = scBss;
	    break;

	  case N_ABS | N_EXT:
	    se[i].SE_nsclass = scAbs;
	    break;

	  case N_UNDF | N_EXT:
	    if(se[i].SE_nvalue == 0)
		se[i].SE_nsclass = scUndefined;
	    else
		se[i].SE_nsclass = scCommon;		
	    break;

	  case N_COMM | N_EXT:
	    se[i].SE_nsclass = scCommon;
	    break;

	  case N_INDR | N_EXT:
	    se[i].SE_nsclass = scIndirect;
	    break;

	  case N_SIZE | N_EXT:
	    se[i].SE_nsclass = scSize;
	    break;

	  case N_FN | N_EXT:
	    se[i].SE_nsclass = scFilename;
	    break;

	  case N_TEXT:
	    se[i].SE_nsclass = scTextInternal;
	    break;

	  case N_DATA:
	    se[i].SE_nsclass = scDataInternal;
	    break;
	    
	  case N_BSS:
	    se[i].SE_nsclass = scBssInternal;
	    break;

	  case N_ABS:
	    se[i].SE_nsclass = scAbsInternal;
	    break;

	  case N_UNDF:
	    if(se[i].SE_nvalue == 0)
		se[i].SE_nsclass = scUndefinedInternal;
	    else
		se[i].SE_nsclass = scCommonInternal;		
	    break;

	  case N_COMM:
	    se[i].SE_nsclass = scCommonInternal;
	    break;

	  case N_INDR:
	    se[i].SE_nsclass = scIndirectInternal;
	    break;

	  case N_SIZE:
	    se[i].SE_nsclass = scSizeInternal;
	    break;

	  case N_WARN:
	    se[i].SE_nsclass = scWarning;
	    break;

	  default:
	    se[i].SE_nsclass = scOther;
	    break;
	}
	
	Hash_add_symbol(&se[i], ms->MS_symPtr);

	if(se[i].SE_nsclass <= LastExternalClass) {
	    PRINT("Added external symbol %s\n", se[i].SE_name);
	} else {
	    PRINT("Added internal symbol %s\n", se[i].SE_name);
	}


	if(se[i].SE_nsclass == scCommon || se[i].SE_nsclass == scCommonInternal) {
	    PRINT("Symbol %s is a common\n", se[i].SE_name);
	    Module_pool_setspace(ms, POOL_BSS, se[i].SE_nvalue);
	}
    }
}

/*
 * Take a zero terminated array and create a symbol table based on it.
 */
extern void
Symbol_fake_table(Module *ms, SymbolEntry *syms, int symsize)
{
    long         i, oldsize, newsize;

    if(ms->MS_symPtr == NULL) {
	oldsize = 0;
	ms->MS_symPtr = LALLOC(SymbolTable);
	ms->MS_symPtr->ST_nSyms = symsize;
    } else {
	oldsize = ms->MS_symPtr->ST_nSyms;
	ms->MS_symPtr->ST_nSyms += symsize;
    }
    
    newsize = ms->MS_symPtr->ST_nSyms * sizeof(struct SymbolEntry);
    ms->MS_symPtr->ST_symBlkPtr =
	(SymbolEntry *)	myrealloc(ms->MS_symPtr->ST_symBlkPtr,
				  oldsize * sizeof(struct SymbolEntry),
				  newsize);
    Hash_create(ms->MS_symPtr);

    for(i = 0; i < oldsize; i++)
	Hash_add_symbol(&ms->MS_symPtr->ST_symBlkPtr[i], ms->MS_symPtr);

    for(; i < ms->MS_symPtr->ST_nSyms; i++) {
	ms->MS_symPtr->ST_symBlkPtr[i] = syms[i - oldsize];
	ms->MS_symPtr->ST_symBlkPtr[i].SE_module = ms;
	Hash_add_symbol(&ms->MS_symPtr->ST_symBlkPtr[i], ms->MS_symPtr);
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

    se = ms->MS_symPtr->ST_symBlkPtr;
    for(i = 0; i < ms->MS_symPtr->ST_nSyms; i++) {
	if(se[i].SE_nsclass == scCommon || se[i].SE_nsclass == scCommonInternal) {
	    /*	
	     * allocate space only if not previously allocated.
	     */
	    if(se[i].SE_ptr == NULL)
		se[i].SE_ptr = Module_pool_allocate(ms, POOL_BSS, 
						    se[i].SE_nvalue);
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
      case scUndefined:
      case scUndefinedInternal:
	return NULL;
      case scText:
      case scTextInternal:
	ss = Section_find_by_name(ms, ".text");
	return (long) ss->SS_ptr + se->SE_nvalue - ss->SS_linkBase;
      case scData:
      case scDataInternal:
	ss = Section_find_by_name(ms, ".data");
	return (long) ss->SS_ptr + se->SE_nvalue - ss->SS_linkBase;
      case scAbs:
      case scAbsInternal:
	return se->SE_nvalue;
      case scBss:
      case scBssInternal:
	break;
      case scIndirect:
      case scIndirectInternal:
	break;
      case scSize:
      case scSizeInternal:
	break;
      case scCommon:
      case scCommonInternal:
	return (long) se->SE_ptr;
      case scFilename:
	break;
      case scWarning:
	break;
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
	se = Hash_find_symbol(name, ms->MS_symPtr);
    return se;
}

static void
Nlist_reloc(Module *ms, Reloc *rel)
{
    struct relocation_info *relinfo;
    SymbolEntry *sym;
    
    relinfo = &rel->RS_relocInfo;
    
    if(relinfo->r_extern) {
	if(relinfo->r_symbolnum > ms->MS_symPtr->ST_nSyms) {
	    error("bad reloc - r_symndx[index]\n");
	    return;
	}
	
	sym = &ms->MS_symPtr->ST_symBlkPtr[relinfo->r_symbolnum];
	
	if(sym == NULL) {
	    error("relocatable external symbol is NULL\n");
	}

	printf("%s\t", sym->SE_name);

	if(sym->SE_nsclass == scUndefined || sym->SE_nsclass == scUndefinedInternal)
	    printf("?");
	else
	    printf(" = 0x%lx\n", Symbol_get_value(sym));

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
	st = ms->MS_symPtr;
	for (i = 0; i < st->ST_nSyms; ++i)  {
	    se = &st->ST_symBlkPtr[i];
	    
	    if(se->SE_nsclass > LastExternalClass)
		continue;
	    
	    printf("%s\t", se->SE_name);
	    
	    switch (se->SE_nsclass)  {
	      case scUndefined: printf("U"); break;
	      case scAbs: printf("A"); break;
	      case scText: printf("T"); break;
	      case scData: printf("D"); break;
	      case scBss: printf("B"); break;
	      case scIndirect: printf("I"); break;
	      case scSize: printf("S"); break;
	      case scCommon: printf("C"); break;
	      case scFilename: printf("F"); break;
	      default: printf("??"); break;
	    }
	    
	    printf("\t0x%lx\n", Symbol_get_value(se));
	}
}
	
		
		


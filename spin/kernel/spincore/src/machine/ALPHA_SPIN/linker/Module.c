/*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 18-Feb-97  Charles Garrett (garrett) at the University of Washington
 *	GetNextStaticRange returns range of addresses of the static
 *	 kernel.
 *
 * 04-Feb-97  Marc Fiuczynski (mef) at the University of Washington
 *	It seems that the gcc 2.7.2.1 based m3 backend produces assembly
 *	code that results in .i3 files having a .text symbol, but that
 *	the start address is zero.  This causes m3gdbttd to not load in
 *	the .io file, consequently making debugging of types defined in
 *	an .i3 file impossible.
 *
 * 10-Jul-96  Tian Fung Lim (tian) at the University of Washington
 *	Changed frees to free from the appropriate pools. Ensured that
 *      memory to be used with myrealloc is zeroed. (non-null
 *      uninitialized ptrs made it free	garbage)
 *
 * 22-Apr-95  Przemek Pardyak (pardy) at the University of Washington
 *	Fixed a bug in add_symbol (was calling malloc with size 0).
 *
 * 02-Apr-95  Przemek Pardyak (pardy) at the University of Washington
 *	Find_unit_descriptors returns additionally unit link information
 *	and checks that there is such information for each Modula-3 
 *	unit descriptor.
 *
 * 26-Jan-95  Przemek Pardyak (pardy) at the University of Washington
 *	Register data and code segments as parts of untraced heap that
 *	do not contain pointers to traced heap.
 *
 * 31-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	Added IsModula3, which allows clients to mark non modula-3 code
 *	as a non-preemptible region.
 *      Fixed TextInfo to return a value in all cases.
 *
 * 06-Nov-95  Emin Gun Sirer (egs) at the University of Washington
 *      Cleaned up Find_unit_descriptors.
 *      Got rid of empty module.
 *
 * 20-Oct-95  Marc Fiuczynski (mef) at the University of Washington
 *	Moved the m3support for open arrays into glue.  Added a m3 array
 *	safe Generate function that just calls the C array based Generate
 *	function.
 *
 * 16-Aug-95  Przemek Pardyak (pardy) at the University of Washington
 *	Changed Find_unit_descriptors to find multiple M3 units 
 *	in one object file.
 *
 * 12-Aug-95  Emin Gun Sirer (egs) at the University of Washington
 *	Zero out the bss and sbss sections.
 *
 * 25-Jul-95  Brian Bershad (bershad) at the University of Washington
 *	Moved Core_init and default domain initialization code out of
 *	here and into CoreDomain.
 *
 * 07-Jul-95  Emin Gun Sirer (egs) at the University of Washington
 *	Added a special debug domain that identifies unresolved symbols
 *      when linked against.
 *
 * 30-Jun-95 Przemyslaw Pardyak (pardy) at the University of Washington
 *	Changed Symbol_findlinkinfo to Symbol_find_unit_descriptor,
 *	it returns the symbol for the Modula-3 module/interface descriptor.
 *	Changed Core_init to take an external array of core symbols.
 *
 * 19-Jun-95  Emin Gun Sirer (egs) at the University of Washington
 *	Added module_setspace to calculate the size of the bss segment
 *      for a module.
 *
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Object file resolution, symbol location, etc. management.
 *
 */
#include <symconst.h>
#include <spincore/src/sal/OpenArray.h>
#include "Interface.h"
#include "Module.h"
#include "Section.h"
#include "Symbol.h"
#include <m3core_sa/ALPHA_SPIN/RTCollectorSRC.h>

void register_clean(void *start, int size)
{
  (*RTCollectorSRC__RegisterClean)(start, size, 0);
}

void unregister_clean(void *start, int size)
{
  (*RTCollectorSRC__UnregisterClean)(start, size);
}


/* 
 * initialize pool allocator
 */
static void
Module_pool_initialize(Module *ms) {
    PoolType i;
    long bytes=0,offset=0;
    char *buf;

    for(i = 0; i < MAXPOOL; i++)
	bytes+=ms->MS_poolsize[i];
    buf = lalloc(bytes);

    for(i = 0; i < MAXPOOL; i++) {
	if(ms->MS_poolsize[i] > 0) {
	    ms->MS_poolfree[i] = ms->MS_pool[i] = buf + offset;
	    offset += ms->MS_poolsize[i];
	    register_clean(ms->MS_pool[i], ms->MS_poolsize[i]);
	    if(i == POOL_BSS)
		bzero(ms->MS_pool[i], ms->MS_poolsize[i]);
	}
    }
}

/*
 * Set aside some space in a given pool
 */
extern void
Module_pool_setspace(Module *ms, PoolType pool, int size) {
    ms->MS_poolsize[pool] += (size + 7) & ~7;
}

/*
 * Allocate some memory from given pool
 */
extern void *
Module_pool_allocate(Module *ms, PoolType pool, int size) {
    char *free = ms->MS_poolfree[pool];

    if(ms->MS_poolsize[pool] - (free - ms->MS_pool[pool]) < size)
	error("no space in pool\n");
    ms->MS_poolfree[pool] += (size + 7) & ~7;
    return free;
}

/*
 * Turns a coff file into a module
 */
extern Module *
MCreate(char *source)
{
    FILHDR  *fileHeader;
    AOUTHDR *optHeader;
    SCNHDR  *shdr, *p_shdr;
    Module  *ms;
    long     i;

    if(source == NULL)
	return NULL;

    ms = lalloc(sizeof(Module));
    bzero((void *) ms, sizeof(Module));
    
    fileHeader     = (FILHDR *) coffxtrct(source, FILHSZ);
    ms->MS_nSec    = fileHeader->f_nscns;
    ms->MS_sectPtr = seclalloc(sizeof(void *) * (ms->MS_nSec + 1));

    /*
     * See if we have an Optional header, and if so,
     * process it.
     */
    if(fileHeader->f_opthdr > 0 && fileHeader->f_opthdr == sizeof(AOUTHDR)) {
	optHeader = (AOUTHDR *) coffxtrct(source + FILHSZ, sizeof(AOUTHDR));
	ms->MS_oldgp = optHeader->gp_value;
    } else {
	free(ms, M_DMODULE);
	coffdone(fileHeader, source);
	return NULL;
    }

    /*
     * create internal and external symbol tables.
     * figure out how much space we need for uninitialized symbols.
     */
    Symbol_create_table(ms, source);

    /*
     * Now, each section's section header follows one
     * after the other. 
     *
     * We gather the sizes together on the first pass over sections,
     * then allocate space for raw data, and then the second pass
     * copies the raw data to the right place.
     * 
     */
    shdr = (SCNHDR *) (source + FILHSZ + fileHeader->f_opthdr);
    for(i = 0; i < ms->MS_nSec; shdr++, i++) {
	p_shdr = (SCNHDR *) coffxtrct(shdr, sizeof(SCNHDR));
	ms->MS_sectPtr[i] = Section_create(p_shdr,optHeader->entry);
	coffdone(p_shdr, shdr);
    }
    /*
     * figure out pool sizes (with 8-padding)
     */
    for(i = 0; i < ms->MS_nSec; i++)
	ms->MS_poolsize[ms->MS_sectPtr[i]->SS_poolindx] += 
	    (ms->MS_sectPtr[i]->SS_size + 7) & ~7;

    /*
     * allocate some memory
     */
    Module_pool_initialize(ms);

    /* 
     * pass 2 over sections. load them into memory.
     */
    shdr = (SCNHDR *) (source + FILHSZ + fileHeader->f_opthdr);
    for(i = 0; i < ms->MS_nSec; shdr++, i++) {
	p_shdr = (SCNHDR *) coffxtrct(shdr, sizeof(SCNHDR));
	Section_load(ms, ms->MS_sectPtr[i], p_shdr, source);
	coffdone(p_shdr, shdr);
    }

    /*
     * Actually assign space to uninitialized variables.
     */
    Symbol_assign_space(ms);
    coffdone(optHeader, source + FILHSZ);
    coffdone(fileHeader, source);
    return ms;
}

extern void
MDestroy(Module *ms)
{
    int i;

    /* check for a valid pointer and make sure it's not the debugModule */
    /* which is allocated statically */
    if(ms == NULL || ms == debugModule)
	return;

    if(ms->MS_sectPtr != NULL)
	spin_free(ms->MS_sectPtr);
    if(ms->MS_intSymPtr) {
	if(ms->MS_intSymPtr->ST_hashPtr != NULL) 
	    spin_free(ms->MS_intSymPtr->ST_hashPtr);
	if(ms->MS_intSymPtr->ST_symBlkPtr != NULL)
	    spin_free(ms->MS_intSymPtr->ST_symBlkPtr);
	if(ms->MS_intSymPtr != NULL)
	    spin_free(ms->MS_intSymPtr);
    }
    if(ms->MS_extSymPtr) {
	if(ms->MS_extSymPtr->ST_hashPtr != NULL)
	    spin_free(ms->MS_extSymPtr->ST_hashPtr);
	if(ms->MS_extSymPtr->ST_symBlkPtr != NULL)
	    spin_free(ms->MS_extSymPtr->ST_symBlkPtr);
	if(ms->MS_extSymPtr != NULL)
	    spin_free(ms->MS_extSymPtr);
    }

    for(i = 0; i < ms->MS_nSec; i++) {
        /* spin_free(ms->MS_sectPtr[i]->SS_name);*/
        /* spin_free(ms->MS_sectPtr[i]); because it's a secalloc ??? */
    }

    /* We allocate only one buffer and assign */
    /* internal pointers to various pools, the first pool with non-zero */
    /* length will however contain the pointer to the begining of the */
    /* whole buffer.  Free that pointer */
    if(ms->MS_poolsize) {
        for(i = 0; i < MAXPOOL; i++) {
	    if(ms->MS_poolsize[i] > 0) {
		spin_free(ms->MS_pool[i]);
		break;
	    }
	}
    }
    spin_free(ms);
}

extern void
UnregisterClean(Module *ms)
{
    int i;

    if(ms == NULL || ms == debugModule)
	return;

    if(ms->MS_poolsize) {
        for(i = 0; i < MAXPOOL; i++) {
	    if(ms->MS_poolsize[i] > 0) {
		unregister_clean(ms->MS_pool[i], ms->MS_poolsize[i]);
	    }
	}
    }
}

long
FullyResolved(Module *m)
{
    if((m == NULL) || (m && m->MS_flags == Resolved_Fully))
	return 1;
    return 0;
}

/*
 * Generate a virtual module from a list of symbols
 */
Module *
Generate(SymbolEntry syms[], int symsize) 
{
    Module *corem;

    corem = lalloc(sizeof(Module));
    bzero((void *) corem, sizeof(Module));

    corem->MS_nSec  = 2;
    
    corem->MS_sectPtr = seclalloc(sizeof(void *) * (corem->MS_nSec + 1));

    corem->MS_sectPtr[0] = seclalloc(sizeof(Section));
    bzero((void *)corem->MS_sectPtr[0], sizeof(Section));
    corem->MS_sectPtr[0]->SS_name = ".text";

    corem->MS_sectPtr[1] = seclalloc(sizeof(Section));
    bzero((void *)corem->MS_sectPtr[1], sizeof(Section));
    corem->MS_sectPtr[1]->SS_name = ".data";
    Symbol_fake_table(corem, syms, symsize);
    corem->MS_flags = Resolved_Fully;
    return corem;
}

/*
 * M3 safe version of generate that takes an M3 open array as its argument.
 */
Module * 
M3SafeGenerate(struct openarray * syms)
{
	return Generate((SymbolEntry*)syms->start,(int)syms->size);
}

extern void
Add_Fake_Symbol(Module *corem, char *name, void *ptr, int text)
{
    SymbolEntry se;

    se.SE_name = name;
    se.SE_ptr = ptr;
    se.SE_nvalue = (long) ptr;
    se.SE_nsclass = text ? scText : scData;

    Symbol_fake_table(corem, &se, 1);
}

extern char *
ExtractModuleName(SymbolEntry *sym) {
    if(sym == NULL)
	return NULL;
    return sym->SE_name + strlen("M3__Link_");
}

extern long
ExtractValue(SymbolEntry *sym) {
    if(sym == NULL)
	return NULL;
    return Symbol_get_value(sym);
}

extern long
Symbol_findvalue(Module *m1, char *name, long *found) {
    SymbolEntry *sym = Symbol_find(m1, name);
    if(sym) {
	*found = 1;
	return Symbol_get_value(sym);
    } else {
	*found = 0;
	return Invalid_Address;
    }
}

int
TextInfo(Module *ms, void **start, long *size)
{
	int i;
	if (ms == 0)
	    return 0;
	for (i = 0;  i < ms->MS_nSec; i++)  {
		if (strcmp(".text", ms->MS_sectPtr[i]->SS_name) == 0) {
			*start = ms->MS_sectPtr[i]->SS_ptr;
			*size = ms->MS_sectPtr[i]->SS_size;
			if (*start == 0 ) goto checkdata;
			return 1;
		}
	}
      checkdata:
	/* no .text.  Since .io files only have .data, look for .data */
	for (i = 0;  i < ms->MS_nSec; i++)  {
		if (strcmp(".data", ms->MS_sectPtr[i]->SS_name) == 0) {
			*start = ms->MS_sectPtr[i]->SS_ptr;
			*size = ms->MS_sectPtr[i]->SS_size;
			return 1;
		}
	}
	return 0;	/* couldn't find anything */
}

extern void
ShowSectionInformation(Module *ms)
{
	int i;
	if (!ms) return;
	for (i = 0;  i < ms->MS_nSec; i++)  {
		printf("\t%s start=0x%lx size=0x%x\n",
		       ms->MS_sectPtr[i]->SS_name,
		       ms->MS_sectPtr[i]->SS_ptr,
		       ms->MS_sectPtr[i]->SS_size);
	}
}

extern int
IsModula3(Module *ms) {
    int i;

    if(ms == NULL)
	return 1;
    if(ms->MS_extSymPtr == NULL)
	return 0;
    for(i = 0; i < ms->MS_extSymPtr->ST_nSyms; ++i) {
        if((ms->MS_extSymPtr->ST_symBlkPtr[i].SE_nsclass == scData) &&
	   ((strncmp(ms->MS_extSymPtr->ST_symBlkPtr[i].SE_name,
		     "MI_", strlen("MI_")) == 0) || 
	    (strncmp(ms->MS_extSymPtr->ST_symBlkPtr[i].SE_name,
		     "MM_", strlen("MM_")) == 0)))
	{
	    return 1;
	}
    }
    return 0;
}

static int 
symbol_prefix(Module *ms, int i, char *prefix)
{
  return (strncmp(ms->MS_extSymPtr->ST_symBlkPtr[i].SE_name,
		  prefix, strlen(prefix)) == 0);
}

static void
add_symbol(Module *ms, int i, void ***list, int *n_desc, int *size)
{
  SymbolEntry *symbol;
  void **tmp;
  int old_size;

  if(*n_desc == *size) {
    old_size = *size;
    *size = (*size == 0) ? 10 : 2 * *size;
    tmp = (void **)symlalloc(*size*sizeof(void *));
    if(*list != NULL) {
      bcopy(*list, tmp, old_size * sizeof(void *));
      bzero(*list, old_size * sizeof(void *));
      free(*list, M_DSYMBOL);
    }
    *list = tmp;
  }
  symbol = &ms->MS_extSymPtr->ST_symBlkPtr[i]; 
  (*list)[*n_desc] = (void *)ExtractValue(symbol);
  (*n_desc)++;
}

extern 
Find_unit_descriptors(Module *ms, 
		      struct openarray **mods,
		      struct openarray **info) 
{
    int i, mods_n_desc, mods_size, info_n_desc, info_size;
    void **mods_list, **info_list;
    static char * interface_prefix = "MI_";
    static char * module_prefix = "MM_";
    static char * interface_link_prefix = "MLink_I_";
    static char * module_link_prefix = "MLink_M_";
    int found = 0;

    mods_n_desc = info_n_desc = 0;
    mods_size   = info_size   = 0;
    mods_list   = info_list   = NULL;

    if(ms == NULL || ms->MS_extSymPtr == NULL) {
      *mods = NULL;
      *info = NULL;
      return;
    }
    for(i = 0; i < ms->MS_extSymPtr->ST_nSyms; ++i) {
        if(ms->MS_extSymPtr->ST_symBlkPtr[i].SE_nsclass == scData) {
	  if(symbol_prefix(ms, i, interface_prefix) ||
	     symbol_prefix(ms, i, module_prefix)) 
	  {
	    add_symbol(ms, i, &mods_list, &mods_n_desc, &mods_size);
	  } else if(symbol_prefix(ms, i, interface_link_prefix) ||
		    symbol_prefix(ms, i, module_link_prefix))
	  {
	    add_symbol(ms, i, &info_list, &info_n_desc, &info_size);
	  }
	}
    }

    if(mods_n_desc != info_n_desc) {
      printf("ERROR >> Find_unit_descriptors: %s: %d units, %d link info\n", 
	     "number of unit and link info descriptors do not match",
	     mods_n_desc, info_n_desc);
      info_n_desc = mods_n_desc = 0;
    }

    if(found) {
      printf("Z >>> %s\n", *(char **)(mods_list[0]));
    }

    if(mods_n_desc == 0) {
      *mods = *info = NULL;
    } else {
      *mods = subarray((void*)mods_list, mods_n_desc, 0);
      *info = subarray((void*)info_list, info_n_desc, 0);
    }
}

extern 
Free_unit_descriptors(struct openarray **mods, struct openarray **info) 
{
  if(*mods != NULL) {
    bzero((*mods)->start, (*mods)->size * sizeof(void *));
    spin_free((*mods)->start);
    subfree(*mods);
    *mods = NULL;
  }
  if(*info != NULL) {
    bzero((*info)->start, (*info)->size * sizeof(void *));
    spin_free((*info)->start);
    subfree(*info);
    *info = NULL;
  }
}

int
Inside(Module *ms, char *addr)
{
  int i;
  int found = 0;
  char *pool;
  
  if(ms != NULL) {
    for(i = 0; i < MAXPOOL; i++) {
      if(ms->MS_poolsize[i] > 0) {
	pool = ms->MS_pool[i];
	if(pool <= addr && (pool + ms->MS_poolsize[i] >= addr)) {
	  if(found) {
	    printf("ERROR >> Module_inside: found twice\n");
	  }
	  found = 1;
	}
      }
    }
  }
  return found;
}

int
Module_GetNextRange(Module *ms, int *idx, char **start, char **stop)
{
  int found = 0;
  char *pool;
  int i;

  if(ms != NULL) {
    for (i = *idx; i < MAXPOOL && !found; i++) {
      if(ms->MS_poolsize[i] > 0) {
	pool   = ms->MS_pool[i];
	*start = pool;
	*stop  = pool + ms->MS_poolsize[i];
	found  = 1;
	(*idx) = i + 1;
      }
    }
  }
  return found;
}

int
Module_GetNextText(Module *ms, int *idx, char **start, char **stop)
{
  char *pool;
  int i;

  if(ms != NULL) {
    while(*idx < ms->MS_nSec)  {
      i = *idx;
      if (strcmp(".text", ms->MS_sectPtr[i]->SS_name) == 0) {
	*start = ms->MS_sectPtr[i]->SS_ptr;
	if(*start != NULL) {
	  *stop  = *start + ms->MS_sectPtr[i]->SS_size;
	  (*idx)++;
	  return 1;
	}
      }
      (*idx)++;
    }
  }
  return 0;
}

/* For the static kernel */
int
Module_GetNextStaticRange(int *idx, char **start, char **stop)
{
  int i;
  extern char alpha_bootstrap, etext, hwrpb_addr, _gp;

  switch (*idx) {
  case 0:
      *start = &alpha_bootstrap;
      *stop = &etext+8;
      *idx = 1;
      return 1;

  case 1:
      *start = &hwrpb_addr;
      *stop = &_gp;
      *idx = 2;
      return 1;
      
  default:
      return 0;
  }
}



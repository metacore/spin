/*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 10-Jul-96  Tian Fung Lim (tian) at the University of Washington
 *	Changed mallocs to use appropriate pools.
 *
 * 07-Nov-95  Emin Gun Sirer (egs) at the University of Washington
 *	Added support for the new RCONST section in OSF 3.2.
 *
 * 12-Aug-95  Emin Gun Sirer (egs) at the University of Washington
 *	Fixed the initialization of nReloc to 0.
 *
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Coff section parsing.
 *
 */
#include "Interface.h"
#include "Section.h"
#include "Module.h"
#include <reloc.h>

static void
Section_analyze_header(Section *ss, SCNHDR *curSec);

static void
Section_get_relocations(Section  *secSum,
			SCNHDR   *curSec,
			char     *source);

/*
 * Create a Section from a COFF Section Header
 * 
 * This routine does not actually copy the raw data for the section.
 * Raw data is handled in a second pass, when all the section sizes
 * are known.
 */
Section *
Section_create(SCNHDR *thisSec, unsigned long aout_entry)
{
    Section *sectSum;

    /*
     * No matter what the contents of a section, we need
     * a Section so that reloc's and symbols that
     * specify a section via index will work.
     */
    sectSum = seclalloc(sizeof(Section));
    sectSum->SS_name = seclalloc(strlen(thisSec->s_name) + 1);
    strcpy(sectSum->SS_name, thisSec->s_name);
    sectSum->SS_nReloc     = 0;
    sectSum->SS_relocPtr   = NULL;
    sectSum->SS_ptr        = NULL;
    sectSum->SS_size       = thisSec->s_size;
    sectSum->SS_baseValue  = thisSec->s_paddr - aout_entry;
    sectSum->SS_flags      = 0;

    Section_analyze_header(sectSum, thisSec);

    return (sectSum);
}

void
Section_load(Module *ms,
	     Section *ss,
	     SCNHDR *thisSec,
	     char *source)
{
    if(ss->SS_flags & SSF_ALLOCATE) {
	ss->SS_ptr     = Module_pool_allocate(ms, ss->SS_poolindx,ss->SS_size);
	ss->SS_flags  |= SSF_ALLOCATED;

	if(ss->SS_flags & SSF_LOAD) {
	    memcpy(ss->SS_ptr, source + thisSec->s_scnptr, ss->SS_size);
	    ss->SS_flags |= SSF_LOADED;
	}
	if(ss->SS_flags & SSF_RELOC_LOAD) {
	    Section_get_relocations(ss, thisSec, source);
	    ss->SS_flags |= SSF_RELOCED_NONE;
	}
    }
}

/*
 * Decide what actions are necessary for the section, 
 * which depends upon what type of section we are dealing with..
 * CALLED BY:	    Section_create
 */
static void
Section_analyze_header(Section *ss, SCNHDR *curSec)
{
    /*
     * Now that the section is safely nestled in its bed,
     * go about the business of getting the interesting
     * contents of the COFF file
     */
    switch (curSec->s_flags & 0x0fff) {
    case STYP_TEXT  :
    case STYP_RDATA :
	ss->SS_poolindx = POOL_TEXT;
	ss->SS_flags |= SSF_ALLOCATE | SSF_RELOC_LOAD | 
	                 SSF_LOAD | SSF_READ_ONLY;
	break;

    case STYP_REG   :
    case STYP_DATA  :
    case STYP_SDATA :
	ss->SS_poolindx = POOL_DATA;
	ss->SS_flags |= SSF_ALLOCATE | SSF_RELOC_LOAD | SSF_LOAD;
	break;

    case STYP_NOLOAD:
	ss->SS_flags |= SSF_ALLOCATE | SSF_RELOC_LOAD;
	break;

    case STYP_BSS   :
    case STYP_SBSS  :
	ss->SS_poolindx = POOL_BSS;
	ss->SS_flags |= SSF_ALLOCATE;
	break;

    case STYP_DSECT :
    case STYP_GROUP :
    case STYP_PAD   :
    case STYP_COPY  :
    case STYP_UCODE :
    default:
	error("bad section type\n");
	break;
    }
}

/*
 * SYNOPSIS:	    Load in relocations for a section
 * CALLED BY:	    Section_create
 */
static void
Section_get_relocations(Section *secSum,
			SCNHDR  *curSec,
			char    *source)
{
    struct reloc *reloc;
    long          i, relocTableSize;

    /*
     * Gather the # of relocations for this section.
     * It may be in the header, or in the 1st reloc
     * entry (if it would otherwise overflow)...
     */
    reloc = (struct reloc *) (source + curSec->s_relptr);

    if(curSec->s_flags & S_NRELOC_OVFL) {
	ASSERT(reloc->r_type == R_ABS);
	secSum->SS_nReloc = reloc->r_vaddr - 1;
	reloc++;
    } else {
	secSum->SS_nReloc = curSec->s_nreloc;
    }
    
    /*
     * Now, allocate the relocation table for the specified
     * section.  With that taken care of, load in all the relocations.
     */
    relocTableSize = secSum->SS_nReloc * sizeof(struct Reloc);
/* FIXME */
    if(relocTableSize == 0) {
      relocTableSize = 2 * sizeof(struct Reloc);
    }
/* FIXME */
    secSum->SS_relocPtr = reloclalloc(relocTableSize);

    for(i=0; i < secSum->SS_nReloc; i++) {
	memcpy(&secSum->SS_relocPtr[i].RS_relocInfo, reloc, sizeof(*reloc));
	secSum->SS_relocPtr[i].RS_patchValue = 0;
	secSum->SS_relocPtr[i].RS_patched = 0;
	reloc++;
    }
}

/*
 * Find a section given its COFF address
 */
extern Section  *
Section_find_by_voffset(Module *ms, long offset)
{
    int i;
    Section  *sec;

    ASSERT(ms);
    for(i = 0; i < ms->MS_nSec; ++i) {
	sec = ms->MS_sectPtr[i];
	if(offset >= sec->SS_baseValue &&
	   offset <= sec->SS_baseValue + sec->SS_size)
	    return sec;
    }
    return NULL;
}

/*
 * Find a section given its name
 */
extern Section  *
Section_find_by_name(Module *ms, char *name)
{
    int i;

    ASSERT(ms);
    ASSERT(name);

    for(i = 0; i < ms->MS_nSec; ++i)
	if(strcmp(name, ms->MS_sectPtr[i]->SS_name) == 0)
	    return ms->MS_sectPtr[i];
    return NULL;
}

/*
 * Find a section given a reference from a reloc entry.
 *
 * this could easily be made more efficient
 */
extern Section *
Section_find_by_reloc_ref(Module  *ms, int offset)
{
    switch(offset % (MAX_R_SN+1)) {
    case R_SN_NULL:
	return Section_find_by_name(ms, ".null");
    case R_SN_TEXT:
	return Section_find_by_name(ms, ".text");
    case R_SN_RDATA:
	return Section_find_by_name(ms, ".rdata");
    case R_SN_DATA:
	return Section_find_by_name(ms, ".data");
    case R_SN_SDATA:
	return Section_find_by_name(ms, ".sdata");
    case R_SN_SBSS:
	return Section_find_by_name(ms, ".sbss");
    case R_SN_BSS:
	return Section_find_by_name(ms, ".bss");
    case R_SN_INIT:
	return Section_find_by_name(ms, ".init");
    case R_SN_LIT8:
	return Section_find_by_name(ms, ".lit8");
    case R_SN_LIT4:
	return Section_find_by_name(ms, ".lit4");
    case R_SN_XDATA:
	return Section_find_by_name(ms, ".xdata");
    case R_SN_PDATA:
	return Section_find_by_name(ms, ".pdata");
    case R_SN_FINI:
	return Section_find_by_name(ms, ".fini");
    case R_SN_LITA:
	return Section_find_by_name(ms, ".lita");
    case R_SN_ABS:
	return Section_find_by_name(ms, ".abs");
    case R_SN_RCONST:
	return Section_find_by_name(ms, ".rconst");
    default:
	/*  offset is an overloaded field so sometimes we wind up here. */
	printf("Unknown section offset %d\n", offset);
	return NULL;
    }
}
		

	    

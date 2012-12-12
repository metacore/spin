/*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
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

static void
Section_get_relocations(Section  *secSum,
			char     *source);

/*
 * Create a Section
 * 
 * This routine does not actually copy the raw data for the section.
 * Raw data is handled in a second pass, when all the section sizes
 * are known.
 */
Section *
Section_create(char *source, char *name)
{
    struct exec *filehdr;
    Section *sectSum;

    filehdr = (struct exec *) source;

    /*
     * No matter what the contents of a section, we need
     * a Section so that reloc's and symbols that
     * specify a section via index will work.
     */
    sectSum = lalloc(sizeof(Section));
    sectSum->SS_name = lalloc(strlen(name) + 1);
    strcpy(sectSum->SS_name, name);
    sectSum->SS_nReloc     = 0;
    sectSum->SS_relocPtr   = NULL;
    sectSum->SS_ptr        = NULL;
    sectSum->SS_flags      = 0;

    if(!strcmp(name, ".text")) {
	sectSum->SS_size = filehdr->a_text;
	sectSum->SS_fileBase = sizeof(struct exec);
	sectSum->SS_linkBase = 0;
	sectSum->SS_poolindx = POOL_TEXT;
	sectSum->SS_flags |= SSF_ALLOCATE | SSF_RELOC_LOAD | 
	    SSF_LOAD | SSF_READ_ONLY;
    } else if(!strcmp(name, ".data")) {
	sectSum->SS_size = filehdr->a_data;
	sectSum->SS_fileBase = sizeof(struct exec) + filehdr->a_text;
	sectSum->SS_linkBase = filehdr->a_text;
	sectSum->SS_poolindx = POOL_DATA;
	sectSum->SS_flags |= SSF_ALLOCATE | SSF_RELOC_LOAD | SSF_LOAD;
    } else if(!strcmp(name, ".bss")) {
	sectSum->SS_size = filehdr->a_bss;
	sectSum->SS_fileBase = 0; /* not in file */
	sectSum->SS_linkBase = filehdr->a_text + filehdr->a_data;
	sectSum->SS_poolindx = POOL_BSS;
	sectSum->SS_flags |= SSF_ALLOCATE;
    } else {
	error("bad section type\n");
    }

    return (sectSum);
}

void
Section_load(Module *ms,
	     Section *ss,
	     char *source)
{
    if(ss->SS_flags & SSF_ALLOCATE) {
	ss->SS_ptr     = Module_pool_allocate(ms, ss->SS_poolindx,ss->SS_size);
	ss->SS_flags  |= SSF_ALLOCATED;

	if(ss->SS_flags & SSF_LOAD) {
	    memcpy(ss->SS_ptr, source + ss->SS_fileBase, ss->SS_size);
	    ss->SS_flags |= SSF_LOADED;
	}
	if(ss->SS_flags & SSF_RELOC_LOAD) {
	    Section_get_relocations(ss, source);
	    ss->SS_flags |= SSF_RELOCED_NONE;
	}
    }
}

/*
 * SYNOPSIS:	    Load in relocations for a section
 * CALLED BY:	    Section_create
 */
static void
Section_get_relocations(Section *secSum,
			char    *source)
{
    struct exec *filehdr;
    struct relocation_info *reloc;
    long i, relocTableSize;
    
    filehdr = (struct exec *) source;

    switch(secSum->SS_poolindx) {
      case POOL_TEXT:
	secSum->SS_nReloc = filehdr->a_trsize / sizeof(struct relocation_info);
	reloc = (struct relocation_info *) (source + sizeof(struct exec) +
					    filehdr->a_text + filehdr->a_data);
	break;
      case POOL_DATA:
	secSum->SS_nReloc = filehdr->a_drsize / sizeof(struct relocation_info);
	reloc = (struct relocation_info *) (source +  sizeof(struct exec) +
					    filehdr->a_text + filehdr->a_data +
					    filehdr->a_trsize);
	break;
      case POOL_BSS:
	error("can't relocate BSS");
	return;
      default:
	error("unknown section");
	return;
    }
    
    /*
     * Now, allocate the relocation table for the specified
     * section.  With that taken care of, load in all the relocations.
     */
    relocTableSize = secSum->SS_nReloc * sizeof(struct Reloc);
    secSum->SS_relocPtr = lalloc(relocTableSize);

    for(i=0; i < secSum->SS_nReloc; i++) {
	memcpy(&secSum->SS_relocPtr[i].RS_relocInfo, reloc, sizeof(*reloc));
	secSum->SS_relocPtr[i].RS_patchValue = 0;
	secSum->SS_relocPtr[i].RS_patched = 0;
	reloc++;
    }
}

/*
 * Find a section given its file address
 */
extern Section  *
Section_find_by_voffset(Module *ms, long offset)
{
    int i;
    Section  *sec;

    ASSERT(ms);
    for(i = 0; i < ms->MS_nSec; ++i) {
	sec = ms->MS_sectPtr[i];
	if(offset >= sec->SS_fileBase &&
	   offset <= sec->SS_fileBase + sec->SS_size)
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
    switch(offset) {
      case N_TEXT:
	return Section_find_by_name(ms, ".text");
      case N_DATA:
	return Section_find_by_name(ms, ".data");
      case N_BSS:
	return Section_find_by_name(ms, ".bss");
      default:
	/* offset is an overloaded field so sometimes we wind up here. */
	printf("Unknown section offset %d\n", offset);
	return NULL;
    }
}
		

	    

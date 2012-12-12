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
 *	Fixed the one-bit drop bug in GPDISP. The calculation is now unsigned.
 *	Added a print in case of memory failure.
 *
 * 18-Jul-95  Emin Gun Sirer (egs) at the University of Washington
 *	Added unresolve support for BRADDR and GPREL32.
 *
 * 07-Jul-95  Emin Gun Sirer (egs) at the University of Washington
 *	Added a special debug domain that identifies unresolved symbols
 *      when linked against.
 *
 * 03-Jul-95  Emin Gun Sirer (egs) at the University of Washington
 *	Fixed the handling of symndx references to lit4 and lit8 sections.
 *
 * 21-Jun-95  Emin Gun Sirer (egs) at the University of Washington
 *	Added code to deal with GPREL32 and BRADDR relocations.
 *
 * 19-Jun-95  Emin Gun Sirer (egs) at the University of Washington
 *	Changed to deal with ABS relocations with negative vaddrs.
 *      Now treats scSUndefined just like scUndefined.
 *      Deal with stack over/underflow. 
 *
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Resolve relocations. Meat of the linker.
 *
 */
#include "Interface.h"
#include "Resolve.h"
#include "Section.h"
#include "Symbol.h"

#undef PRINT
#define PRINT if(linker_debug) printf

int linker_debug = 0;

static int Resolve(Reloc *rel, Section *sec, Module *ms, Module *against);

extern LinkState
Resolve_section(Section *ss, Module *ms, Module *against)
{
    Reloc *reloc;
    int    j, done = 1;
    
    PRINT("Fixing up section %s ", ss->SS_name);
    PRINT("vaddr 0x%lx size 0x%lx ", (long)ss->SS_ptr, ss->SS_size);
    PRINT("relocs %ld relocptr 0x%x\n",ss->SS_nReloc, ss->SS_relocPtr);

    if(ms == NULL || against == NULL)
	return;
    if(ss->SS_nReloc != 0 && ss->SS_relocPtr == NULL) {
	printf("In section %s, reloc count is non-zero (%d), but reloc block is NULL\n", ss->SS_name, ss->SS_nReloc);
	printf("Did someone overwrite memory ???\n");
	return;
    }
    if(against != debugModule) {
	/* do actual linking fast */
	for(j = 0; j < ss->SS_nReloc; ++j) {
	    reloc = &ss->SS_relocPtr[j];
	    if(reloc->RS_patched == 0) {
		done &= Resolve(reloc, ss, ms, against);
	    }
	}
	return ss->SS_flags = (done ? Resolved_Fully: Resolved_Not);
    } else {
	/* print where things go wrong */
	for(j = 0; j < ss->SS_nReloc; ++j) {
	    reloc = &ss->SS_relocPtr[j];
	    if(reloc->RS_patched == 0) {
		if(Resolve(reloc, ss, ms, against) == 0) {
		    /* enable debugging and print what's up */
		    linker_debug = 1;
		    Resolve(reloc, ss, ms, NULL);
		    linker_debug = 0;
		}
	    }
	}
	return ss->SS_flags;
    }
}

long stack[256];  /* for push, store, rshift and sub relocations */
int tos;          /* top of stack */

/*
 * Returns true for successfully resolved relocations
 */
static int
Resolve(Reloc *rel, Section *sec, Module *ms, Module *against)
{
    Section      *ss;
    SymbolEntry  *sym, *sym2;
    int          *relAddress;
    struct relocation_info *relinfo;
    long          word, value, offset;


    ASSERT(rel->RS_patched == 0);

    relinfo = &rel->RS_relocInfo;

    /*
     * We don't handle some of the bits that relate to PIC code.
     */
    if(relinfo->r_baserel) {
	error("r_baserel unsupported\n");
	return 0;
    }

    if(relinfo->r_jmptable) {
	error("r_jmptable unsupported\n");
	return 0;
    }
    
    if(relinfo->r_relative) {
	error("r_relative unsupported\n");
	return 0;
    }

    if(relinfo->r_copy) {
	error("r_copy unsupported\n");
	return 0;
    }

    /*
     * Determine the offset into the "raw data" that this entry
     * refers to and check for out-of-range errors.
     */
    offset = relinfo->r_address;
    if(offset < 0 || offset > sec->SS_size) {
	error("bad reloc - r_vaddr out of bounds\n");
	return 0;
    }

    /*
     * Determine the value for the relocation.
     */
    if(relinfo->r_extern) { 
            /*
             * If r_extern is set then the value for relocation is the
             * value of the external symbol which is at the absolute
             * r_symndex in the object file's original external table.
             * To get a pointer to this symbol the external map for this
             * object file is used which maps absolute external symbol
             * table indexes into pointers to their merge external symbol
             * table entry.
             */
            if(relinfo->r_symbolnum > ms->MS_symPtr->ST_nSyms) {
                error("bad reloc - r_symbolnum[index]\n");
		return 0;
	    }

	    sym = &ms->MS_symPtr->ST_symBlkPtr[relinfo->r_symbolnum];

	    if(sym == NULL) {
		error("relocatable external symbol is NULL\n");
		return 0;
	    }

	    if(sym->SE_nsclass == scUndefined || sym->SE_nsclass == scUndefinedInternal) {
		sym2 = Symbol_find(against, sym->SE_name);
		if(sym2 != NULL && (value = Symbol_get_value(sym2)) != NULL &&
		    (sym2->SE_nsclass <= LastExternalClass)) {
		    PRINT("Resolve: found <%s> 0x%lx\n", sym->SE_name, value);
		} else {
		    PRINT("Resolve: Undefined symbol <%s>\n", sym->SE_name);
		    return 0;
		}
	    } else {
		value = Symbol_get_value(sym);
		PRINT("Resolve: known <%s> 0x%lx\n", sym->SE_name, value);
	    }
    } else {
	/*
	 * internal relocation.
	 *
	 * r_symbolnum now refers to a section number
	 */
	ss = Section_find_by_reloc_ref(ms, relinfo->r_symbolnum);
	if(ss != NULL) {
	    /*
	     * If r_extern is not set then the value for the relocation is
	     * the final object's base address minus the original object
	     * file's base address.  These base addresses are for the
	     * section refered to by the section number in the r_symndx
	     * field.
	     */
	    switch(relinfo->r_symbolnum) {
	      case N_TEXT:
	      case N_DATA:
	      case N_BSS:
		value = (long) ss->SS_ptr - ss->SS_linkBase;
		break;
	      default:
	        PRINT("unknown section index %d\n", relinfo->r_symbolnum);
	    }
	}
    }
    PRINT("offset 0x%02lx, inst 0x%x\t",offset,*(int *)((char*)sec->SS_ptr + offset));
    
    relAddress = (int *) ((char *) sec->SS_ptr + offset);
    rel->RS_patchAddr = relAddress;

    /*
     * if r_pcrel is set then we must subtract the address of the
     * instruction in the object file.
     */
    if(relinfo->r_pcrel) {
      PRINT("PC relative ");
      value -= ((long) relAddress) + 4;

      switch(relinfo->r_length) {
      case 0:
	rel->RS_patchValue = *((char *) relAddress);
	*((char *) relAddress) = (char) value;
	PRINT("patched to 0x%lx\t", (char) value);
	  break;
      case 1:
	rel->RS_patchValue = *((short *) relAddress);
	*((short *) relAddress) = (short) value;
	PRINT("patched to 0x%lx\t", (short) value);
	break;
      case 2:
	rel->RS_patchValue = *((long *) relAddress);
	*((long *) relAddress) = (long) value;
	PRINT("patched to 0x%lx\t", (long) value);
	break;
      default:
	error("unknown relocation size");
	return 0;
      }
      
    } else {
      switch(relinfo->r_length) {
      case 0:
	word = *((char *) relAddress);
	rel->RS_patchValue = word;
	word += value;
	*((char *) relAddress) = (char) word;
	PRINT("patched to 0x%lx\t", (char) word);
	break;
      case 1:
	word = *((short *) relAddress);
	rel->RS_patchValue = word;
	word += value;
	*((short *) relAddress) = (short) word;
	PRINT("patched to 0x%lx\t", (short) word);
	break;
      case 2:
	word = *((long *) relAddress);
	rel->RS_patchValue = word;
	word += value;
	*((long *) relAddress) = (long) word;
	PRINT("patched to 0x%lx\t", (long) word);
	break;
      default:
	error("unknown relocation size");
	return 0;
      }
    }

    PRINT("address 0x%lx symbolnum %d pcrel %d length %d extern %d\n", 
	  relinfo->r_address,
	  relinfo->r_symbolnum,
	  relinfo->r_pcrel,
	  (1 << relinfo->r_length),
	  relinfo->r_extern);

    rel->RS_patched = 1;
    return 1;
}


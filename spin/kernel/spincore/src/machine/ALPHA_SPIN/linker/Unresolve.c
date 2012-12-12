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
#include <machine/inst.h>
#include <symconst.h>
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
    
    PRINT("Unfixing up section %s, start 0x%lx ",ss->SS_name,ss->SS_baseValue);
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
    struct reloc *relinfo;
    int           type, disp;
    signed long   immed;
    long          err, val, mask, word, word_lo, value, offset;

    ASSERT(rel->RS_patched == 1);
    err = 0;
    relinfo = &rel->RS_relocInfo;

    /*
     * Determine the value for the relocation.
     */
    if(relinfo->r_type != R_GPDISP &&
       relinfo->r_type != R_GPVALUE &&
       relinfo->r_extern){
	sym = &ms->MS_extSymPtr->ST_symBlkPtr[relinfo->r_symndx];
	if(sym == NULL) {
	    error("relocatable external symbol is NULL\n");
	    return 0;
	}
	if(sym->SE_nsclass == scUndefined) {
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
    switch(relinfo->r_type) {
    case R_ABS:
	PRINT("R_ABS:\t\t");
	break;
    case R_LITERAL: {
	union alpha_instruction *instr;
	PRINT("R_LITERAL:\t");
	
	bcopy(&rel->RS_patchValue, rel->RS_patchAddr,
	      sizeof(union alpha_instruction));
    }
    case R_LITUSE:
        PRINT("R_LITUSE:\t");
        break;
    case R_GPDISP: {
	union alpha_instruction *lda_instr, *ldah_instr, *tmp;
	PRINT("R_GPDISP:\t");
	
	ldah_instr = (union alpha_instruction *) rel->RS_patchAddr;
	lda_instr = (union alpha_instruction *)((long)rel->RS_patchAddr +
						      relinfo->r_symndx);

	if(lda_instr->m_format.opcode == op_ldah) {
	    tmp = lda_instr;
	    lda_instr = ldah_instr;
	    ldah_instr = tmp;
	}

	if (lda_instr->m_format.opcode != op_lda ||
	    ldah_instr->m_format.opcode != op_ldah) {
	    error("cannot find lda/ldah pair\n");
	    err = 1;
	}
	immed = rel->RS_patchValue;
	lda_instr->m_format.memory_displacement = immed;
	ldah_instr->m_format.memory_displacement = (immed + 32768) / 65536;
	break;
    }
    case R_REFLONG:
	PRINT("R_REFLONG:\t");
	*(int *)relAddress = rel->RS_patchValue;
	PRINT("unpatched to 0x%lx\t", word);
	break;
    case R_REFQUAD:
	PRINT("R_REFQUAD:\t");
	*(long *)relAddress = rel->RS_patchValue;
	PRINT("unpatched to 0x%lx\t", word);
	break;
    case R_GPREL32:
	PRINT("R_GPREL32:\t"); 
	*(signed int *)relAddress = rel->RS_patchValue;
	break;
    case R_BRADDR: {
	union alpha_instruction *instr = (union alpha_instruction *)relAddress;
	PRINT("R_BRADDR(!):\t");
	instr->b_format.branch_displacement = rel->RS_patchValue / 4;
	break;
    }
    case R_HINT: PRINT("R_HINT:\t\t");break;
    case R_SREL16:
	PRINT("R_SREL16(!):\t");
	*(short *)relAddress = rel->RS_patchValue;
	break;
    case R_SREL32:
	PRINT("R_SREL32(!):\t");
	*(int *)relAddress = rel->RS_patchValue;
	break;
    case R_SREL64:
	PRINT("R_SREL64(!):\t");
	*(long *)relAddress = rel->RS_patchValue;
	break;
    case R_OP_PUSH: PRINT("R_OP_PUSH:\t"); break;
    case R_OP_STORE: 
	PRINT("R_OP_STORE:\t");
	val = *(long *)relAddress;

	/* remove target bits from destination word */
	mask = 0xffffffffffffffffUL;
	if (relinfo->r_size != 64)
	    mask &= ((1UL << (unsigned long)relinfo->r_size) - 1UL);
	mask <<= (unsigned long)relinfo->r_offset;
	word &= ~(mask);

	/* select target bits from source and position them */
	if (relinfo->r_size != 64)
	    value = rel->RS_patchValue;

	value <<= (unsigned long)relinfo->r_offset;

	/* or source target bits into destination */
	val |= value;
	*(long *)(relAddress) = val;
	break;
    case R_OP_PSUB: PRINT("R_OP_PSUB:\t"); break;
    case R_OP_PRSHIFT:	PRINT("R_OP_PRSHIFT:\t"); break;
    case R_GPVALUE:	PRINT("R_GPVALUE(!!):\t"); break;
    default: PRINT("UNRECOGNIZED Relocation(!!!!!!!)\t"); err = 1; break;
    }
    if(err == 0)
	rel->RS_patched = 0;
    return err == 0;
}


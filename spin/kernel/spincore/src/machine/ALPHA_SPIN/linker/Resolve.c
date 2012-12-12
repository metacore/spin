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
#include <machine/inst.h>
#include <symconst.h>
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
    
    PRINT("Fixing up section %s, start 0x%lx ", ss->SS_name, ss->SS_baseValue);
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
    struct reloc *relinfo;
    int           type, disp;
    signed long   immed;
    unsigned long uimmed;
    long          err, val, mask, word, word_lo, value, offset;


    ASSERT(rel->RS_patched == 0);

    /* For debugging, set everything to zero */
    type = disp = immed = val = mask = word = word_lo = value = offset = 0;
    err = 0;

    relinfo = &rel->RS_relocInfo;

    /*
     * Determine the offset into the "raw data" that this entry
     * refers to and check for out-of-range errors.
     */
    if(relinfo->r_type != R_ABS && 
       relinfo->r_type != R_OP_PUSH && 
       relinfo->r_type != R_OP_PSUB &&
       relinfo->r_type != R_OP_PRSHIFT) {
	offset = relinfo->r_vaddr - sec->SS_baseValue;
	if(offset < 0 || offset > sec->SS_size || offset & 0x3) {
	    error("bad reloc - r_vaddr out of bounds or unaligned\n");
	    return 0;
	}
    }

    /*
     * Determine the value for the relocation.
     */
    if(relinfo->r_type != R_GPDISP &&
       relinfo->r_type != R_GPVALUE &&
       relinfo->r_extern){
            /*
             * If r_extern is set then the value for relocation is the
             * value of the external symbol which is at the absolute
             * r_symndex in the object file's original external table.
             * To get a pointer to this symbol the external map for this
             * object file is used which maps absolute external symbol
             * table indexes into pointers to their merge external symbol
             * table entry.
             */
            if(relinfo->r_symndx > ms->MS_extSymPtr->ST_nSyms) {
                error("bad reloc - r_symndx[index]\n");
		return 0;
	    }

	    sym = &ms->MS_extSymPtr->ST_symBlkPtr[relinfo->r_symndx];

	    if(sym == NULL) {
		error("relocatable external symbol is NULL\n");
		return 0;
	    }

	    if(sym->SE_nsclass == scUndefined ||
	       sym->SE_nsclass == scSUndefined) {
		sym2 = Symbol_find(against, sym->SE_name);
		if(sym2 != NULL && (value = Symbol_get_value(sym2)) != 
		                    Invalid_Address) {
		    PRINT("Resolve: found <%s> 0x%lx\n", sym->SE_name, value);
		} else {
		    PRINT("Resolve: Undefined symbol <%s>\n", sym->SE_name);
		    return 0;
		}
	    } else {
		value = Symbol_get_value(sym);
		PRINT("Resolve: known <%s> 0x%lx\n", sym->SE_name, value );
	    }
    }
    else if (relinfo->r_type == R_GPVALUE)
	value = relinfo->r_symndx;
    else {
	/*
	 * internal relocation.
	 *
	 * r_symndx now refers to a section number by the R_SN_name
	 */
	ss = Section_find_by_reloc_ref(ms, relinfo->r_symndx);
	if(ss != NULL) {
	    /*
	     * If r_extern is not set then the value for the relocation is
	     * the final object's base address minus the original object
	     * file's base address.  These base addresses are for the
	     * section refered to by the section number in the r_symndx
	     * field.
	     */
	    switch(relinfo->r_symndx){
	    case R_SN_NULL:
	    case R_SN_ABS:
	        value = 0;
	        break;
	    case R_SN_TEXT:
	    case R_SN_INIT:
	    case R_SN_FINI:
	    case R_SN_RDATA:
	    case R_SN_XDATA:
	    case R_SN_PDATA:
	    case R_SN_DATA:
	    case R_SN_SDATA:
	    case R_SN_SBSS:
	    case R_SN_BSS:
	    case R_SN_LIT8:
	    case R_SN_LIT4:
	    case R_SN_RCONST:
	        value = (long) ss->SS_ptr - ss->SS_baseValue;
	        break;
	    case R_SN_LITA:
	        value = (long) ss->SS_ptr;
	        break;
	    default:
	        PRINT("unknown section index %d\n", relinfo->r_symndx);
	    }
	}
    }
    PRINT("offset 0x%02lx, inst 0x%x\t",offset,*(int *)((char*)sec->SS_ptr + offset));
    
    relAddress = (int *) ((char *) sec->SS_ptr + offset);
    rel->RS_patchAddr = relAddress;

    /*
     * factored out preamble
     */
    switch(relinfo->r_type) {
    case R_OP_PUSH:
    case R_OP_PRSHIFT:
    case R_OP_PSUB:
	if (relinfo->r_extern) {
	    relinfo->r_vaddr = value;
	} else {
	    /* local so it's a relative address */
	    relinfo->r_vaddr += value;
	}
	break;
    }

    /* 
     * Real work begins here
     */
    switch(relinfo->r_type) {
    case R_ABS:
	/* 
	 * ABS only occurs after a GPDISP to signal where an LDA 
	 * instruction needs to be patched following an LDAH.
	 *
	 * We use the GPDISP to patch both lda and ldah, so we do nothing here.
	 */
	PRINT("R_ABS:\t\t");
	break;
    case R_LITERAL: {
	union alpha_instruction *instr;
	PRINT("R_LITERAL:\t");
	
	instr = (union alpha_instruction *) relAddress;
	/*
	 * FIX maybe we should check to make sure it is off of gp
	 * but DEC does not.
	 */
	immed = instr->m_format.memory_displacement;
	rel->RS_patchValue = immed;
	PRINT("(original immed 0x%lx) ", immed);
	immed = (ms->MS_oldgp + immed) - ms->MS_gpcoffaddr;
	PRINT("(new immed 0x%lx) ", immed);

	instr->m_format.memory_displacement = immed;
	break;
    }
    case R_LITUSE:
	/* 
	 * This is a relocation entry that allows some optimizations.
	 * Do them when we have more time.
	 */
        PRINT("R_LITUSE:\t");
        break;
    case R_GPDISP: {
	union alpha_instruction *lda_instr, *ldah_instr, *tmp;
	PRINT("R_GPDISP:\t");
	
	ldah_instr = (union alpha_instruction *) relAddress;
	lda_instr = (union alpha_instruction *)((long)relAddress + relinfo->r_symndx);

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

	immed = 65536 * ldah_instr->m_format.memory_displacement +
	                 lda_instr->m_format.memory_displacement;
	rel->RS_patchValue = immed;
	PRINT("(old immed 0x%lx) ", immed);
	uimmed = ms->MS_newgp - (ms->MS_oldgp - immed) - (long) sec->SS_ptr;
	PRINT("(new immed 0x%lx) ", uimmed);

	lda_instr->m_format.memory_displacement = uimmed;
	ldah_instr->m_format.memory_displacement = (uimmed + 32768) / 65536;
	break;
    }
    case R_REFLONG:
	PRINT("R_REFLONG:\t");
	word = *(int *)relAddress;
	rel->RS_patchValue = word;
	word += value;
	if(word < 0x7fffffffL || word < -0x80000000L)
	    error("reflong bigger than 32 bits\n");
	*(int *)relAddress = word;
	PRINT("patched to 0x%lx\t", word);
	break;
    case R_REFQUAD:
	PRINT("R_REFQUAD:\t");
	word = *(long *)relAddress;
	rel->RS_patchValue = word;
	word += value;
	*(long *)relAddress = word;
	PRINT("patched to 0x%lx\t", word);
	break;
    case R_GPREL32:
	PRINT("R_GPREL32:\t");
	word = *(signed int *)relAddress;
	rel->RS_patchValue = word;
	immed = word;

	if(relinfo->r_extern) {
	    /*
	     * For R_GPREF relocation types that are external references
	     * the constant at the entry is just an offset from the
	     * symbol.
	     */
	    immed += value - ms->MS_newgp; /* gp_value; */
	} else {
	    /*
	     * For R_GPREF relocation types that are local references
	     * the constant at the entry is just an offset from the
	     * gp_value for that object file.
	     * src_gp_value - gp_value;
	     */
	    immed += value + ms->MS_oldgp - ms->MS_newgp;
	}
	/*
	 *  The value after relocation for a R_GPREL32 must fit into
	 * a long word.  If not, it is an error.
	 */
	if(immed > (signed long) 0x000000007fffffff || immed < (signed long) 0xffffffff80000000) {
	    printf("gp relocation out of range!!!\n");
	    return 0;
	}
	PRINT("patched to 0x%lx\t", immed);
	*(unsigned int *)relAddress = immed;
	break;

    case R_BRADDR: {
	union alpha_instruction *instr = (union alpha_instruction *)relAddress;
	PRINT("R_BRADDR(!):\t");
	immed = 4 * instr->b_format.branch_displacement;
	rel->RS_patchValue = immed;
	if (relinfo->r_extern == 0) {
	    /*
	     * For R_BRADDR relocation types that are local
	     * references, the constant at the entry is the offset
	     * to the branch target.
	     */
	    immed += relinfo->r_vaddr + 4;
	}

	/*
	 *  Do the relocation and check to see if the target of the
	 *  branch can be reached from the location of the branch.
	 */
	immed += value;             /* immed is now address of destination */

	if (immed & 3) {
	    printf("branch at pc=0x%lx to 0x%lx is not aligned\n",
		   relAddress, immed);
	    immed &= ~3UL;
	}

	/*  Make pc relative  */
	immed -= (unsigned long) relAddress + 4;
	/*  Store in instruction  */
	instr->b_format.branch_displacement = immed/4;
	/*  See if we could store displacement in 21-bits  */
	if (instr->b_format.branch_displacement != immed/4) {
	    printf("branch relocation out-of-range, from 0x%lx to 0x%lx\n",
		   relAddress, relAddress + immed);
	    instr->b_format.branch_displacement = rel->RS_patchValue / 4;
	    return 0;
	}
	break;
    }

    case R_HINT:
	/*
	 * indicates the beginning of a procedure
	 */
	PRINT("R_HINT:\t\t");
	break;

	/* from reloc.h
	 * self relative relocations mean that the memory location at
	 * r_vaddr contains an offset to the destination. If the relocation
	 * is r_extern==1, then the value at the memory location is ignored
	 * (maybe we should allow offsets?). If r_extern==0, then the value
	 * at the memory location is the actual offset. 
	 *
	 * The linker uses the relocated target and a relocated r_vaddr to
	 * determine the offset. Offsets are considered signed.
	 */
    case R_SREL16:
	PRINT("R_SREL16(!):\t");
	word = *(short *)relAddress;
	rel->RS_patchValue = word;
	word += value;
	*(short *)relAddress = word;
	break;
    case R_SREL32:
	PRINT("R_SREL32(!):\t");
	word = *(int *)relAddress;
	rel->RS_patchValue = word;
	word += value;
	*(int *)relAddress = word;
	break;
    case R_SREL64:
	PRINT("R_SREL64(!):\t");
	word = *(long *)relAddress;
	rel->RS_patchValue = word;
	word += value;
	*(long *)relAddress = word;
	break;
    case R_OP_PUSH:               /* stack[++tos] = relocate(vaddr) */
	PRINT("R_OP_PUSH:\t");
	if(tos >= asize(stack)) {
	    PRINT("stack overflow\n");
	    return 0;
	}
	stack[++tos] = relinfo->r_vaddr;
	break;
    case R_OP_STORE:              /* vaddr(r_offset:r_size) = stack[tos--] */
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
	    value = stack[tos] & ((1UL << (unsigned long)relinfo->r_size) - 1UL);
	value <<= (unsigned long)relinfo->r_offset;

	rel->RS_patchValue = value;
	/* or source target bits into destination */
	val |= value;

	if(tos-- < -1) {
	    PRINT("stack underflow\n");
	    return 0;
	}
	*(long *)(relAddress) = val;
	break;
    case R_OP_PSUB:
	/*
	 * stack[tos] = stack[tos] - relocate(vaddr) 
	 */
	PRINT("R_OP_PSUB:\t");
	stack[tos] -= - relinfo->r_vaddr;
	break;
    case R_OP_PRSHIFT:
	/*
	 * stack[tos] = stack[tos] >> relocate(vaddr)
	 */
	PRINT("R_OP_PRSHIFT:\t");
	stack[tos] >>= (unsigned long) relinfo->r_vaddr;
	break;
    case R_GPVALUE:
	/*
	 *  This relocation signifies a new gp value range is
	 *  in effect.
	 */
	PRINT("R_GPVALUE(!!):\t"); err = 1;
	break;
    default:
	PRINT("UNRECOGNIZED Relocation(!!!!!!!)\t"); err = 1;
	break;
    }
    PRINT("extern %d vaddr 0x%lx symndx 0x%x offset 0x%x size 0x%x\n", 
	   relinfo->r_extern,
	   relinfo->r_vaddr,
	   relinfo->r_symndx,
	   relinfo->r_offset,
	   relinfo->r_size);

    if(err == 0)
	rel->RS_patched = 1;
    return err == 0;
}


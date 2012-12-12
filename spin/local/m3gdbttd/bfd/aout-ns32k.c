/* Written by Ian Dall
 *            5-Jun-94
 * HISTORY
 * 05-Jun-94  Ian Dall (idall@eleceng.adelaide.edu.au) at University of Adelaide
 *	This code was formerly in aout-pc532-mach.c. This organisations
 *	is intended to allow multiple format support more easilly.
 *
 */

#define BYTES_IN_WORD 4

#include "bfd.h"
#include "aout/aout64.h"
#include "aout-ns32k.h"
#define MY(OP) MYNS(OP)


#define MY_swap_std_reloc_in MY(swap_std_reloc_in)
#define MY_swap_std_reloc_out MY(swap_std_reloc_out)

static void
MY_swap_std_reloc_in PARAMS ((bfd *abfd, struct reloc_std_external *bytes,
			      arelent *cache_ptr, asymbol **symbols));

static void
MY_swap_std_reloc_out PARAMS((bfd *abfd, arelent *g,
			      struct reloc_std_external *natptr));


/* The ns32k series is ah, unusual, when it comes to relocation.
 * There are three storage methods for relocateable objects.  There
 * are displacements, immediate operands and ordinary twos complement
 * data. Of these, only the last fits into the standard relocation
 * scheme.  Immediate operands are stored huffman encoded and
 * immediate operands are stored big endian (where as the natural byte
 * order is little endian for this achitecture).

 * Note that the ns32k displacement storage method is orthogonal to
 * whether the relocation is pc relative or not. The "displacement"
 * storage scheme is used for essentially all address constants. The
 * displacement can be relative to zero (absolute displacement),
 * relative to the pc (pc relative), the stack pointer, the frame
 * pointer, the static base register and general purpose register etc.

 * For example:
 *
 *  sym1: .long .	# pc relative 2's complement
 *  sym1: .long foo	# 2's complement not pc relative
 *
 *  self:  movd @self, r0 # pc relative displacement
 *	   movd foo, r0 # non pc relative displacement
 *
 *  self:  movd self, r0 # pc relative immediate
 *         movd foo, r0 # non pc relative immediate
 *
 * In addition, for historical reasons the encoding of the relocation types
 * in the a.out format relocation entries is such that even the relocation
 * methods which are standard are not encoded the standard way.
 *
 */
 

bfd_reloc_status_type
 ns32k_reloc_disp(bfd *abfd, arelent *reloc_entry,
		  struct symbol_cache_entry *symbol,
		  PTR data,
		  asection *input_section,
		  bfd *output_bfd,
		  char **error_message);
bfd_reloc_status_type
  ns32k_reloc_imm (bfd *abfd,
		   arelent *reloc_entry,
		   struct symbol_cache_entry *symbol,
		   PTR data,
		   asection *input_section,
		   bfd *output_bfd,
		   char **error_message);
bfd_reloc_status_type
  ns32k_reloc_contents_disp (const reloc_howto_type *howto,
		    bfd *input_bfd,
		    bfd_vma relocation,
		    bfd_byte *location);
bfd_reloc_status_type
  ns32k_reloc_contents_imm (const reloc_howto_type *howto,
		    bfd *input_bfd,
		    bfd_vma relocation,
		    bfd_byte *location);

reloc_howto_type MY(howto_table)[] = 
{
/* ns32k immediate operands */
HOWTO2(BFD_RELOC_NS32K_IMM_8, 0, 0, 8, false, 0, true,
       ns32k_reloc_imm, ns32k_reloc_contents_imm,"NS32K_IMM_8",
       true, 0x000000ff,0x000000ff, false),
HOWTO2(BFD_RELOC_NS32K_IMM_16, 0, 1, 16, false, 0, true,
       ns32k_reloc_imm, ns32k_reloc_contents_imm, "NS32K_IMM_16",
       true, 0x0000ffff,0x0000ffff, false),
HOWTO2(BFD_RELOC_NS32K_IMM_32, 0, 2, 32, false, 0, true,
       ns32k_reloc_imm, ns32k_reloc_contents_imm,"NS32K_IMM_32",
       true, 0xffffffff,0xffffffff, false),
HOWTO2(BFD_RELOC_NS32K_IMM_8_PCREL, 0, 0, 8, true, 0, false,
       ns32k_reloc_imm, ns32k_reloc_contents_imm,"PCREL_NS32K_IMM_8",
       true, 0x000000ff, 0x000000ff, false),
HOWTO2(BFD_RELOC_NS32K_IMM_16_PCREL, 0, 1, 16, true, 0, false,
       ns32k_reloc_imm, ns32k_reloc_contents_imm,"PCREL_NS32K_IMM_16",
       true, 0x0000ffff,0x0000ffff, false),
HOWTO2(BFD_RELOC_NS32K_IMM_32_PCREL, 0, 2, 32, true, 0, false,
       ns32k_reloc_imm, ns32k_reloc_contents_imm,"PCREL_NS32K_IMM_32",
       true, 0xffffffff,0xffffffff, false),

/* ns32k displacements */
HOWTO2(BFD_RELOC_NS32K_DISP_8, 0, 0, 8, false, 0, true,
       ns32k_reloc_disp, ns32k_reloc_contents_disp,"NS32K_DISP_8",
       true, 0x000000ff,0x000000ff, false),
HOWTO2(BFD_RELOC_NS32K_DISP_16, 0, 1, 16, false, 0, true,
       ns32k_reloc_disp, ns32k_reloc_contents_disp,"NS32K_DISP_16",
       true, 0x0000ffff, 0x0000ffff, false),
HOWTO2(BFD_RELOC_NS32K_DISP_32, 0, 2, 32, false, 0, true,
       ns32k_reloc_disp, ns32k_reloc_contents_disp,"NS32K_DISP_32",
       true, 0xffffffff, 0xffffffff, false),
HOWTO2(BFD_RELOC_NS32K_DISP_8_PCREL, 0, 0, 8, true, 0, false,
       ns32k_reloc_disp, ns32k_reloc_contents_disp,"PCREL_NS32K_DISP_8",
       true, 0x000000ff,0x000000ff, false),
HOWTO2(BFD_RELOC_NS32K_DISP_16_PCREL, 0, 1, 16, true, 0, false,
       ns32k_reloc_disp, ns32k_reloc_contents_disp,"PCREL_NS32K_DISP_16",
       true, 0x0000ffff,0x0000ffff, false),
HOWTO2(BFD_RELOC_NS32K_DISP_32_PCREL, 0, 2, 32, true, 0, false,
       ns32k_reloc_disp, ns32k_reloc_contents_disp,"PCREL_NS32K_DISP_32",
       true, 0xffffffff,0xffffffff, false),

/* Normal 2's complement */
HOWTO(BFD_RELOC_8, 0, 0, 8, false, 0, complain_overflow_bitfield,0,
      "8", true, 0x000000ff,0x000000ff, false),
HOWTO(BFD_RELOC_16, 0, 1, 16, false, 0, complain_overflow_bitfield,0,
      "16", true, 0x0000ffff,0x0000ffff, false),
HOWTO(BFD_RELOC_32, 0, 2, 32, false, 0, complain_overflow_bitfield,0,
      "32", true, 0xffffffff,0xffffffff, false),
HOWTO(BFD_RELOC_8_PCREL, 0, 0, 8, true, 0, complain_overflow_signed, 0,
      "PCREL_8", true, 0x000000ff,0x000000ff, false),
HOWTO(BFD_RELOC_16_PCREL, 0, 1, 16, true, 0, complain_overflow_signed, 0,
      "PCREL_16", true, 0x0000ffff,0x0000ffff, false),
HOWTO(BFD_RELOC_32_PCREL, 0, 2, 32, true, 0, complain_overflow_signed, 0,
      "PCREL_32", true, 0xffffffff,0xffffffff, false),
};

#define MY_reloc_howto(BFD,REL,IN,EX,PC) ({ \
  unsigned int r_length; \
  int r_ns32k_type; \
  BFD_ASSERT((BFD)->xvec->header_byteorder_big_p == false); \
  IN =  ((REL)->r_index[2] << 16) \
    | ((REL)->r_index[1] << 8) \
      |  (REL)->r_index[0]; \
  EX  = (0 != ((REL)->r_type[0] & RELOC_STD_BITS_EXTERN_LITTLE)); \
  PC   = (0 != ((REL)->r_type[0] & RELOC_STD_BITS_PCREL_LITTLE)); \
  r_length  =       ((REL)->r_type[0] & RELOC_STD_BITS_LENGTH_LITTLE) \
    >> RELOC_STD_BITS_LENGTH_SH_LITTLE; \
  r_ns32k_type  =    ((REL)->r_type[0] & RELOC_STD_BITS_NS32K_TYPE_LITTLE) \
    >> RELOC_STD_BITS_NS32K_TYPE_SH_LITTLE; \
  MY(howto_table) + r_length + 3 * (PC) + 6 * r_ns32k_type;})

#define MY_put_reloc(BFD, EXT, IDX, VAL, HOWTO, RELOC) {  \
  unsigned int r_length; \
  int r_pcrel; \
  int r_ns32k_type; \
  PUT_WORD ((BFD), (VAL), (RELOC)->r_address); \
  r_length = (HOWTO)->size ;	/* Size as a power of two */ \
  r_pcrel  = (int) (HOWTO)->pc_relative; /* Relative to PC? */ \
  r_ns32k_type = ((HOWTO) - MY(howto_table) )/6; \
  BFD_ASSERT ((BFD)->xvec->header_byteorder_big_p == false); \
  (RELOC)->r_index[2] = (IDX) >> 16; \
  (RELOC)->r_index[1] = (IDX) >> 8; \
  (RELOC)->r_index[0] = (IDX); \
  (RELOC)->r_type[0] = \
    ((EXT)?    RELOC_STD_BITS_EXTERN_LITTLE: 0) \
      | (r_pcrel?     RELOC_STD_BITS_PCREL_LITTLE: 0) \
	| (r_length <<  RELOC_STD_BITS_LENGTH_SH_LITTLE) \
	  | (r_ns32k_type <<  RELOC_STD_BITS_NS32K_TYPE_SH_LITTLE);}


#define RELOC_STD_BITS_NS32K_TYPE_BIG 0x06
#define RELOC_STD_BITS_NS32K_TYPE_LITTLE 0x60
#define RELOC_STD_BITS_NS32K_TYPE_SH_BIG 1
#define RELOC_STD_BITS_NS32K_TYPE_SH_LITTLE 5

#define STAT_FOR_EXEC
#include <aoutx.h>

CONST struct reloc_howto_struct *
DEFUN(MY(bfd_reloc_type_lookup),(abfd,code),
      bfd *abfd AND
      bfd_reloc_code_real_type code)
{
#define ENTRY(i,j)	case i: return &MY(howto_table)[j]
  int ext = obj_reloc_entry_size (abfd) == RELOC_EXT_SIZE;
  BFD_ASSERT(ext == 0);
  if (code == BFD_RELOC_CTOR)
    switch (bfd_get_arch_info (abfd)->bits_per_address)
      {
      case 32:
	code = BFD_RELOC_32;
	break;
      }
  switch (code)
    {
      ENTRY(BFD_RELOC_NS32K_IMM_8, 0);
      ENTRY(BFD_RELOC_NS32K_IMM_16, 1);
      ENTRY(BFD_RELOC_NS32K_IMM_32, 2);
      ENTRY(BFD_RELOC_NS32K_IMM_8_PCREL, 3);
      ENTRY(BFD_RELOC_NS32K_IMM_16_PCREL, 4);
      ENTRY(BFD_RELOC_NS32K_IMM_32_PCREL, 5);
      ENTRY(BFD_RELOC_NS32K_DISP_8, 6);
      ENTRY(BFD_RELOC_NS32K_DISP_16, 7);
      ENTRY(BFD_RELOC_NS32K_DISP_32, 8);
      ENTRY(BFD_RELOC_NS32K_DISP_8_PCREL, 9);
      ENTRY(BFD_RELOC_NS32K_DISP_16_PCREL, 10);
      ENTRY(BFD_RELOC_NS32K_DISP_32_PCREL, 11);
      ENTRY(BFD_RELOC_8, 12);
      ENTRY(BFD_RELOC_16, 13);
      ENTRY(BFD_RELOC_32, 14);
      ENTRY(BFD_RELOC_8_PCREL, 15);
      ENTRY(BFD_RELOC_16_PCREL, 16);
      ENTRY(BFD_RELOC_32_PCREL, 17);
    default: return (CONST struct reloc_howto_struct *) 0;
    }
#undef ENTRY
}


static void
DEFUN(MY_swap_std_reloc_in, (abfd, bytes, cache_ptr, symbols),
  bfd *abfd AND
  struct reloc_std_external *bytes AND
  arelent *cache_ptr AND
  asymbol **symbols)
{
  int r_index;
  int r_extern;
  unsigned int r_length;
  int r_pcrel;
  int r_ns32k_type;
  struct aoutdata  *su = &(abfd->tdata.aout_data->a);

  cache_ptr->address = bfd_h_get_32 (abfd, bytes->r_address);

  /* now the fun stuff */

  cache_ptr->howto = MY_reloc_howto(abfd, bytes, r_index, r_extern, r_pcrel);

  MOVE_ADDRESS(0);
}


static void
DEFUN(MY_swap_std_reloc_out,(abfd, g, natptr),
      bfd *abfd AND
      arelent *g AND
      struct reloc_std_external *natptr)
{
  int r_index;
  asymbol *sym = *(g->sym_ptr_ptr);
  int r_extern;
  unsigned int r_length;
  int r_pcrel;
  int r_ns32k_type;
  unsigned int r_addend;
  asection *output_section = sym->section->output_section;


  r_addend = g->addend + (*(g->sym_ptr_ptr))->section->output_section->vma;
    
  /* name was clobbered by aout_write_syms to be symbol index */

  /* If this relocation is relative to a symbol then set the 
     r_index to the symbols index, and the r_extern bit.

     Absolute symbols can come in in two ways, either as an offset
     from the abs section, or as a symbol which has an abs value.
     check for that here
     */
     

  if (bfd_is_com_section (output_section)
      || output_section == &bfd_abs_section
      || output_section == &bfd_und_section) 
    {
      if (bfd_abs_section.symbol == sym)
      {
	/* Whoops, looked like an abs symbol, but is really an offset
	   from the abs section */
	r_index = 0;
	r_extern = 0;
       }
      else 
      {
	/* Fill in symbol */
	r_extern = 1;
#define KEEPIT flags
	r_index =  stoi((*(g->sym_ptr_ptr))->KEEPIT);
#undef KEEPIT     
      }
    }
  else 
    {
      /* Just an ordinary section */
      r_extern = 0;
      r_index  = output_section->target_index;      
    }

  MY_put_reloc(abfd, r_extern, r_index, g->address, g->howto, natptr);
}

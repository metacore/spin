/* SPARC-specific support for 64-bit ELF
   Copyright 1993 Free Software Foundation, Inc.

This file is part of BFD, the Binary File Descriptor library.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* We need a published ABI spec for this.  Until one comes out, don't
   assume this'll remain unchanged forever.  */

#include "bfd.h"
#include "sysdep.h"
#include "libbfd.h"
#include "libelf.h"

enum reloc_type
  {
    R_SPARC_NONE = 0,
    R_SPARC_8, R_SPARC_16, R_SPARC_32,
    R_SPARC_DISP8, R_SPARC_DISP16, R_SPARC_DISP32,
    R_SPARC_WDISP30, R_SPARC_WDISP22,
    R_SPARC_HI22, R_SPARC_22,
    R_SPARC_13, R_SPARC_LO10,
    R_SPARC_GOT10, R_SPARC_GOT13, R_SPARC_GOT22,
    R_SPARC_PC10, R_SPARC_PC22,
    R_SPARC_WPLT30,
    R_SPARC_COPY,
    R_SPARC_GLOB_DAT, R_SPARC_JMP_SLOT,
    R_SPARC_RELATIVE,
    R_SPARC_UA32,

    /* v9 relocs */
    R_SPARC_10, R_SPARC_11, R_SPARC_64,
    R_SPARC_OLO10, R_SPARC_HH22, R_SPARC_HM10, R_SPARC_LM22,
    R_SPARC_PC_HH22, R_SPARC_PC_HM10, R_SPARC_PC_LM22,
    R_SPARC_WDISP16, R_SPARC_WDISP19,
    R_SPARC_GLOB_JMP, R_SPARC_LO7,

    R_SPARC_max
  };

#if 0
static CONST char *CONST reloc_type_names[] =
{
  "R_SPARC_NONE",
  "R_SPARC_8", "R_SPARC_16", "R_SPARC_32",
  "R_SPARC_DISP8", "R_SPARC_DISP16", "R_SPARC_DISP32",
  "R_SPARC_WDISP30", "R_SPARC_WDISP22",
  "R_SPARC_HI22", "R_SPARC_22",
  "R_SPARC_13", "R_SPARC_LO10",
  "R_SPARC_GOT10", "R_SPARC_GOT13", "R_SPARC_GOT22",
  "R_SPARC_PC10", "R_SPARC_PC22",
  "R_SPARC_WPLT30",
  "R_SPARC_COPY",
  "R_SPARC_GLOB_DAT", "R_SPARC_JMP_SLOT",
  "R_SPARC_RELATIVE",
  "R_SPARC_UA32",

  "R_SPARC_10", "R_SPARC_11", "R_SPARC_64",
  "R_SPARC_OLO10", "R_SPARC_HH22", "R_SPARC_HM10", "R_SPARC_LM22",
  "R_SPARC_PC_HH22", "R_SPARC_PC_HM10", "R_SPARC_PC_LM22",
  "R_SPARC_WDISP16", "R_SPARC_WDISP19",
  "R_SPARC_GLOB_JMP", "R_SPARC_LO7",
};
#endif

extern void abort ();
#define DIE	((bfd_reloc_status_type(*)())abort)

static reloc_howto_type elf_sparc_howto_table[] =
{
  HOWTO (R_SPARC_NONE, 0, 0, 0, false, 0, complain_overflow_dont, bfd_elf_generic_reloc, "R_SPARC_NONE", false, 0, 0x00000000, true),
  HOWTO (R_SPARC_8, 0, 0, 8, false, 0, complain_overflow_bitfield, bfd_elf_generic_reloc, "R_SPARC_8", false, 0, 0x000000ff, true),
  HOWTO (R_SPARC_16, 0, 1, 16, false, 0, complain_overflow_bitfield, bfd_elf_generic_reloc, "R_SPARC_16", false, 0, 0x0000ffff, true),
  HOWTO (R_SPARC_32, 0, 2, 32, false, 0, complain_overflow_bitfield, bfd_elf_generic_reloc, "R_SPARC_32", false, 0, 0xffffffff, true),
  HOWTO (R_SPARC_DISP8, 0, 0, 8, true, 0, complain_overflow_signed, bfd_elf_generic_reloc, "R_SPARC_DISP8", false, 0, 0x000000ff, true),
  HOWTO (R_SPARC_DISP16, 0, 1, 16, true, 0, complain_overflow_signed, bfd_elf_generic_reloc, "R_SPARC_DISP16", false, 0, 0x0000ffff, true),
  HOWTO (R_SPARC_DISP32, 0, 2, 32, true, 0, complain_overflow_signed, bfd_elf_generic_reloc, "R_SPARC_DISP32", false, 0, 0x00ffffff, true),
  HOWTO (R_SPARC_WDISP30, 2, 2, 30, true, 0, complain_overflow_signed, bfd_elf_generic_reloc, "R_SPARC_WDISP30", false, 0, 0x3fffffff, true),
  HOWTO (R_SPARC_WDISP22, 2, 2, 22, true, 0, complain_overflow_signed, bfd_elf_generic_reloc, "R_SPARC_WDISP22", false, 0, 0x003fffff, true),
  HOWTO (R_SPARC_HI22, 10, 2, 22, false, 0, complain_overflow_dont, bfd_elf_generic_reloc, "R_SPARC_HI22", false, 0, 0x003fffff, true),
  HOWTO (R_SPARC_22, 0, 2, 22, false, 0, complain_overflow_bitfield, bfd_elf_generic_reloc, "R_SPARC_22", false, 0, 0x003fffff, true),
  HOWTO (R_SPARC_13, 0, 2, 13, false, 0, complain_overflow_bitfield, bfd_elf_generic_reloc, "R_SPARC_13", false, 0, 0x00001fff, true),
  HOWTO (R_SPARC_LO10, 0, 2, 10, false, 0, complain_overflow_dont, bfd_elf_generic_reloc, "R_SPARC_LO10", false, 0, 0x000003ff, true),
  HOWTO (R_SPARC_GOT10, 0, 2, 10, false, 0, complain_overflow_bitfield, bfd_elf_generic_reloc, "R_SPARC_GOT10", false, 0, 0x000003ff, true),
  HOWTO (R_SPARC_GOT13, 0, 2, 13, false, 0, complain_overflow_bitfield, bfd_elf_generic_reloc, "R_SPARC_GOT13", false, 0, 0x00001fff, true),
  HOWTO (R_SPARC_GOT22, 10, 2, 22, false, 0, complain_overflow_bitfield, bfd_elf_generic_reloc, "R_SPARC_GOT22", false, 0, 0x003fffff, true),
  HOWTO (R_SPARC_PC10, 0, 2, 10, false, 0, complain_overflow_bitfield, bfd_elf_generic_reloc, "R_SPARC_PC10", false, 0, 0x000003ff, true),
  HOWTO (R_SPARC_PC22, 0, 2, 22, false, 0, complain_overflow_bitfield, bfd_elf_generic_reloc, "R_SPARC_PC22", false, 0, 0x003fffff, true),
  HOWTO (R_SPARC_WPLT30, 0, 0, 00, false, 0, complain_overflow_dont, bfd_elf_generic_reloc, "R_SPARC_WPLT30", false, 0, 0x00000000, true),
  HOWTO (R_SPARC_COPY, 0, 0, 00, false, 0, complain_overflow_dont, bfd_elf_generic_reloc, "R_SPARC_COPY", false, 0, 0x00000000, true),
  HOWTO (R_SPARC_GLOB_DAT, 0, 0, 00, false, 0, complain_overflow_dont, bfd_elf_generic_reloc, "R_SPARC_GLOB_DAT", false, 0, 0x00000000, true),
  HOWTO (R_SPARC_JMP_SLOT, 0, 0, 00, false, 0, complain_overflow_dont, bfd_elf_generic_reloc, "R_SPARC_JMP_SLOT", false, 0, 0x00000000, true),
  HOWTO (R_SPARC_RELATIVE, 0, 0, 00, false, 0, complain_overflow_dont, bfd_elf_generic_reloc, "R_SPARC_RELATIVE", false, 0, 0x00000000, true),
  HOWTO (R_SPARC_UA32, 0, 0, 00, false, 0, complain_overflow_dont, bfd_elf_generic_reloc, "R_SPARC_UA32", false, 0, 0x00000000, true),
  HOWTO (R_SPARC_10, 0, 2, 10, false, 0, complain_overflow_bitfield, bfd_elf_generic_reloc, "R_SPARC_10", false, 0, 0x000003ff, true),
  HOWTO (R_SPARC_11, 0, 2, 11, false, 0, complain_overflow_bitfield, bfd_elf_generic_reloc, "R_SPARC_11", false, 0, 0x000007ff, true),
  HOWTO (R_SPARC_64, 0, 4, 00, false, 0, complain_overflow_bitfield, bfd_elf_generic_reloc, "R_SPARC_64", false, 0, (((bfd_vma) 0xffffffff) << 32) + 0xffffffff, true),
  HOWTO (R_SPARC_OLO10, 0, 2, 10, false, 0, complain_overflow_dont, bfd_elf_generic_reloc, "R_SPARC_OLO10", false, 0, 0x000003ff, true),
  HOWTO (R_SPARC_HH22, 42, 2, 22, false, 0, complain_overflow_dont, bfd_elf_generic_reloc, "R_SPARC_HH22", false, 0, 0x003fffff, true),
  HOWTO (R_SPARC_HM10, 32, 2, 10, false, 0, complain_overflow_dont, bfd_elf_generic_reloc, "R_SPARC_HM10", false, 0, 0x000003ff, true),
  HOWTO (R_SPARC_LM22, 10, 2, 22, false, 0, complain_overflow_dont, bfd_elf_generic_reloc, "R_SPARC_LM22", false, 0, 0x003fffff, true),
  HOWTO (R_SPARC_PC_HH22, 42, 2, 22, true, 0, complain_overflow_dont, bfd_elf_generic_reloc, "R_SPARC_HH22", false, 0, 0x003fffff, true),
  HOWTO (R_SPARC_PC_HM10, 32, 2, 10, true, 0, complain_overflow_dont, bfd_elf_generic_reloc, "R_SPARC_HM10", false, 0, 0x000003ff, true),
  HOWTO (R_SPARC_PC_LM22, 10, 2, 22, true, 0, complain_overflow_dont, bfd_elf_generic_reloc, "R_SPARC_LM22", false, 0, 0x003fffff, true),
  HOWTO (R_SPARC_WDISP16, 2, 2, 16, true, 0, complain_overflow_signed, bfd_elf_generic_reloc, "R_SPARC_WDISP16", false, 0, 0, true),
  HOWTO (R_SPARC_WDISP19, 2, 2, 22, true, 0, complain_overflow_signed, bfd_elf_generic_reloc, "R_SPARC_WDISP19", false, 0, 0x0007ffff, true),
  HOWTO (R_SPARC_GLOB_JMP, 0, 0, 00, false, 0, complain_overflow_dont, bfd_elf_generic_reloc, "R_SPARC_GLOB_DAT", false, 0, 0x00000000, true),
  HOWTO (R_SPARC_LO7, 0, 2, 7, false, 0, complain_overflow_dont, bfd_elf_generic_reloc, "R_SPARC_LO7", false, 0, 0x0000007f, true),
};

struct elf_reloc_map
  {
    unsigned char bfd_reloc_val;
    unsigned char elf_reloc_val;
  };

static CONST struct elf_reloc_map sparc_reloc_map[] =
{
  {BFD_RELOC_NONE, R_SPARC_NONE,},
  {BFD_RELOC_16, R_SPARC_16,},
  {BFD_RELOC_8, R_SPARC_8},
  {BFD_RELOC_8_PCREL, R_SPARC_DISP8},
  {BFD_RELOC_CTOR, R_SPARC_32},	/* @@ Assumes 32 bits.  */
  {BFD_RELOC_32, R_SPARC_32},
  {BFD_RELOC_32_PCREL, R_SPARC_DISP32},
  {BFD_RELOC_HI22, R_SPARC_HI22},
  {BFD_RELOC_LO10, R_SPARC_LO10,},
  {BFD_RELOC_32_PCREL_S2, R_SPARC_WDISP30},
  {BFD_RELOC_SPARC22, R_SPARC_22},
  {BFD_RELOC_SPARC13, R_SPARC_13},
  {BFD_RELOC_SPARC_GOT10, R_SPARC_GOT10},
  {BFD_RELOC_SPARC_GOT13, R_SPARC_GOT13},
  {BFD_RELOC_SPARC_GOT22, R_SPARC_GOT22},
  {BFD_RELOC_SPARC_PC10, R_SPARC_PC10},
  {BFD_RELOC_SPARC_PC22, R_SPARC_PC22},
  {BFD_RELOC_SPARC_WPLT30, R_SPARC_WPLT30},
  {BFD_RELOC_SPARC_COPY, R_SPARC_COPY},
  {BFD_RELOC_SPARC_GLOB_DAT, R_SPARC_GLOB_DAT},
  {BFD_RELOC_SPARC_JMP_SLOT, R_SPARC_JMP_SLOT},
  {BFD_RELOC_SPARC_RELATIVE, R_SPARC_RELATIVE},
  {BFD_RELOC_SPARC_WDISP22, R_SPARC_WDISP22},
/*  { BFD_RELOC_SPARC_UA32, R_SPARC_UA32 }, not used?? */
  {BFD_RELOC_SPARC_10, R_SPARC_10},
  {BFD_RELOC_SPARC_11, R_SPARC_11},
  {BFD_RELOC_SPARC_64, R_SPARC_64},
  {BFD_RELOC_SPARC_OLO10, R_SPARC_OLO10},
  {BFD_RELOC_SPARC_HH22, R_SPARC_HH22},
  {BFD_RELOC_SPARC_HM10, R_SPARC_HM10},
  {BFD_RELOC_SPARC_LM22, R_SPARC_LM22},
  {BFD_RELOC_SPARC_PC_HH22, R_SPARC_PC_HH22},
  {BFD_RELOC_SPARC_PC_HM10, R_SPARC_PC_HM10},
  {BFD_RELOC_SPARC_PC_LM22, R_SPARC_PC_LM22},
  {BFD_RELOC_SPARC_WDISP16, R_SPARC_WDISP16},
  {BFD_RELOC_SPARC_WDISP19, R_SPARC_WDISP19},
  {BFD_RELOC_SPARC_GLOB_JMP, R_SPARC_GLOB_JMP},
  {BFD_RELOC_SPARC_LO7, R_SPARC_LO7},
};

static CONST struct reloc_howto_struct *
bfd_elf64_bfd_reloc_type_lookup (abfd, code)
     bfd *abfd;
     bfd_reloc_code_real_type code;
{
  int i;
  for (i = 0; i < sizeof (sparc_reloc_map) / sizeof (struct elf_reloc_map); i++)
    {
      if (sparc_reloc_map[i].bfd_reloc_val == code)
	return &elf_sparc_howto_table[(int) sparc_reloc_map[i].elf_reloc_val];
    }
  return 0;
}

static void
elf_info_to_howto (abfd, cache_ptr, dst)
     bfd *abfd;
     arelent *cache_ptr;
     Elf64_Internal_Rela *dst;
{
  BFD_ASSERT (ELF64_R_TYPE (dst->r_info) < (unsigned int) R_SPARC_max);
  cache_ptr->howto = &elf_sparc_howto_table[ELF64_R_TYPE (dst->r_info)];
}

#define TARGET_BIG_SYM	bfd_elf64_sparc_vec
#define TARGET_BIG_NAME	"elf64-sparc"
#define ELF_ARCH	bfd_arch_sparc
#define ELF_MACHINE_CODE EM_SPARC64

#include "elf64-target.h"

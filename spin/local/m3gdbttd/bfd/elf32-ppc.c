/* PowerPC-specific support for 32-bit ELF
   Copyright 1994 Free Software Foundation, Inc.
   Written by Ian Lance Taylor, Cygnus Support.

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

/* This file is based on a preliminary PowerPC ELF ABI.  The
   information may not match the final PowerPC ELF ABI.  It includes
   suggestions from the in-progress Embedded PowerPC ABI, and that
   information may also not match.  */

#include "bfd.h"
#include "sysdep.h"
#include "libbfd.h"
#include "libelf.h"

static bfd_reloc_status_type ppc_elf_unsupported_reloc
  PARAMS ((bfd *, arelent *, asymbol *, PTR, asection *, bfd *, char **));
static bfd_reloc_status_type ppc_elf_addr16_ha_reloc
  PARAMS ((bfd *, arelent *, asymbol *, PTR, asection *, bfd *, char **));
static bfd_reloc_status_type ppc_elf_got16_reloc
  PARAMS ((bfd *, arelent *, asymbol *, PTR, asection *, bfd *, char **));
static const struct reloc_howto_struct *bfd_elf32_bfd_reloc_type_lookup
  PARAMS ((bfd *abfd, bfd_reloc_code_real_type code));
static void powerpc_info_to_howto
  PARAMS ((bfd *abfd, arelent *cache_ptr, Elf32_Internal_Rela *dst));

#define USE_RELA

enum reloc_type
{
  R_PPC_NONE = 0,
  R_PPC_ADDR32,
  R_PPC_ADDR24,
  R_PPC_ADDR16,
  R_PPC_ADDR16_LO,
  R_PPC_ADDR16_HI,
  R_PPC_ADDR16_HA,
  R_PPC_ADDR14,
  R_PPC_ADDR14_BRTAKEN,
  R_PPC_ADDR14_BRNTAKEN,
  R_PPC_REL24,
  R_PPC_REL14,
  R_PPC_REL14_BRTAKEN,
  R_PPC_REL14_BRNTAKEN,
  R_PPC_GOT16,
  R_PPC_GOT16_LO,
  R_PPC_GOT16_HI,
  R_PPC_GOT16_HA,
  R_PPC_PLT24,
  R_PPC_COPY,
  R_PPC_GLOB_DAT,
  R_PPC_JMP_SLOT,
  R_PPC_RELATIVE,
  R_PPC_LOCAL24PC,
  R_PPC_UADDR32,
  R_PPC_UADDR16,

  /* The remaining relocs are from the Embedded ELF ABI, and are not
     in the SVR4 ELF ABI.  */
  R_PPC_CGOT16,
  R_PPC_CGOT16_LO,
  R_PPC_CGOT16_HI,
  R_PPC_CGOT16_HA,
  R_PPC_GOT_REG,
  R_PPC_CGOT_REG,
  R_PPC_NADDR16,
  R_PPC_NADDR32,
  R_PPC_NADDR24,
  R_PPC_DISP16,
  R_PPC_GOTRL16,
  R_PPC_GOTRLA16,
  R_PPC_CGOTRL16,
  R_PPC_CGOTRLA16,
  R_PPC_max
};

static reloc_howto_type elf_powerpc_howto_table[] =
{
  /* This reloc does nothing.  */
  HOWTO (R_PPC_NONE,	        /* type */                                 
	 0,	                /* rightshift */                           
	 2,	                /* size (0 = byte, 1 = short, 2 = long) */ 
	 32,	                /* bitsize */                   
	 false,	                /* pc_relative */                          
	 0,	                /* bitpos */                               
	 complain_overflow_bitfield, /* complain_on_overflow */
	 bfd_elf_generic_reloc, /* special_function */                     
	 "R_PPC_NONE",          /* name */                                 
	 false,	                /* partial_inplace */                      
	 0,		        /* src_mask */                             
	 0,            		/* dst_mask */                             
	 false),                /* pcrel_offset */

  /* A standard 32 bit relocation.  */
  HOWTO (R_PPC_ADDR32,          /* type */                                 
	 0,	                /* rightshift */                           
	 2,	                /* size (0 = byte, 1 = short, 2 = long) */ 
	 32,	                /* bitsize */                   
	 false,	                /* pc_relative */                          
	 0,	                /* bitpos */                               
	 complain_overflow_bitfield, /* complain_on_overflow */
	 bfd_elf_generic_reloc, /* special_function */                     
	 "R_PPC_ADDR32",	/* name */
	 false,	                /* partial_inplace */                      
	 0,		        /* src_mask */                             
	 0xffffffff,   		/* dst_mask */                             
	 false),                /* pcrel_offset */

  /* An absolute 26 bit branch; the lower two bits must be zero.
     FIXME: we don't check that, we just clear them.  */
  HOWTO (R_PPC_ADDR24,          /* type */                                 
	 0,	                /* rightshift */                           
	 2,	                /* size (0 = byte, 1 = short, 2 = long) */ 
	 26,	                /* bitsize */                   
	 false,	                /* pc_relative */                          
	 0,	                /* bitpos */                               
	 complain_overflow_bitfield, /* complain_on_overflow */
	 bfd_elf_generic_reloc, /* special_function */                     
	 "R_PPC_ADDR24",	/* name */
	 false,	                /* partial_inplace */                      
	 0,		        /* src_mask */                             
	 0x3fffffc,    		/* dst_mask */                             
	 false),                /* pcrel_offset */

  /* A standard 16 bit relocation.  */
  HOWTO (R_PPC_ADDR16,          /* type */                                 
	 0,	                /* rightshift */                           
	 1,	                /* size (0 = byte, 1 = short, 2 = long) */ 
	 16,	                /* bitsize */                   
	 false,	                /* pc_relative */                          
	 0,	                /* bitpos */                               
	 complain_overflow_bitfield, /* complain_on_overflow */
	 bfd_elf_generic_reloc, /* special_function */                     
	 "R_PPC_ADDR16",	/* name */
	 false,	                /* partial_inplace */                      
	 0,		        /* src_mask */                             
	 0xffff,    		/* dst_mask */                             
	 false),                /* pcrel_offset */

  /* A 16 bit relocation without overflow.  */
  HOWTO (R_PPC_ADDR16_LO,       /* type */                                 
	 0,	                /* rightshift */                           
	 1,	                /* size (0 = byte, 1 = short, 2 = long) */ 
	 16,	                /* bitsize */                   
	 false,	                /* pc_relative */                          
	 0,	                /* bitpos */                               
	 complain_overflow_dont,/* complain_on_overflow */
	 bfd_elf_generic_reloc, /* special_function */                     
	 "R_PPC_ADDR16_LO",	/* name */
	 false,	                /* partial_inplace */                      
	 0,		        /* src_mask */                             
	 0xffff,       		/* dst_mask */                             
	 false),                /* pcrel_offset */

  /* The high order 16 bits of an address.  */
  HOWTO (R_PPC_ADDR16_HI,       /* type */                                 
	 16,	                /* rightshift */                           
	 1,	                /* size (0 = byte, 1 = short, 2 = long) */ 
	 16,	                /* bitsize */                   
	 false,	                /* pc_relative */                          
	 0,	                /* bitpos */                               
	 complain_overflow_dont, /* complain_on_overflow */
	 bfd_elf_generic_reloc, /* special_function */                     
	 "R_PPC_ADDR16_HI",	/* name */
	 false,	                /* partial_inplace */                      
	 0,		        /* src_mask */                             
	 0xffff,       		/* dst_mask */                             
	 false),                /* pcrel_offset */

  /* The high order 16 bits of an address, plus 1 if the contents of
     the low 16 bits, treated as a signed number, is negative.  */
  HOWTO (R_PPC_ADDR16_HA,       /* type */                                 
	 16,	                /* rightshift */                           
	 1,	                /* size (0 = byte, 1 = short, 2 = long) */ 
	 16,	                /* bitsize */                   
	 false,	                /* pc_relative */                          
	 0,	                /* bitpos */                               
	 complain_overflow_dont, /* complain_on_overflow */
	 ppc_elf_addr16_ha_reloc, /* special_function */                     
	 "R_PPC_ADDR16_HA",	/* name */
	 false,	                /* partial_inplace */                      
	 0,		        /* src_mask */                             
	 0xffff,    		/* dst_mask */                             
	 false),                /* pcrel_offset */

  /* An absolute 16 bit branch; the lower two bits must be zero.
     FIXME: we don't check that, we just clear them.  */
  HOWTO (R_PPC_ADDR14,          /* type */                                 
	 0,	                /* rightshift */                           
	 2,	                /* size (0 = byte, 1 = short, 2 = long) */ 
	 16,	                /* bitsize */                   
	 false,	                /* pc_relative */                          
	 0,	                /* bitpos */                               
	 complain_overflow_bitfield, /* complain_on_overflow */
	 bfd_elf_generic_reloc, /* special_function */                     
	 "R_PPC_ADDR14",	/* name */
	 false,	                /* partial_inplace */                      
	 0,		        /* src_mask */                             
	 0xfffc,    		/* dst_mask */                             
	 false),                /* pcrel_offset */

  /* An absolute 16 bit branch, for which bit 10 should be set to
     indicate that the branch is expected to be taken.  The lower two
     bits must be zero.  */
  HOWTO (R_PPC_ADDR14_BRTAKEN,  /* type */                                 
	 0,	                /* rightshift */                           
	 2,	                /* size (0 = byte, 1 = short, 2 = long) */ 
	 16,	                /* bitsize */                   
	 false,	                /* pc_relative */                          
	 0,	                /* bitpos */                               
	 complain_overflow_bitfield, /* complain_on_overflow */
	 ppc_elf_unsupported_reloc, /* special_function */             
	 "R_PPC_ADDR14_BRTAKEN",/* name */
	 false,	                /* partial_inplace */                      
	 0,		        /* src_mask */                             
	 0xfffc,      		/* dst_mask */                             
	 false),                /* pcrel_offset */

  /* An absolute 16 bit branch, for which bit 10 should be set to
     indicate that the branch is not expected to be taken.  The lower
     two bits must be zero.  */
  HOWTO (R_PPC_ADDR14_BRNTAKEN, /* type */                                 
	 0,	                /* rightshift */                           
	 2,	                /* size (0 = byte, 1 = short, 2 = long) */ 
	 16,	                /* bitsize */                   
	 false,	                /* pc_relative */                          
	 0,	                /* bitpos */                               
	 complain_overflow_bitfield, /* complain_on_overflow */
	 ppc_elf_unsupported_reloc, /* special_function */             
	 "R_PPC_ADDR14_BRNTAKEN",/* name */
	 false,	                /* partial_inplace */                      
	 0,		        /* src_mask */                             
	 0xfffc,    		/* dst_mask */                             
	 false),                /* pcrel_offset */

  /* A relative 26 bit branch; the lower two bits must be zero.  */
  HOWTO (R_PPC_REL24,           /* type */                                 
	 0,	                /* rightshift */                           
	 2,	                /* size (0 = byte, 1 = short, 2 = long) */ 
	 26,	                /* bitsize */                   
	 true,	                /* pc_relative */                          
	 0,	                /* bitpos */                               
	 complain_overflow_signed, /* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */                     
	 "R_PPC_REL24",		/* name */
	 false,	                /* partial_inplace */                      
	 0,		        /* src_mask */                             
	 0x3fffffc,    		/* dst_mask */                             
	 true),                 /* pcrel_offset */

  /* A relative 16 bit branch; the lower two bits must be zero.  */
  HOWTO (R_PPC_REL14,           /* type */                                 
	 0,	                /* rightshift */                           
	 2,	                /* size (0 = byte, 1 = short, 2 = long) */ 
	 16,	                /* bitsize */                   
	 true,	                /* pc_relative */                          
	 0,	                /* bitpos */                               
	 complain_overflow_signed, /* complain_on_overflow */
	 bfd_elf_generic_reloc, /* special_function */                     
	 "R_PPC_REL14",		/* name */
	 false,	                /* partial_inplace */                      
	 0,		        /* src_mask */                             
	 0xfffc,    		/* dst_mask */                             
	 true),                 /* pcrel_offset */

  /* A relative 16 bit branch.  Bit 10 should be set to indicate that
     the branch is expected to be taken.  The lower two bits must be
     zero.  */
  HOWTO (R_PPC_REL14_BRTAKEN,   /* type */                                 
	 0,	                /* rightshift */                           
	 2,	                /* size (0 = byte, 1 = short, 2 = long) */ 
	 16,	                /* bitsize */                   
	 false,	                /* pc_relative */                          
	 0,	                /* bitpos */                               
	 complain_overflow_signed, /* complain_on_overflow */
	 ppc_elf_unsupported_reloc, /* special_function */             
	 "R_PPC_REL14_BRTAKEN",	/* name */
	 false,	                /* partial_inplace */                      
	 0,		        /* src_mask */                             
	 0xfffc,    		/* dst_mask */                             
	 true),                 /* pcrel_offset */

  /* A relative 16 bit branch.  Bit 10 should be set to indicate that
     the branch is not expected to be taken.  The lower two bits must
     be zero.  */
  HOWTO (R_PPC_REL14_BRNTAKEN,  /* type */                                 
	 0,	                /* rightshift */                           
	 2,	                /* size (0 = byte, 1 = short, 2 = long) */ 
	 16,	                /* bitsize */                   
	 false,	                /* pc_relative */                          
	 0,	                /* bitpos */                               
	 complain_overflow_signed, /* complain_on_overflow */
	 ppc_elf_unsupported_reloc, /* special_function */             
	 "R_PPC_REL14_BRNTAKEN",/* name */
	 false,	                /* partial_inplace */                      
	 0,		        /* src_mask */                             
	 0xfffc,    		/* dst_mask */                             
	 true),                 /* pcrel_offset */

  /* Like R_PPC_ADDR16, but referring to the GOT table entry for the
     symbol.  */
  HOWTO (R_PPC_GOT16,           /* type */                                 
	 0,	                /* rightshift */                           
	 1,	                /* size (0 = byte, 1 = short, 2 = long) */ 
	 16,	                /* bitsize */                   
	 false,	                /* pc_relative */                          
	 0,	                /* bitpos */                               
	 complain_overflow_bitfield, /* complain_on_overflow */
	 ppc_elf_got16_reloc,	/* special_function */                     
	 "R_PPC_GOT16",		/* name */
	 false,	                /* partial_inplace */                      
	 0,		        /* src_mask */                             
	 0xffff,    		/* dst_mask */                             
	 false),                /* pcrel_offset */

  /* Like R_PPC_ADDR16_LO, but referring to the GOT table entry for
     the symbol.  */
  HOWTO (R_PPC_GOT16_LO,        /* type */                                 
	 0,	                /* rightshift */                           
	 1,	                /* size (0 = byte, 1 = short, 2 = long) */ 
	 16,	                /* bitsize */                   
	 false,	                /* pc_relative */                          
	 0,	                /* bitpos */                               
	 complain_overflow_bitfield, /* complain_on_overflow */
	 ppc_elf_got16_reloc,	/* special_function */                     
	 "R_PPC_GOT16_LO",	/* name */
	 false,	                /* partial_inplace */                      
	 0,		        /* src_mask */                             
	 0xffff,    		/* dst_mask */                             
	 false),                /* pcrel_offset */

  /* Like R_PPC_ADDR16_HI, but referring to the GOT table entry for
     the symbol.  */
  HOWTO (R_PPC_GOT16_HI,        /* type */                                 
	 16,	                /* rightshift */                           
	 1,	                /* size (0 = byte, 1 = short, 2 = long) */ 
	 16,	                /* bitsize */                   
	 false,	                /* pc_relative */                          
	 0,	                /* bitpos */                               
	 complain_overflow_bitfield, /* complain_on_overflow */
	 ppc_elf_got16_reloc,	/* special_function */                     
	 "R_PPC_GOT16_HI",	/* name */
	 false,	                /* partial_inplace */                      
	 0,		        /* src_mask */                             
	 0xffff,    		/* dst_mask */                             
	 false),                 /* pcrel_offset */

  /* Like R_PPC_ADDR16_HA, but referring to the GOT table entry for
     the symbol.  FIXME: Not supported.  */
  HOWTO (R_PPC_GOT16_HA,        /* type */                                 
	 0,	                /* rightshift */                           
	 1,	                /* size (0 = byte, 1 = short, 2 = long) */ 
	 16,	                /* bitsize */                   
	 false,	                /* pc_relative */                          
	 0,	                /* bitpos */                               
	 complain_overflow_bitfield, /* complain_on_overflow */
	 ppc_elf_unsupported_reloc, /* special_function */                     
	 "R_PPC_GOT16_HA",	/* name */
	 false,	                /* partial_inplace */                      
	 0,		        /* src_mask */                             
	 0xffff,    		/* dst_mask */                             
	 false),                /* pcrel_offset */

  /* Like R_PPC_REL24, but referring to the procedure linkage table
     entry for the symbol.  FIXME: Not supported.  */
  HOWTO (R_PPC_PLT24,           /* type */                                 
	 0,	                /* rightshift */                           
	 2,	                /* size (0 = byte, 1 = short, 2 = long) */ 
	 26,	                /* bitsize */                   
	 true,	                /* pc_relative */                          
	 0,	                /* bitpos */                               
	 complain_overflow_signed, /* complain_on_overflow */
	 bfd_elf_generic_reloc, /* special_function */                     
	 "R_PPC_PLT24",		/* name */
	 false,	                /* partial_inplace */                      
	 0,		        /* src_mask */                             
	 0x3fffffc,    		/* dst_mask */                             
	 true),                 /* pcrel_offset */

  /* This is used only by the dynamic linker.  The symbol should exist
     both in the object being run and in some shared library.  The
     dynamic linker copies the data addressed by the symbol from the
     shared library into the object.  I have no idea what the purpose
     of this is.  */
  HOWTO (R_PPC_COPY,            /* type */                                 
	 0,	                /* rightshift */                           
	 2,	                /* size (0 = byte, 1 = short, 2 = long) */ 
	 32,	                /* bitsize */                   
	 false,	                /* pc_relative */                          
	 0,	                /* bitpos */                               
	 complain_overflow_bitfield, /* complain_on_overflow */
	 bfd_elf_generic_reloc, /* special_function */                     
	 "R_PPC_COPY",		/* name */
	 false,	                /* partial_inplace */                      
	 0,		        /* src_mask */                             
	 0,            		/* dst_mask */                             
	 false),                /* pcrel_offset */

  /* Like R_PPC_ADDR32, but used when setting global offset table
     entries.  */
  HOWTO (R_PPC_GLOB_DAT,        /* type */                                 
	 0,	                /* rightshift */                           
	 2,	                /* size (0 = byte, 1 = short, 2 = long) */ 
	 32,	                /* bitsize */                   
	 false,	                /* pc_relative */                          
	 0,	                /* bitpos */                               
	 complain_overflow_bitfield, /* complain_on_overflow */
	 bfd_elf_generic_reloc, /* special_function */                     
	 "R_PPC_GLOB_DAT",	/* name */
	 false,	                /* partial_inplace */                      
	 0,		        /* src_mask */                             
	 0xffffffff,  		/* dst_mask */                             
	 false),                /* pcrel_offset */

  /* Marks a procedure linkage table entry for a symbol.  */
  HOWTO (R_PPC_JMP_SLOT,        /* type */                                 
	 0,	                /* rightshift */                           
	 2,	                /* size (0 = byte, 1 = short, 2 = long) */ 
	 32,	                /* bitsize */                   
	 false,	                /* pc_relative */                          
	 0,	                /* bitpos */                               
	 complain_overflow_bitfield, /* complain_on_overflow */
	 bfd_elf_generic_reloc, /* special_function */                     
	 "R_PPC_JMP_SLOT",	/* name */
	 false,	                /* partial_inplace */                      
	 0,		        /* src_mask */                             
	 0,            		/* dst_mask */                             
	 false),                /* pcrel_offset */

  /* Used only by the dynamic linker.  When the object is run, this
     longword is set to the load address of the object, plus the
     addend.  */
  HOWTO (R_PPC_RELATIVE,        /* type */                                 
	 0,	                /* rightshift */                           
	 2,	                /* size (0 = byte, 1 = short, 2 = long) */ 
	 32,	                /* bitsize */                   
	 false,	                /* pc_relative */                          
	 0,	                /* bitpos */                               
	 complain_overflow_bitfield, /* complain_on_overflow */
	 bfd_elf_generic_reloc, /* special_function */                     
	 "R_PPC_RELATIVE",	/* name */
	 false,	                /* partial_inplace */                      
	 0,		        /* src_mask */                             
	 0xffffffff,   		/* dst_mask */                             
	 false),                /* pcrel_offset */

  /* Like R_PPC_REL24, but uses the value of the symbol within the
     object rather than the final value.  Normally used for
     _GLOBAL_OFFSET_TABLE_.  FIXME: Not supported.  */
  HOWTO (R_PPC_LOCAL24PC,       /* type */                                 
	 0,	                /* rightshift */                           
	 2,	                /* size (0 = byte, 1 = short, 2 = long) */ 
	 26,	                /* bitsize */                   
	 true,	                /* pc_relative */                          
	 0,	                /* bitpos */                               
	 complain_overflow_signed, /* complain_on_overflow */
	 ppc_elf_unsupported_reloc, /* special_function */                     
	 "R_PPC_LOCAL24PC",	/* name */
	 false,	                /* partial_inplace */                      
	 0,		        /* src_mask */                             
	 0x3fffffc,   		/* dst_mask */                             
	 true),                 /* pcrel_offset */

  /* Like R_PPC_ADDR32, but may be unaligned.  */
  HOWTO (R_PPC_UADDR32,         /* type */                                 
	 0,	                /* rightshift */                           
	 2,	                /* size (0 = byte, 1 = short, 2 = long) */ 
	 32,	                /* bitsize */                   
	 false,	                /* pc_relative */                          
	 0,	                /* bitpos */                               
	 complain_overflow_bitfield, /* complain_on_overflow */
	 bfd_elf_generic_reloc, /* special_function */                     
	 "R_PPC_UADDR32",	/* name */
	 false,	                /* partial_inplace */                      
	 0,		        /* src_mask */                             
	 0xffffffff,   		/* dst_mask */                             
	 false),                /* pcrel_offset */

  /* Like R_PPC_ADDR16, but may be unaligned.  */
  HOWTO (R_PPC_UADDR16,         /* type */                                 
	 0,	                /* rightshift */                           
	 1,	                /* size (0 = byte, 1 = short, 2 = long) */ 
	 16,	                /* bitsize */                   
	 false,	                /* pc_relative */                          
	 0,	                /* bitpos */                               
	 complain_overflow_bitfield, /* complain_on_overflow */
	 bfd_elf_generic_reloc, /* special_function */                     
	 "R_PPC_UADDR16",	/* name */
	 false,	                /* partial_inplace */                      
	 0,		        /* src_mask */                             
	 0xffff,       		/* dst_mask */                             
	 false),                /* pcrel_offset */

  /* The remaining relocs are from the Embedded ELF ABI, and are not
     in the SVR4 ELF ABI.  */

  /* Like R_PPC_GOT16, but using the constant GOT table.  */
  HOWTO (R_PPC_CGOT16,          /* type */                                 
	 0,	                /* rightshift */                           
	 1,	                /* size (0 = byte, 1 = short, 2 = long) */ 
	 16,	                /* bitsize */                   
	 false,	                /* pc_relative */                          
	 0,	                /* bitpos */                               
	 complain_overflow_bitfield, /* complain_on_overflow */
	 ppc_elf_got16_reloc,	/* special_function */                     
	 "R_PPC_CGOT16",	/* name */
	 false,	                /* partial_inplace */                      
	 0,		        /* src_mask */                             
	 0xffff,   		/* dst_mask */                             
	 false),                /* pcrel_offset */

  /* Like R_PPC_GOT16_LO, but using the constant GOT table.  */
  HOWTO (R_PPC_CGOT16_LO,       /* type */                                 
	 0,	                /* rightshift */                           
	 1,	                /* size (0 = byte, 1 = short, 2 = long) */ 
	 16,	                /* bitsize */                   
	 false,	                /* pc_relative */                          
	 0,	                /* bitpos */                               
	 complain_overflow_bitfield, /* complain_on_overflow */
	 ppc_elf_got16_reloc,	/* special_function */                     
	 "R_PPC_CGOT16_LO",	/* name */
	 false,	                /* partial_inplace */                      
	 0,		        /* src_mask */                             
	 0xffff,   		/* dst_mask */                             
	 false),                /* pcrel_offset */

  /* Like R_PPC_GOT16_HI, but using the constant GOT table.  */
  HOWTO (R_PPC_CGOT16_HI,       /* type */                                 
	 16,	                /* rightshift */                           
	 1,	                /* size (0 = byte, 1 = short, 2 = long) */ 
	 16,	                /* bitsize */                   
	 false,	                /* pc_relative */                          
	 0,	                /* bitpos */                               
	 complain_overflow_bitfield, /* complain_on_overflow */
	 ppc_elf_got16_reloc,	/* special_function */                     
	 "R_PPC_CGOT16_HI",	/* name */
	 false,	                /* partial_inplace */                      
	 0,		        /* src_mask */                             
	 0xffff,   		/* dst_mask */                             
	 false),                /* pcrel_offset */

  /* Like R_PPC_GOT16_HA, but using the constant GOT table.  */
  HOWTO (R_PPC_CGOT16_HA,       /* type */                                 
	 0,	                /* rightshift */                           
	 1,	                /* size (0 = byte, 1 = short, 2 = long) */ 
	 16,	                /* bitsize */                   
	 false,	                /* pc_relative */                          
	 0,	                /* bitpos */                               
	 complain_overflow_bitfield, /* complain_on_overflow */
	 ppc_elf_unsupported_reloc, /* special_function */                     
	 "R_PPC_CGOT16_HA",	/* name */
	 false,	                /* partial_inplace */                      
	 0,		        /* src_mask */                             
	 0xffff,   		/* dst_mask */                             
	 false),                /* pcrel_offset */

  /* The dedicated GOT register.  */
  HOWTO (R_PPC_GOT_REG,         /* type */                                 
	 0,	                /* rightshift */                           
	 1,	                /* size (0 = byte, 1 = short, 2 = long) */ 
	 5,	                /* bitsize */                   
	 false,	                /* pc_relative */                          
	 0,	                /* bitpos */                               
	 complain_overflow_bitfield, /* complain_on_overflow */
	 ppc_elf_unsupported_reloc, /* special_function */                     
	 "R_PPC_GOT_REG",	/* name */
	 false,	                /* partial_inplace */                      
	 0,		        /* src_mask */                             
	 0x1f,   		/* dst_mask */                             
	 false),                /* pcrel_offset */

  /* The dedicated CGOT register.  */
  HOWTO (R_PPC_CGOT_REG,        /* type */                                 
	 0,	                /* rightshift */                           
	 1,	                /* size (0 = byte, 1 = short, 2 = long) */ 
	 5,	                /* bitsize */                   
	 false,	                /* pc_relative */                          
	 0,	                /* bitpos */                               
	 complain_overflow_bitfield, /* complain_on_overflow */
	 ppc_elf_unsupported_reloc, /* special_function */                     
	 "R_PPC_CGOT_REG",	/* name */
	 false,	                /* partial_inplace */                      
	 0,		        /* src_mask */                             
	 0x1f,   		/* dst_mask */                             
	 false),                /* pcrel_offset */

  /* Like R_PPC_ADDR16, but subtract the symbol.  */
  HOWTO (R_PPC_NADDR16,         /* type */                                 
	 0,	                /* rightshift */                           
	 1,	                /* size (0 = byte, 1 = short, 2 = long) */ 
	 16,	                /* bitsize */                   
	 false,	                /* pc_relative */                          
	 0,	                /* bitpos */                               
	 complain_overflow_bitfield, /* complain_on_overflow */
	 ppc_elf_unsupported_reloc, /* special_function */                     
	 "R_PPC_NADDR16",	/* name */
	 false,	                /* partial_inplace */                      
	 0,		        /* src_mask */                             
	 0xffff,   		/* dst_mask */                             
	 false),                /* pcrel_offset */

  /* Like R_PPC_ADDR32, but subtract the symbol.  */
  HOWTO (R_PPC_NADDR32,         /* type */                                 
	 0,	                /* rightshift */                           
	 2,	                /* size (0 = byte, 1 = short, 2 = long) */ 
	 32,	                /* bitsize */                   
	 false,	                /* pc_relative */                          
	 0,	                /* bitpos */                               
	 complain_overflow_bitfield, /* complain_on_overflow */
	 ppc_elf_unsupported_reloc, /* special_function */                     
	 "R_PPC_NADDR32",	/* name */
	 false,	                /* partial_inplace */                      
	 0,		        /* src_mask */                             
	 0xffffffff,   		/* dst_mask */                             
	 false),                /* pcrel_offset */

  /* Like R_PPC_ADDR24, but subtract the symbol.  */
  HOWTO (R_PPC_NADDR24,         /* type */                                 
	 0,	                /* rightshift */                           
	 2,	                /* size (0 = byte, 1 = short, 2 = long) */ 
	 26,	                /* bitsize */                   
	 false,	                /* pc_relative */                          
	 0,	                /* bitpos */                               
	 complain_overflow_bitfield, /* complain_on_overflow */
	 ppc_elf_unsupported_reloc, /* special_function */                     
	 "R_PPC_NADDR24",	/* name */
	 false,	                /* partial_inplace */                      
	 0,		        /* src_mask */                             
	 0x3fffffc,   		/* dst_mask */                             
	 false),                /* pcrel_offset */

  /* Offset of symbol relative to containing csect.  */
  HOWTO (R_PPC_DISP16,          /* type */                                 
	 0,	                /* rightshift */                           
	 1,	                /* size (0 = byte, 1 = short, 2 = long) */ 
	 16,	                /* bitsize */                   
	 false,	                /* pc_relative */                          
	 0,	                /* bitpos */                               
	 complain_overflow_bitfield, /* complain_on_overflow */
	 ppc_elf_unsupported_reloc, /* special_function */                     
	 "R_PPC_DISP16",	/* name */
	 false,	                /* partial_inplace */                      
	 0,		        /* src_mask */                             
	 0xffff,   		/* dst_mask */                             
	 false),                /* pcrel_offset */

  /* Like R_PPC_GOT16, but may be converted to an address computation
     instead of a load.  */
  HOWTO (R_PPC_GOTRL16,         /* type */                                 
	 0,	                /* rightshift */                           
	 1,	                /* size (0 = byte, 1 = short, 2 = long) */ 
	 16,	                /* bitsize */                   
	 false,	                /* pc_relative */                          
	 0,	                /* bitpos */                               
	 complain_overflow_bitfield, /* complain_on_overflow */
	 ppc_elf_got16_reloc,	/* special_function */                     
	 "R_PPC_GOTRL16",	/* name */
	 false,	                /* partial_inplace */                      
	 0,		        /* src_mask */                             
	 0xffff,   		/* dst_mask */                             
	 false),                /* pcrel_offset */

  /* Like R_PPC_GOT16, but may be converted to a load instead of an
     address computation.  */
  HOWTO (R_PPC_GOTRLA16,        /* type */                                 
	 0,	                /* rightshift */                           
	 1,	                /* size (0 = byte, 1 = short, 2 = long) */ 
	 16,	                /* bitsize */                   
	 false,	                /* pc_relative */                          
	 0,	                /* bitpos */                               
	 complain_overflow_bitfield, /* complain_on_overflow */
	 ppc_elf_got16_reloc,	/* special_function */                     
	 "R_PPC_GOTRLA16",	/* name */
	 false,	                /* partial_inplace */                      
	 0,		        /* src_mask */                             
	 0xffff,  		/* dst_mask */                             
	 false),                /* pcrel_offset */

  /* Like R_PPC_CGOT16, but may be converted to an address computation
     instead of a load.  */
  HOWTO (R_PPC_CGOTRL16,        /* type */                                 
	 0,	                /* rightshift */                           
	 1,	                /* size (0 = byte, 1 = short, 2 = long) */ 
	 16,	                /* bitsize */                   
	 false,	                /* pc_relative */                          
	 0,	                /* bitpos */                               
	 complain_overflow_bitfield, /* complain_on_overflow */
	 ppc_elf_unsupported_reloc, /* special_function */                     
	 "R_PPC_CGOTRL16",	/* name */
	 false,	                /* partial_inplace */                      
	 0,		        /* src_mask */                             
	 0xffff,   		/* dst_mask */                             
	 false),                /* pcrel_offset */

  /* Like R_PPC_CGOT16, but may be converted to a load instead of an
     address computation.  */
  HOWTO (R_PPC_CGOTRLA16,       /* type */                                 
	 0,	                /* rightshift */                           
	 1,	                /* size (0 = byte, 1 = short, 2 = long) */ 
	 16,	                /* bitsize */                   
	 false,	                /* pc_relative */                          
	 0,	                /* bitpos */                               
	 complain_overflow_bitfield, /* complain_on_overflow */
	 ppc_elf_unsupported_reloc, /* special_function */
	 "R_PPC_CGOTRLA16",	/* name */
	 false,	                /* partial_inplace */                      
	 0,		        /* src_mask */                             
	 0xffff,   		/* dst_mask */                             
	 false),                /* pcrel_offset */
};

/* Don't pretend we can deal with unsupported relocs.  */

/*ARGSUSED*/
static bfd_reloc_status_type
ppc_elf_unsupported_reloc (abfd, reloc_entry, symbol, data, input_section,
			   output_bfd, error_message)
     bfd *abfd;
     arelent *reloc_entry;
     asymbol *symbol;
     PTR data;
     asection *input_section;
     bfd *output_bfd;
     char **error_message;
{
  abort ();
}

/* Handle the ADDR16_HA reloc by adjusting the reloc addend.  */

static bfd_reloc_status_type
ppc_elf_addr16_ha_reloc (abfd, reloc_entry, symbol, data, input_section,
		         output_bfd, error_message)
     bfd *abfd;
     arelent *reloc_entry;
     asymbol *symbol;
     PTR data;
     asection *input_section;
     bfd *output_bfd;
     char **error_message;
{
  bfd_vma relocation;

  if (output_bfd != (bfd *) NULL)
    return bfd_elf_generic_reloc (abfd, reloc_entry, symbol, data,
				  input_section, output_bfd, error_message);

  if (bfd_is_com_section (symbol->section))
    relocation = 0;
  else
    relocation = symbol->value;

  relocation += (symbol->section->output_section->vma
		 + symbol->section->output_offset
		 + reloc_entry->addend);

  if ((relocation & 0x8000) != 0)
    reloc_entry->addend += 0x10000;

  return bfd_reloc_continue;
}

/* Handle the GOT16 reloc.  We want to use the offset within the .got
   section, not the actual VMA.  This is appropriate when generating
   an embedded ELF object, for which the .got section acts like the
   AIX .toc section.  When and if we support PIC code, we will have to
   change this, perhaps by switching off on the e_type field.  */

static bfd_reloc_status_type
ppc_elf_got16_reloc (abfd, reloc_entry, symbol, data, input_section,
		     output_bfd, error_message)
     bfd *abfd;
     arelent *reloc_entry;
     asymbol *symbol;
     PTR data;
     asection *input_section;
     bfd *output_bfd;
     char **error_message;
{
  asection *sec;

  if (output_bfd != (bfd *) NULL)
    return bfd_elf_generic_reloc (abfd, reloc_entry, symbol, data,
				  input_section, output_bfd, error_message);

  sec = bfd_get_section (*reloc_entry->sym_ptr_ptr);
  BFD_ASSERT (bfd_is_und_section (sec)
	      || strcmp (bfd_get_section_name (abfd, sec), ".got") == 0
	      || strcmp (bfd_get_section_name (abfd, sec), ".cgot") == 0);
  reloc_entry->addend -= sec->output_section->vma;
  return bfd_reloc_continue;
}

/* Map BFD reloc types to PowerPC ELF reloc types.  */

struct powerpc_reloc_map
{
  unsigned char bfd_reloc_val;
  unsigned char elf_reloc_val;
};

static const struct powerpc_reloc_map powerpc_reloc_map[] =
{
  { BFD_RELOC_NONE, R_PPC_NONE, },
  { BFD_RELOC_32, R_PPC_ADDR32 },
  { BFD_RELOC_CTOR, R_PPC_ADDR32 },
  { BFD_RELOC_PPC_B26, R_PPC_REL24 },
  { BFD_RELOC_PPC_BA26, R_PPC_ADDR24 },
  { BFD_RELOC_PPC_TOC16, R_PPC_GOT16 }
};

static const struct reloc_howto_struct *
bfd_elf32_bfd_reloc_type_lookup (abfd, code)
     bfd *abfd;
     bfd_reloc_code_real_type code;
{
  int i;

  for (i = 0;
       i < sizeof (powerpc_reloc_map) / sizeof (struct powerpc_reloc_map);
       i++)
    {
      if (powerpc_reloc_map[i].bfd_reloc_val == code)
	return &elf_powerpc_howto_table[powerpc_reloc_map[i].elf_reloc_val];
    }

  return NULL;
}

/* Set the howto pointer for a PowerPC ELF reloc.  */

static void
powerpc_info_to_howto (abfd, cache_ptr, dst)
     bfd *abfd;
     arelent *cache_ptr;
     Elf32_Internal_Rela *dst;
{
  BFD_ASSERT (ELF32_R_TYPE (dst->r_info) < (unsigned int) R_PPC_max);
  cache_ptr->howto = &elf_powerpc_howto_table[ELF32_R_TYPE (dst->r_info)];
}

#define TARGET_BIG_SYM		bfd_elf32_powerpc_vec
#define TARGET_BIG_NAME		"elf32-powerpc"
#define ELF_ARCH		bfd_arch_powerpc
#define ELF_MACHINE_CODE	EM_CYGNUS_POWERPC
#define ELF_MAXPAGESIZE		0x10000
#define elf_info_to_howto	powerpc_info_to_howto

#include "elf32-target.h"

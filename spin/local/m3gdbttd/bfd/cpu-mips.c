/* bfd back-end for mips support
   Copyright (C) 1990, 91, 92, 93, 94 Free Software Foundation, Inc.
   Written by Steve Chamberlain of Cygnus Support.

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

#include "bfd.h"
#include "sysdep.h"
#include "libbfd.h"

static bfd_arch_info_type arch_info_struct[] = 
{
  {
    32,	/* 32 bits in a word */
    32,	/* 32 bits in an address */
    8,	/* 8 bits in a byte */
    bfd_arch_mips,
    3000,
    "mips",
    "mips:3000",
    3,
    true,
    bfd_default_compatible, 
    bfd_default_scan,
    0,
  },
  {
    32,	/* 32 bits in a word */
    32,	/* 32 bits in an address */
    8,	/* 8 bits in a byte */
    bfd_arch_mips,
    6000,
    "mips",
    "mips:6000",
    3,
    false,
    bfd_default_compatible, 
    bfd_default_scan,
    0,
  },
  {
    64, /* 64 bits in a word */
    64, /* 64 bits in an address */
    8,  /* 8 bits in a byte */
    bfd_arch_mips,
    4000,
    "mips",
    "mips:4000",
    3,
    false,
    bfd_default_compatible, 
    bfd_default_scan ,
    0,
  }
};

void
bfd_mips_arch ()
{
  register int i;

  for (i = 0; i < sizeof (arch_info_struct) / sizeof (*arch_info_struct); i++)
    bfd_arch_linkin(&arch_info_struct[i]);
}

/* BFD back-end for rs6000 support
   Copyright (C) 1990, 1991 Free Software Foundation, Inc.
   FIXME: Can someone provide a transliteration of this name into ASCII?
   Using the following chars caused a compiler warning on HIUX (so I replaced
   them with octal escapes), and isn't useful without an understanding of what
   character set it is.
   Written by Mimi Ph\373\364ng-Th\345o V\365 of IBM
   and John Gilmore of Cygnus Support.

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

static bfd_arch_info_type arch_info_struct = 
  {
    32,	/* 32 bits in a word */
    32,	/* 32 bits in an address */
    8,	/* 8 bits in a byte */
    bfd_arch_rs6000,
    6000,	/* only 1 machine */
    "rs6000",
    "rs6000:6000",
    3,
    true, /* the one and only */
    bfd_default_compatible, 
    bfd_default_scan ,
    0,
  };

void
bfd_rs6000_arch ()
{
  bfd_arch_linkin(&arch_info_struct);
}

/* BFD back-end for i386 a.out (Mach 3) binaries.
   Copyright (C) 1990, 1991, 1993, 1994 Free Software Foundation, Inc.

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

/* This is for Mach 3, which uses a.out, not Mach-O.  */

#ifndef MACH
#define MACH
#endif

/* There is no magic number or anything which lets us distinguish this target
   from i386aout or i386bsd.  So this target is only useful if it is the
   default target.  */

#define	PAGE_SIZE	4096
#define	SEGMENT_SIZE	PAGE_SIZE
#define BYTES_IN_WORD	4
#define TEXT_START_ADDR	0x10000

/*
 * Is the assumption that N_MAGIC == ZMAGIC still true?
 * Also, why do we not adjust for the exec header?  Does our
 * loader do things differently?
 */
#define N_HEADER_IN_TEXT(x)	1 		/* (N_MAGIC(x) == ZMAGIC) */
#define N_TXTSIZE(x)	((x).a_text)

/*
 * Mach 3.0 does not (yet) support shared libraries.
 */
#define	N_SHARED_LIB(x)	0

/*
 * If the entry isn't the standard entry point for UX binaries, then
 * assume that text starts at 0.  This is a terrible assumption, but
 * it does the right thing for emulator and BSD386 binaries.
 */
#define N_TXTADDR(x) \
    (N_MAGIC(x) != ZMAGIC ? 0 :	/* object file or NMAGIC */\
     ((x).a_entry != (TEXT_START_ADDR + EXEC_BYTES_SIZE)) ? 0 :	\
     N_HEADER_IN_TEXT(x)  ?	\
	    TEXT_START_ADDR + EXEC_BYTES_SIZE :	/* no padding */\
	    TEXT_START_ADDR			/* a page of padding */\
    )

/*
 * This is an attempt to be compatible with BSD386 binaries, which
 * have a slightly different layout.
 */
#define N_TXTOFF(x)							\
    ((x).a_entry == 0 ? 0x1000:						\
    (N_MAGIC(x) != ZMAGIC ? EXEC_BYTES_SIZE : /* object file or NMAGIC */\
     N_HEADER_IN_TEXT(x) ?						\
	    EXEC_BYTES_SIZE :			/* no padding */	\
	    PAGE_SIZE				/* a page of padding */	\
    ))

#define DEFAULT_ARCH bfd_arch_i386
#define MY(OP) CAT(i386mach3_,OP)
#define TARGETNAME "a.out-i386-mach3"

#include "bfd.h"
#include "sysdep.h"
#include "libbfd.h"
#include "libaout.h"

#include "aout-target.h"

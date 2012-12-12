/* Definitions of target machine for GNU compiler.
   Motorola 88100 Delta machine running SVR4
   Copyright (C) 1993 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "m88k/sysv4.h"

/* The .init section preamble code on the svr4 m88k Delta system looks like:

		_init:	subu	r31, r31, 0x30
			st	r1, r31, 0x20

   do_global_ctors_aux in crtstuff.c emits do_global_ctors which is put
   into the .init section.  However, this function has its own 
   prolog which allocates more of the stack.  The epilogue of do_global_ctors
   only knows about its own stack allocation, not that in the .init
   section preamble.  So we must undo the allocation performed by the .init
   section preamble.  We use the following instruction:

			addu	r31, r31, 0x30

   which is placed immediately after the two instructions shown above
   and before do_global_ctors.

   Note that this is a kludge since it depends on knowing the constant
   0x30 above.  However, no better fix is known at this time.  */
   
#undef INIT_SECTION_PREAMBLE
#define INIT_SECTION_PREAMBLE	asm ("\taddu\t #r31,#r31,0x30")

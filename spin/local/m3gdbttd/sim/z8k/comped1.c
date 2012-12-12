/* instruction interpreter module 1
   Copyright (C) 1992, 1993 Free Software Foundation, Inc.

This file is part of Z8KSIM

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Z8KZIM is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Z8KZIM; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include <ansidecl.h>
#include "sysdep.h"
#include "tm.h"
#include "sim.h"


#ifdef __GNUC__
#define INLINE static inline
#include "inlines.h"
#endif

#include "tc-gen1.h"


/* Configuration for GCC for Intel i386 running Linux.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.
   Contributed by H.J. Lu (hjl@nynexst.com)

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

#include "i386/xm-i386.h"
#include "xm-svr3.h"

#undef BSTRING
#define BSTRING
#undef bcmp
#undef bcopy
#undef bzero
#undef index
#undef rindex

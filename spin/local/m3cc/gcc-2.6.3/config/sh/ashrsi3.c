/* Copyright (C) 1994 Free Software Foundation, Inc.

This file is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file with other programs, and to distribute
those programs without any restriction coming from the use of this
file.  (The General Public License restrictions do apply in other
respects; for example, they cover modification of the file, and
distribution when not linked into another program.)

This file is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* As a special exception, if you link this library with other files,
   some of which are compiled with GCC, to produce an executable,
   this library does not by itself cause the resulting executable
   to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */


/* libgcc1 routines for the Hitachi SH cpu.
   Contributed by Steve Chamberlain.
   sac@cygnus.com */



long
__ashrsi3 (int a, int b)
{
  switch ((b - 1))
    {
    case 24 + 7:
      a >>= 1;
    case 24 + 6:
      a >>= 1;
    case 24 + 5:
      a >>= 1;
    case 24 + 4:
      a >>= 1;
    case 24 + 3:
      a >>= 1;
    case 24 + 2:
      a >>= 1;
    case 24 + 1:
      a >>= 1;
    case 24 + 0:
      a >>= 1;
    case 16 + 7:
      a >>= 1;
    case 16 + 6:
      a >>= 1;
    case 16 + 5:
      a >>= 1;
    case 16 + 4:
      a >>= 1;
    case 16 + 3:
      a >>= 1;
    case 16 + 2:
      a >>= 1;
    case 16 + 1:
      a >>= 1;
    case 16 + 0:
      a >>= 1;
    case 8 + 7:
      a >>= 1;
    case 8 + 6:
      a >>= 1;
    case 8 + 5:
      a >>= 1;
    case 8 + 4:
      a >>= 1;
    case 8 + 3:
      a >>= 1;
    case 8 + 2:
      a >>= 1;
    case 8 + 1:
      a >>= 1;
    case 8 + 0:
      a >>= 1;
    case 0 + 7:
      a >>= 1;
    case 0 + 6:
      a >>= 1;
    case 0 + 5:
      a >>= 1;
    case 0 + 4:
      a >>= 1;
    case 0 + 3:
      a >>= 1;
    case 0 + 2:
      a >>= 1;
    case 0 + 1:
      a >>= 1;
    case 0 + 0:
      a >>= 1;;
    };
  return a;
}

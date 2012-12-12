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
__ashlsi3 (int a, int b)
{
  switch (b)
    {
    default : return 0;
    case 0: return a << 0;
    case 1: return a << 1;
    case 2: return a << 2;
    case 3: return a << 3;
    case 4: return a << 4;
    case 5: return a << 5;
    case 6: return a << 6;
    case 7: return a << 7;
    case 8: return a << 8;
    case 9: return a << 9;
    case 10: return a << 10;
    case 11: return a << 11;
    case 12: return a << 12;
    case 13: return a << 13;
    case 14: return a << 14;
    case 15: return a << 15;
    case 16: return a << 16;
    case 17: return a << 17;
    case 18: return a << 18;
    case 19: return a << 19;
    case 20: return a << 20;
    case 21: return a << 21;
    case 22: return a << 22;
    case 23: return a << 23;
    case 24: return a << 24;
    case 25: return a << 25;
    case 26: return a << 26;
    case 27: return a << 27;
    case 28: return a << 28;
    case 29: return a << 29;
    case 30: return a << 30;
    case 31: return a << 31;
    }
}


/* Implementation of strtod for systems with atof.
   Copyright (C) 1991 Free Software Foundation, Inc.

This file is part of the libiberty library.
Libiberty is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

Libiberty is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with libiberty; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 675 Mass Ave,
Cambridge, MA 02139, USA.  */

#include <ctype.h>

extern double atof ();

/* Disclaimer: this is currently just used by CHILL in GDB and therefore
   has not been tested well.  It may have been tested for nothing except
   that it compiles.  */

double
strtod (str, ptr)
     char *str;
     char **ptr;
{
  char *p;

  if (ptr == (char **)0)
    return atof (str);
  
  p = str;
  
  while (isspace (*p))
    ++p;
  
  if (*p == '+' || *p == '-')
    ++p;

  /* INF or INFINITY.  */
  if ((p[0] == 'i' || p[0] == 'I')
      && (p[1] == 'n' || p[1] == 'N')
      && (p[2] == 'f' || p[2] == 'F'))
    {
      if ((p[3] == 'i' || p[3] == 'I')
	  && (p[4] == 'n' || p[4] == 'N')
	  && (p[5] == 'i' || p[5] == 'I')
	  && (p[6] == 't' || p[6] == 'T')
	  && (p[7] == 'y' || p[7] == 'Y'))
	{
	  *ptr = p + 7;
	  return atof (str);
	}
      else
	{
	  *ptr = p + 3;
	  return atof (str);
	}
    }

  /* NAN or NAN(foo).  */
  if ((p[0] == 'n' || p[0] == 'N')
      && (p[1] == 'a' || p[1] == 'A')
      && (p[2] == 'n' || p[2] == 'N'))
    {
      p += 3;
      if (*p == '(')
	{
	  ++p;
	  while (*p != '\0' && *p != ')')
	    ++p;
	  if (*p == ')')
	    ++p;
	}
      *ptr = p;
      return atof (str);
    }

  /* digits, with 0 or 1 periods in it.  */
  if (isdigit (*p) || *p == '.')
    {
      int got_dot = 0;
      while (isdigit (*p) || (!got_dot && *p == '.'))
	{
	  if (*p == '.')
	    got_dot = 1;
	  ++p;
	}

      /* Exponent.  */
      if (*p == 'e' || *p == 'E')
	{
	  int i;
	  i = 1;
	  if (p[i] == '+' || p[i] == '-')
	    ++i;
	  if (isdigit (p[i]))
	    {
	      while (isdigit (p[i]))
		++i;
	      *ptr = p + i;
	      return atof (str);
	    }
	}
      *ptr = p;
      return atof (str);
    }
  /* Didn't find any digits.  Doesn't look like a number.  */
  *ptr = str;
  return 0.0;
}

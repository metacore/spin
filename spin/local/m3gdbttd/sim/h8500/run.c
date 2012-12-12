/* run front end support for H8/500
   Copyright (C) 1987, 1992 Free Software Foundation, Inc.

This file is part of H8300 SIM

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


/* Steve Chamberlain
   sac@cygnus.com */

#include <varargs.h>
#include <stdio.h>
#include "../../bfd/bfd.h"
#include "sysdep.h"
#include "remote-sim.h"

int
main (ac, av)
     int ac;
     char **av;
{
  bfd *abfd;
  bfd_vma start_address;
  asection *s;
  int i;
  int verbose = 0;
  int trace = 0;
  char *name = "";

  for (i = 1; i < ac; i++)
    {
      if (strcmp (av[i], "-v") == 0)
	{
	  verbose = 1;
	}
      else if (strcmp (av[i], "-t") == 0)
	{
	  trace = 1;
	}
      else if (strcmp (av[i], "-c") == 0)
	{
	  int csize;
	  csize = atoi (av[i + 1]);
	  sim_csize (csize);
	  i++;
	}
      else
	{
	  name = av[i];
	}
    }
  if (verbose)
    {
      printf ("run %s\n", name);
    }
  abfd = bfd_openr (name, "coff-h8500");
  if (abfd)
    {

      if (bfd_check_format (abfd, bfd_object))
	{

	  for (s = abfd->sections; s; s = s->next)
	    {
	      unsigned char *buffer = malloc (bfd_section_size (abfd, s));
	      bfd_get_section_contents (abfd,
					s,
					buffer,
					0,
					bfd_section_size (abfd, s));
	      sim_write (s->vma, buffer, bfd_section_size (abfd, s));
	    }

	  start_address = bfd_get_start_address (abfd);
	  sim_create_inferior (start_address, NULL, NULL);
	  if (trace)
	    {
	      int done = 0;
	      while (!done)
		{
		  done = sim_trace ();
		}
	    }
	  else
	    {
	      sim_resume (0, 0);
	    }
	  if (verbose)
	    sim_info (0);

	  return 0;
	}
    }

  return 1;
}

void
printf_filtered (va_alist)
     va_dcl
{
  char *msg;
  va_list args;

  va_start (args);
  msg = va_arg (args, char *);
  vfprintf (stdout, msg, args);
  va_end (args);
}

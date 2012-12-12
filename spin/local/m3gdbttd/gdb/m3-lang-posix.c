/* M3 OS-dependent language support routines for POSIX and GDB, the GNU debugger.
   Copyright 1992, 1993 Free Software Foundation, Inc.

This file is part of GDB.

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

/*
 * HISTORY
 * 23-Jan-96  Brian Bershad (bershad) at the University of Washington
 *              Created.
 *
 */


#include "defs.h"
#include "gdbcmd.h"
#include "symtab.h"
#include "gdbtypes.h"
#include "expression.h"
#include "parser-defs.h"
#include "language.h"
#include "value.h"
#include "c-lang.h"
#include "m3-lang.h"
#include "m3-uid.h"
#include "frame.h"
#include "target.h"
#include "m3-lang-os.h"


struct symbol * find_m3_ir ();

static struct type *thread__t = 0;
static int thread__t__id_size, thread__t__id_offset;
static int thread__t__state_size, thread__t__state_offset;
static int thread__t__next_size, thread__t__next_offset;
static int thread__t__cond_size, thread__t__cond_offset;
static struct type * thread__t__cond_type;
static int thread__t__mutex_size, thread__t__mutex_offset;
static struct type * thread__t__mutex_type;
static int thread__t__time_size, thread__t__time_offset;
static struct type * thread__t__time_type;
static int thread__t__context_size, thread__t__context_offset;
static struct type * thread__t__context_type;
static int thread__t__buf_size, thread__t__buf_offset;
static struct type * thread__t__buf_type;


static int
is_posix_target()
{
  return (find_m3_ir ('M', "ThreadPosix") != NULL);
}


    
static
void
init_thread_constants() 
{
  int thread__t__context_size, thread__t__context_offset;
  struct type * thread__t__context_type;

  if (!is_posix_target()) {
      fprintf_filtered(stderr,"warning -- m3 posix not supported\n");
      return;
  }
  

  find_m3_ir ('I', "Thread");
  find_m3_ir ('M', "ThreadPosix");

  thread__t = find_m3_type_named ("Thread.T");

  find_m3_rec_field (thread__t, "id", 
		     &thread__t__id_size, &thread__t__id_offset, 0);
  find_m3_rec_field (thread__t, "state", 
		     &thread__t__state_size, &thread__t__state_offset, 0);
  find_m3_rec_field (thread__t, "next", 
		     &thread__t__next_size, &thread__t__next_offset, 0);
  find_m3_rec_field (thread__t, "waitingForCondition", 
		     &thread__t__cond_size, &thread__t__cond_offset, 
		     &thread__t__cond_type);
  find_m3_rec_field (thread__t, "waitingForMutex", 
		     &thread__t__mutex_size, &thread__t__mutex_offset, 
		     &thread__t__mutex_type);
  find_m3_rec_field (thread__t, "waitingForTime", 
		     &thread__t__time_size, &thread__t__time_offset, 
		     &thread__t__time_type);
  find_m3_rec_field (thread__t, "context",
		     &thread__t__context_size, &thread__t__context_offset,
		     &thread__t__context_type);
  find_m3_rec_field (thread__t__context_type, "buf",
		     &thread__t__buf_size, &thread__t__buf_offset, 0);

  /* skip past the method pointer */
  thread__t__id_offset    += TARGET_PTR_BIT;
  thread__t__state_offset += TARGET_PTR_BIT;
  thread__t__next_offset  += TARGET_PTR_BIT;
  thread__t__cond_offset  += TARGET_PTR_BIT;
  thread__t__mutex_offset += TARGET_PTR_BIT;
  thread__t__time_offset  += TARGET_PTR_BIT;
  thread__t__buf_offset   += TARGET_PTR_BIT + thread__t__context_offset;
}


/*--------------------------------------------------------- jmpbuf layout ---*/

#if defined(mips)
/* see config/mips/tm-mips.h/REGISTER_NAMES */
#define HAVE_REGISTER_MAP
static int regno_to_jmpbuf [] = {
   3,  4,  5,  6,  7,  8,  9, 10,
  11, 12, 13, 14, 15, 16, 17, 18,
  19, 20, 21, 22, 23, 24, 25, 26,
  27, 28, 29, 30, 31, 32, 33, 34,
   3,  3,  3,  3,  3,  2,
  38, 39, 40, 41, 42, 43, 44, 45,
  46, 47, 48, 49, 50, 51, 52, 53,
  54, 55, 56, 57, 58, 59, 60, 61,
  62, 63, 64, 65, 66, 67, 68, 69,
   3,  3,  3,  3,  3,  3,  3,  3,
   3,  3};
#endif

#if defined(__alpha)
/* see config/alpha/tm-alpha.h/REGISTER_NAMES */
#define HAVE_REGISTER_MAP
static int regno_to_jmpbuf [] = {
   4,  5,  6,  7,  8,  9, 10, 11,
  12, 13, 14, 15, 16, 17, 18, 19,
  20, 21, 22, 23, 24, 25, 26, 27,
  28, 29, 30, 31, 32, 33, 34, 35,
  37, 38, 39, 40, 41, 42, 43, 44,
  45, 46, 47, 48, 49, 50, 51, 52,
  53, 54, 55, 56, 57, 58, 59, 60,
  61, 62, 63, 64, 65, 66, 67, 68,
   2, 35};
#endif

/*---------------------------------------------------- thread enumeration ---*/


static void
get_m3_thread (ref, t)
     CORE_ADDR ref;
     M3_THREAD *t;
{
    /* in case we get stuck */
    t->ref   = ref;
    t->id    = 0;
    t->bits  = 0;

    if (!ref) return;

    m3_read_object_fields_bits (ref, thread__t, 0, &(t->bits));
    if (!t->bits) return;

    t->id = m3_unpack_ord (t->bits, thread__t__id_offset,thread__t__id_size,0);
}

static void
first_m3_thread (t)
     M3_THREAD *t;
{
    value_ptr v = eval ("ThreadPosix.self");
    LONGEST ref = m3_unpack_pointer (VALUE_CONTENTS (v), 0);
    get_m3_thread (ref, t);
}

static void
next_m3_thread (t)
     M3_THREAD *t;
{
    LONGEST ref;

    if (!t) return;
    if (!(t->bits)) return;
    ref = m3_unpack_pointer (t->bits, thread__t__next_offset);
    get_m3_thread (ref, t);
}

/*---------------------------------------------------------------- switch ---*/


static void
m3_posix_threads (args, from_tty)
     char *args;
     int from_tty;
{
  int first_id, state;
  M3_THREAD cur;

  init_thread_constants ();

  first_m3_thread (&cur);
  first_id = cur.id;

  fprintf_filtered (stdout, "-id-   -Thread.T-  -state-\n");
  while (cur.bits) {
    fprintf_filtered (stdout, "%4d  16_%lx  ", cur.id, cur.ref);

    state = m3_unpack_ord (cur.bits, thread__t__state_offset,
			   thread__t__state_size, 0);
    switch (state) {
      case 0 /* alive */:
	fprintf_filtered (stdout, "alive");
	break; 
      case 1 /* waiting */:
	fprintf_filtered (stdout, "waiting for condition 16_%x",
		  m3_unpack_pointer (cur.bits, thread__t__cond_offset));
	break;
      case 2 /* locking */:
	fprintf_filtered (stdout, "waiting for mutex 16_%x",
		  m3_unpack_pointer (cur.bits, thread__t__mutex_offset));
	break;
      case 3 /* pausing */:
	fprintf_filtered (stdout, "waiting until ");
	m3_val_print2 (thread__t__time_type, cur.bits, thread__t__time_offset, 
		       thread__t__time_size, stdout, 0, 0);
	break;
      case 4 /* blocking */:
	fprintf_filtered (stdout, "waiting for I/O");
	break;
      case 5 /* dying */:
	fprintf_filtered (stdout, "waiting for somebody to join");
	break;
      case 6 /* dead */:
	fprintf_filtered (stdout, "dead");
	break;
      default:
	fprintf_filtered (stdout, "<unknown state = %d>", state);
	break;
      }
    fputs_filtered ("\n", stdout);

    /* advance to the next thread */
    next_m3_thread (&cur);
    if (cur.id == first_id) break;
  }
}





static int
reg_offset(int regno)
{
#ifdef HAVE_REGISTER_MAP
     return thread__t__buf_offset / HOST_CHAR_BIT
	+ regno_to_jmpbuf [regno] * TARGET_PTR_BIT / HOST_CHAR_BIT;
#else
     return 0;
#endif
}



    


struct m3_os_ops m3_posix;

struct m3_os_ops *
init_target_os_m3_ops_posix()
{
    if (!is_posix_target())
	return 0;

    m3_posix.os_name = "POSIX";
    m3_posix.os_init_thread_constants = init_thread_constants;
    m3_posix.os_threads = m3_posix_threads;
    m3_posix.os_get_m3_thread = get_m3_thread;
    m3_posix.os_next_m3_thread = next_m3_thread;
    m3_posix.os_first_m3_thread = first_m3_thread;
    m3_posix.os_reg_offset = reg_offset;
    m3_posix.os_new_typecode_format = 0;
    return &m3_posix;
}


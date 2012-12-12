/* M3 OS-dependent language support routines for SPIN and GDB, the GNU debugger.
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
 *		Created.
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
static struct type *strand__t = 0;	/* base type */

static int thread__t__tid_size, thread__t__tid_offset;

static int thread__t__state_size, thread__t__state_offset;

static int thread__t__gcnext_size, thread__t__gcnext_offset;

static int thread__t__cond_size, thread__t__cond_offset;

static int thread__t__func_size, thread__t__func_offset;
static int thread__t__arg_size, thread__t__arg_offset;

static int thread__t__wait_event_size, thread__t__wait_event_offset;
static struct type * thread__t__wait_event_type;

static int thread__t__lock_size, thread__t__lock_offset;
static struct type * thread__t__lock_type;



static int
is_spin_target()
{
  return (find_m3_ir ('I', "Strand") != NULL);
}

    
    
    

#define TRY(m,f)	{if (!f) error("%s failed\n", m);}

static void
init_thread_constants() 
{
  int thread__t__context_size, thread__t__context_offset;
  struct type * thread__t__context_type;
  int id_size;
  int id_offset;
  int offset;
  

  if (!is_spin_target()) {
      fprintf_filtered(stderr,"warning -- m3 spin not supported\n");
      return;
  };
  
  if (thread__t) return;

  TRY("Thread",find_m3_ir ('I', "Thread"));
  
  TRY("Strand", find_m3_ir ('I', "Strand"));


  thread__t = find_m3_type_named ("Thread.T");
  strand__t = find_m3_type_named ("Strand.T");  


  /* XX WARNING -- THIS CODE DOES NOT WORK */
  TRY("tid",find_m3_obj_field (strand__t, "tid", 
		     &thread__t__tid_size, &thread__t__tid_offset, 0));

  TRY("state",find_m3_obj_field (thread__t, "state", 
		     &thread__t__state_size, &thread__t__state_offset, 0));
  TRY("gcnext",find_m3_obj_field (thread__t, "gcnext", 
		     &thread__t__gcnext_size, &thread__t__gcnext_offset, 0));

  TRY("func", find_m3_obj_field (thread__t, "func", 
		     &thread__t__func_size, &thread__t__func_offset, 0));

  TRY("arg",find_m3_obj_field (thread__t, "arg", 
		     &thread__t__arg_size, &thread__t__arg_offset, 0));    

  
  TRY("wait_event", find_m3_obj_field (thread__t, "wait_event", 
		     &thread__t__wait_event_size,
	             &thread__t__wait_event_offset,
		     &thread__t__wait_event_type));
  
  TRY("lock", find_m3_obj_field (thread__t, "lock", 
		     &thread__t__lock_size, &thread__t__lock_offset, 
		     &thread__t__lock_type));


  /*
   * Compute the data offset taking into account the supertype relationship
   * between strands and threads. We should have a way to encode this
   * more automatically.
   */
  
  TRY("strand", find_m3_obj_field(strand__t, "id", &id_size, &id_offset, 0));

  offset = ((id_size + id_offset));
  
  thread__t__tid_offset    += TARGET_PTR_BIT;
  thread__t__state_offset += offset + TARGET_PTR_BIT;
  thread__t__gcnext_offset  += offset + TARGET_PTR_BIT;
  thread__t__func_offset += offset + TARGET_PTR_BIT;
  thread__t__arg_offset += offset + TARGET_PTR_BIT;
  thread__t__wait_event_offset  += offset + TARGET_PTR_BIT;
  thread__t__lock_offset += offset + TARGET_PTR_BIT;

  fprintf_filtered(stderr,"thread__t__tid_offset %d\n", thread__t__tid_offset);
  fprintf_filtered(stderr,"thread__t__state_offset %d\n", thread__t__state_offset);
  fprintf_filtered(stderr,"thread__t__gcnext_offset %d\n", thread__t__gcnext_offset);
  fprintf_filtered(stderr,"thread__t__func_offset %d\n", thread__t__func_offset);
  fprintf_filtered(stderr,"thread__t__arg_offset %d\n", thread__t__arg_offset);
  fprintf_filtered(stderr,"thread__t__wait_event_offset %d\n", thread__t__wait_event_offset);
  fprintf_filtered(stderr,"thread__t__lock_offset %d\n", thread__t__lock_offset);

/*thread__t__time_offset  += TARGET_PTR_BIT;
  thread__t__buf_offset   += TARGET_PTR_BIT + thread__t__context_offset;*/

  fprintf_filtered(stdout, "Spin threads\n");
}


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

    t->id = m3_unpack_ord (t->bits, thread__t__tid_offset,thread__t__tid_size,0);
}

static void
first_m3_thread (t)
     M3_THREAD *t;
{
    value_ptr v = eval ("Thread.activeThreads");
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
    ref = m3_unpack_pointer (t->bits, thread__t__gcnext_offset);
    get_m3_thread (ref, t); /*  */
}


char *m3_thread_state[4] = { "nascent", "active", "finished", "exceptionPosted"};

static void
m3_spin_threads(args, from_tty)
     char *args;
     int from_tty;
{
  int first_id, state;
  M3_THREAD cur;

  fprintf_filtered(stdout,"Not working; use info threads\n");
  
  init_thread_constants ();

  first_m3_thread (&cur);
  first_id = cur.id;

  fprintf_filtered (stdout, "-id-   -Thread.T-  -state-\n");
  while (cur.bits) {
    fprintf_filtered (stdout, "%4d  16_%lx  ", cur.id, cur.ref);
    

    state = m3_unpack_ord (cur.bits, thread__t__state_offset,
			   thread__t__state_size, 0);
    if (state < 0 || state > 3)
	fprintf_filtered(stdout,"Bad State: %d", state);
    else
	fprintf_filtered(stdout,m3_thread_state[state]);
    fputs_filtered ("\n", stdout);

    /* advance to the next thread */
    next_m3_thread (&cur);
    if (cur.id == first_id) break;
  }
}

static int
reg_offset(int regno)
{
    return 0;		/* XX */
}

struct m3_os_ops m3_spin;

struct m3_os_ops *
init_target_os_m3_ops_spin()
{
    if (!is_spin_target())  {
	fprintf_filtered(stdout,"Warning. NOT SPIN TARGET\n");
	return 0;
    }

    m3_spin.os_name = "SPIN";
    m3_spin.os_new_typecode_format = 1;   /* new typecode structure */
    m3_spin.os_init_thread_constants = init_thread_constants;
    m3_spin.os_threads = m3_spin_threads;
    m3_spin.os_get_m3_thread = get_m3_thread;
    m3_spin.os_next_m3_thread = next_m3_thread;
    m3_spin.os_first_m3_thread = first_m3_thread;
    m3_spin.os_reg_offset = reg_offset;
    m3_spin.os_new_typecode_format = 1;
    return &m3_spin;
}


    

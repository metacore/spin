/* 
 * Mach Operating System
 * Copyright (c) 1993 Carnegie Mellon University
 * All Rights Reserved.
 * 
 * Permission to use, copy, modify and distribute this software and its
 * documentation is hereby granted, provided that both the copyright
 * notice and this permission notice appear in all copies of the
 * software, derivative works or modified versions, and any portions
 * thereof, and that both notices appear in supporting documentation.
 * 
 * CARNEGIE MELLON ALLOWS FREE USE OF THIS SOFTWARE IN ITS "AS IS"
 * CONDITION.  CARNEGIE MELLON DISCLAIMS ANY LIABILITY OF ANY KIND FOR
 * ANY DAMAGES WHATSOEVER RESULTING FROM THE USE OF THIS SOFTWARE.
 * 
 * Carnegie Mellon requests users of this software to return to
 * 
 *  Software Distribution Coordinator  or  Software.Distribution@CS.CMU.EDU
 *  School of Computer Science
 *  Carnegie Mellon University
 *  Pittsburgh PA 15213-3890
 * 
 * any improvements or extensions that they make and grant Carnegie Mellon
 * the rights to redistribute these changes.
 */
/*
 * Cache structures and definitions for gdb targets.
 * 
 * HISTORY
 * $Log: gdb-cache.h,v $
 * Revision 1.2  1996/08/15 04:33:40  fgray
 * x86 merge.
 *
 * Revision 1.1  1995/07/06  01:05:03  bershad
 * New File
 *
 * Revision 1.1  1995/07/06  00:21:57  bershad
 * *** empty log message ***
 *
 * Revision 2.1.2.1  93/08/06  23:38:02  mrt
 * 	Created.
 * 	[93/08/06  12:34:19  grm]
 * 
 */

#ifndef _GDB_CACHE_H
#define _GDB_CACHE_H

/*
 * Author: Gerald Malan <grm@cs.cmu.edu>
 *	   School of Computer Science
 *	   Carnegie Mellon University
 *	   Pittsburgh, PA, USA
 *
 */

/*
 * -grm
 */

#ifndef	NULL
#define NULL	(0)
#endif	/* NULL */

typedef unsigned long vm_address_t;		/* some sizes we should have */
typedef unsigned long vm_offset_t;
typedef unsigned long vm_size_t;


struct memory_object {
  struct memory_object * prev;
  struct memory_object * next;
  int len;
  unsigned long start;
  unsigned long end;
  vm_address_t  bytes;
};

typedef struct memory_object * mem_object_t;

struct memory_cache {
  struct memory_object * head;
  struct memory_object * tail;
  int valid;
  int enabled;
  int max_granularity;
  int granularity;
  char * name;
};

#define DEFAULT_GRANULARITY	256

struct cache_list_entry {
  struct cache_list_entry * prev;
  struct cache_list_entry * next;
  struct memory_cache * cache;
};

#define INVALID_MEMORY_OBJECT	((struct memory_object *)-1)
#define INVALID_CACHE		((struct memory_cache *)-1)

typedef struct cache_list_entry * cache_list_entry_t;

/* Prototypes for cache functions */
struct memory_cache *
gdb_cache_create PARAMS ((char *, int));

void
gdb_cache_destroy PARAMS ((struct memory_cache *));

void
gdb_cache_clean PARAMS ((struct memory_cache *));

void
gdb_cache_write PARAMS ((struct memory_cache *, CORE_ADDR, vm_address_t, 
                         vm_size_t));

int
gdb_cache_read PARAMS ((struct memory_cache *, CORE_ADDR, vm_address_t, 
                        vm_size_t, char **, int *));

void
gdb_cache_invalidate PARAMS ((struct memory_cache *, CORE_ADDR, vm_size_t));

#define gdb_cache_supply(c,cp,myaddr,len)	bcopy(cp, myaddr, len)

#endif
